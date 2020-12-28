#' @title Distance-weighting of Landsat variable
#' @description To account for the diminishing effect of spatial variables (e.g. NDVI exposure) as
#'     distance increases, we fitted a logit function to weight each incremental isochrone.
#'
#' @param isochrones object of class \code{sf} containing road features,
#'     derived from the \code{\link[DRIGLUCoSE]{isodistances}} function.
#' @param tag character or NA; string containing the column name, that indicates the unique tag column.
#' @param time character; string containing the column name, that indicates the time column.
#' @param landsat_list list of landsat products, derived from the \code{\link[DRIGLUCoSE]{LS_L1C}} function. See Details.
#' @param band character; spectral band or index from the landsat object,
#'     to which distance-weighting should be applied.
#' @param b numeric; slope of distance decay function
#' @param m numeric; x for g(x) = 0.5
#' @param cores the number of cores to use.
#' @param stats The function to be applied. See Details
#'
#' @details
#'     All unique tags from the isochrones shapefile will be overlapped with the \code{landsat_list},
#'     and only the first overlapping landsat image will be used. This allows you to analyze multiple
#'     areas at once.\cr
#'     \cr
#'     Supported functions for the \code{stat} object:\cr
#'     "sum", "mean", "min", "max", "sd", "rms", "skew", "median" and "percentile". When using "percentile",
#'     provide a \code{list} as follows:\cr
#'     \code{list("percentile", upper_lim)}, where \code{upper_lim} is the upper limit of the percentile (e.g. 0.95).
#'
#' @return A \code{tibble} containing the spatially weighted statistics of the
#'     intersects of the isochrone and Landsat objects.
#' @export
#'
#' @import raster
#' @import sf
#' @import dplyr
#'
#' @importFrom mosaicCore makeFun
#' @importFrom mosaicCalc antiD
#' @importFrom stats quantile
#' @importFrom stats median
#' @importFrom parallel makeCluster
#' @importFrom parallel parLapply
#' @importFrom parallel stopCluster
#' @importFrom parallel mclapply
LS_band_weighting <- function(isochrones, tag = "tag", time = "time",
                              landsat_list, band = "NDVI",
                              b = 8, m = 0.5, cores,
                              stats = list("sd", "median",
                                           list("percentile", 0.05),
                                           list("percentile", 0.95),
                                           "skew")){
  # 1. Check input -------------------------------------------------------
  # landsat_list
  if (is.list(landsat_list)) {
    landsat_list_class <- sapply(landsat_list, class) %>% unique()
    if (!(landsat_list_class == "RasterStack" || landsat_list_class == "RasterBrick")) {
      stop("landsat_list must be a list of RasterStack or RasterBrick objects.")
    }
  } else {
    stop("landsat_list must be a list.")
  }

  # band
  if (as.numeric(table(sapply(landsat_list, names))[band]) != length(landsat_list)) {
    stop("band must be a landsat band in all raster elements of landsat_list.")
  }

  # isochrones
  if (!is(isochrones, "sf")) {
    stop("isochrones must be sf object.")
  } else if (nrow(isochrones) == 0) {
    stop("isochrones musst contain at least one feature.")
  } else {
    # Check if geometry column only contains POINT features
    sf_class <- isochrones %>%
      dplyr::pull(geom) %>%
      sf::st_geometry_type() %>%
      as.character() %>%
      unique()

    if((length(sf_class) > 1) ||
       !((sf_class == "MULTIPOLYGON" || sf_class == "POLYGON"))) {
      stop("isochrones must only contain either MULTIPOLYGON or POLYGON features.")
    }
  }

  # tag
  if (is.na(tag)) {
    message("No tag is specified. It is strongly recommend to specify a tag to join the input with the output.")

    correct_input = TRUE
    while (correct_input) {
      message("Do you want to continue? (y/n)")
      input <- readline(prompt="")
      if (input == "n") {
        stop("Process has been stoped.")
      } else if (input == "y") {
        correct_input = FALSE
      } else {
        message("Incorrect input.")
      }
    }
  } else if (!tag %in% names(isochrones)) {
    stop("Tag must be a column of the sf object.")
  }

  # time
  if (!time %in% names(isochrones)) {
    stop("time must be a column of the sf object.")
  } else if (is.factor(isochrones[[time]])) {
    isochrones[[time]] <- as.numeric(levels(isochrones[[time]]))[isochrones[[time]]]
  }

  # cores
  if (cores < 1L) stop("Number of cores must be 1 or greater.")

  # stats
  stats_boolean <- lapply(stats, function(i) {
    if (length(i) == 2) {
      if (i[[1]] == "percentile" && is.numeric(i[[2]])) TRUE
      else FALSE
    } else if (i %in% c("sum", "mean", "min", "max",
                        "sd", "rms", "skew", "median")) TRUE
    else FALSE
  }) %>% unlist()

  if (any(!stats_boolean)) stop("Not all entries of stat are supported.")

  # 2. Internal LS_band_weighting function ------------------------------
  this_LS_band_weighting <- function(isochrones, tag, time,
                                     landsat_list, band,
                                     b, m, cores,
                                     stats) {

    #### 1. Rings ####
    # Select this_tag from isochrones shapefile
    this_tag <- isochrones %>%
      dplyr::arrange(time)

    # Create empty sf for rings output
    isoch_rings <- isochrones[0,] %>% dplyr::select(tag, time)

    for(i in 1:nrow(this_tag)) {
      if (i == 1) {
        isoch_rings[1,] <- this_tag[i,] %>% dplyr::select(tag, time) # first isochrone
      } else {
        # create subsequent rings and add subsequent to sf
        isoch_rings <- sf::st_difference(this_tag[i,], this_tag[i-1,]) %>%
          dplyr::select(tag, time) %>%
          dplyr::add_row(isoch_rings, .)
      }
    }
    # Calculate ring areas
    isoch_rings <- isoch_rings %>%
      dplyr::mutate(area = sf::st_area(.)) %>%
      dplyr::relocate(geom, .after = dplyr::last_col())

    #### Distance weights ####
    # Assign distance weights to isochrones
    this_tag <- this_tag %>%
      mutate(area = st_area(.)) %>%  # calulate area of isochrones
      mutate(r = sqrt(area/pi)) %>%
      select(-area) %>%
      mutate(r_norm = as.numeric(r/max(r))) %>% # normalized mean radii
      relocate(geom, .after = last_col())

    # Define spatial weight function
    g <- mosaicCore::makeFun(1 / (1 + exp(b * (x - m))) ~ c(x, b, m))

    # Define integral
    G <- mosaicCalc::antiD(g(x, b = b, m = m)~x)

    # calculate weights:
    # For individual weights the above defined integral is calculated within the limits of the
    # inner and outer radius of the normalized radii of each isocrone,
    # and again normalized by the integral from 0 to the outermost isochrone boundary

    # Empty distance weight matrix for outputs
    dist_weights <- matrix(nrow = nrow(this_tag), ncol = 1)

    for (i in 1:nrow(this_tag)) {
      if (i == 1) {
        # Weights for 5 min isochrone
        dist_weights[i,] <- G(this_tag$r_norm[i]) / G(1)
      } else {
        # Weights for outer isochrones
        dist_weights[i,] <- (G(this_tag$r_norm[i]) - G(this_tag$r_norm[i-1])) / G(1)
      }
    }
    # Convert to vector
    dist_weights <- dist_weights %>% t() %>% as.vector()

    #### Landsat ####
    # Get isochrones that match with current tag-ID and select only the one with the highest range
    max_isochrones <- isochrones %>%
      dplyr::filter(time == max(time))

    # Check which raster overlaps with max_isochrones
    landsat_overlap <- lapply(landsat_list, function(i) {
      crop_error <- try(raster::crop(i, raster::extent(max_isochrones)), silent = T)

      ifelse(class(crop_error) != "try-error", TRUE, FALSE)
    }) %>% unlist()

    if (any(landsat_overlap)) {
      landsat_overlap <- landsat_list[[dplyr::first(which(landsat_overlap == TRUE))]]
    } else {
      stop("landsat_list must completely overlap with isochrones.")
    }

    # Temporary output DataFrame for the Landsat-band statisticss
    raster_stats <- as.data.frame(matrix(nrow = 0, ncol = length(stats)))

    # For every time / level-of-distance, repeat the buffer analysis and save to output DataFrame
    for (this_time in isoch_rings[[time]]) {
      this_isoch <- isoch_rings %>%
        dplyr::filter(time == this_time)

      # Select only band of interest, crop and mask with buffered of this_isoch
      landsat_mask <- landsat_overlap[[band]] %>%
        raster::crop(raster::extent(this_isoch)) %>%
        raster::mask(as(this_isoch, "Spatial"))


      # Caculate statistics
      raster_stats <- lapply(stats, function(i) {
        if (length(i) == 2) {
          if (i[[1]] == "percentile") {
            stats::quantile(raster::values(landsat_mask), i[[2]], na.rm = T) %>%
              as.numeric()
          }
        } else if (i %in% c("sum", "mean", "min", "max", "sd", "rms", "skew")) {
          raster::cellStats(landsat_mask, i)
        } else if (i == "median") {
          stats::median(raster::values(landsat_mask), na.rm = TRUE)
        }
      }) %>%
        unlist() %>%
        t() %>%
        rbind(raster_stats, .)
    }

    #### Band-statstics * weigths ####
    # Multiply areal weights with distance weights
    ring_values <- (raster_stats * dist_weights) %>%
      colSums()

    if (!is.na(tag)) {
      output <- c(isochrones[[tag]] %>% unique(), ring_values)
      names(output) <- c(
        "tag",
        sapply(stats, function(i) {
          if (length(i) == 2) {
            if (i[[1]] == "percentile") {
              paste0("X", i[[2]]*100, "_percentile")
            }
          } else i
        })
      )
    } else {
      output <- ring_values
      names(output) <- c(
        sapply(stats, function(i) {
          if (length(i) == 2) {
            if (i[[1]] == "percentile") {
              paste0("X", i[[2]]*100, "_percentile")
            }
          } else i
        })
      )
    }

    return(output)
  }

  # 3. Parallel apply function ---------------------------------------------

  # Convert isochrones to list to enable mclapply
  isochrones_list <- isochrones %>%
    dplyr::group_by(tag) %>%
    dplyr::group_split()

  # WINDWOS
  if (Sys.info()[["sysname"]] == "Windows") {
    # Use mclapply for paralleling the isodistance function
    cl <- parallel::makeCluster(cores)
    LS_band_weightes <- parallel::parLapply(cl, isochrones_list, fun = this_LS_band_weighting,
                                            tag = tag, time = time,
                                            landsat_list = landsat_list, band = band,
                                            b = b, m = m, cores = cores,
                                            stats = stats)
    parallel::stopCluster(cl)
  }
  # Linux and macOS
  else {
    # Use mclapply for paralleling the isodistance function
    LS_band_weightes <- parallel::mclapply(isochrones_list, this_LS_band_weighting,
                                           tag = tag, time = time,
                                           landsat_list = landsat_list, band = band,
                                           b = b, m = m, cores = cores,
                                           stats = stats,
                                           mc.cores = cores, mc.preschedule = FALSE)
  }

  # Convert list to one tibble
  output_tibble <- DRIGLUCoSE::rbind.parallel(LS_band_weightes, cores = cores) %>%
    as_tibble()

  invisible(gc())

  return(output_tibble)
}
