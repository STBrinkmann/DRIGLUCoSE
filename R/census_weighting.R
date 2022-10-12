#' @title Distance-weighting of spatial census variable
#' @description To account for the diminishing effect of spatial variables (e.g. SES exposure) as
#'     distance increases, we fitted a logit function to weight each incremental isochrone. From
#'     a census shapefile, the spatially weighted areal mean of the isochrones are calculated.
#'
#' @param isochrones object of class \code{sf} containing road features,
#'     derived from the \code{\link[DRIGLUCoSE]{isodistances}} function.
#' @param tag character or NA; string containing the column name, that indicates the unique tag column.
#' @param time character; string containing the column name, that indicates the time column.
#' @param census_sf object of class \code{sf} containing a census shapefile.
#' @param b numeric; slope of distance decay function
#' @param m numeric; x for g(x) = 0.5
#' @param cores the number of cores to use.
#'
#' @details
#'     For a detailed explanation of this method, see documentation on GitHub.
#'
#' @return A \code{tibble} containing the spatially weighted census variable(s) of the
#'     intersects of the isochrone and Landsat objects.
#' @export
#'
#' @import sf
#' @import dplyr
#' @import mosaic
#'
#' @importFrom rlang parse_quo
#' @importFrom mosaicCore makeFun
#' @importFrom mosaicCalc antiD
#' @importFrom parallel makeCluster
#' @importFrom parallel parLapply
#' @importFrom parallel stopCluster
#' @importFrom parallel mclapply
census_weighting <- function(isochrones, tag = "tag", time = "time",
                             census, b = 8, m = 0.5, cores = 1){
  # 1. Check input -------------------------------------------------------
  # isochrones
  if (!is(isochrones, "isochrone")) {
    stop("isochrones must be isochrone object.")
  } else if (nrow(isochrones) == 0) {
    stop("isochrones musst contain at least one feature.")
  } else {

    class(isochrones) <- class(isochrones)[class(isochrones) != "isochrone"]

    # Check if geometry column only contains MULTIPOLYGON or POLYGON features
    sf_class <- isochrones %>%
      dplyr::pull(geom) %>%
      sf::st_geometry_type() %>%
      as.character() %>%
      unique()

    if(!(sf_class == "MULTIPOLYGON" || sf_class == "POLYGON")) {
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
  }
  if (is.character(isochrones[[time]])) {
    isochrones[[time]] <- as.numeric(isochrones[[time]])
  } else if (is.factor(isochrones[[time]])) {
    isochrones[[time]] <- as.numeric(levels(isochrones[[time]]))[isochrones[[time]]]
  }

  # census
  if (!is(census, "sf")) {
    stop("census must be sf object.")
  } else if (nrow(census) == 0) {
    stop("census musst contain at least one feature.")
  } else {

    # Get geometry column name
    geometry_col_name <- lapply(census, is, "sfc") %>%
      unlist() %>%
      which() %>%
      names()

    if (length(geometry_col_name) > 1) {
      warning("x contains more than one geometry column. Only the first one will be used.")
      geometry_col_name <- dplyr::first(geometry_col_name)
    }

    if (geometry_col_name != "geom") {
      census <- rename(census, geom = geometry_col_name)
    }

    # Check if geometry column only contains MULTIPOLYGON or POLYGON features
    sf_class <- census %>%
      dplyr::pull(geom) %>%
      sf::st_geometry_type() %>%
      as.character() %>%
      unique()

    if(!(sf_class == "MULTIPOLYGON" || sf_class == "POLYGON")) {
      stop("isochrones must only contain either MULTIPOLYGON or POLYGON features.")
    }
  }

  if (any(!sapply(census %>% dplyr::as_tibble() %>% dplyr::select(-geom), is.numeric))) {
    stop("census must contain only numeric features.")
  }

  # cores
  #if (cores < 1L) stop("Number of cores must be 1 or greater.")

  # 2. Internal census_weighting function ------------------------------
  this_census_weighting <- function(.isochrones, .tag, .time,
                                    .census, .b, .m) {

    #### 1. Rings ####
    # Select this_tag from .isochrones shapefile
    this_tag <- .isochrones %>%
      dplyr::arrange(.time)

    sf::st_agr(this_tag) = "constant"

    # Create empty sf for rings output
    isoch_rings <- .isochrones[0,] %>% dplyr::select(!! rlang::parse_quo(.tag, env = rlang::global_env()),
                                                     !! rlang::parse_quo(.time, env = rlang::global_env()))

    for(i in 1:nrow(this_tag)) {
      if (i == 1) {
        isoch_rings[1,] <- this_tag[i,] %>% dplyr::select(!! rlang::parse_quo(.tag, env = rlang::global_env()),
                                                          !! rlang::parse_quo(.time, env = rlang::global_env())) # first isochrone
      } else {
        # create subsequent rings and add subsequent to sf
        isoch_rings <- sf::st_difference(this_tag[i,], this_tag[i-1,]) %>%
          dplyr::select(!! rlang::parse_quo(.tag, env = rlang::global_env()),
                        !! rlang::parse_quo(.time, env = rlang::global_env())) %>%
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
      dplyr::mutate(area = sf::st_area(.)) %>%  # calulate area of isochrones
      dplyr::mutate(r = sqrt(area/pi)) %>%
      dplyr::select(-area) %>%
      dplyr::mutate(r_norm = as.numeric(r/max(r))) %>% # normalized mean radii
      dplyr::relocate(geom, .after = last_col())

    # Define spatial weight function
    g <- mosaicCore::makeFun(1 / (1 + exp(b * (x - m))) ~ c(x, b = .b, m = .m))

    # Define integral
    G <- mosaicCalc::antiD(g(x, b, m)~x)

    # calculate weights:
    # For individual weights the above defined integral is calculated within the limits of the
    # inner and outer radius of the normalized radii of each isocrone,
    # and again normalized by the integral from 0 to the outermost isochrone boundary

    # Empty distance weight matrix for outputs
    dist_weights <- matrix(nrow = nrow(this_tag), ncol = 1)

    for (i in 1:nrow(this_tag)) {
      if (i == 1) {
        # Weights for 5 min isochrone
        dist_weights[i,] <- G(this_tag$r_norm[i], b=.b, m=.m) / G(1, b=.b, m=.m)
      } else {
        # Weights for outer isochrones
        dist_weights[i,] <- (G(this_tag$r_norm[i], b=.b, m=.m) - G(this_tag$r_norm[i-1], b=.b, m=.m)) / G(1, b=.b, m=.m)
      }
    }
    # Convert to vector
    dist_weights <- dist_weights %>% t() %>% as.vector()


    #### Areal weights ####
    # Extract variable names of cda (name of the census variables)
    var_names <- names(census)
    var_names <- var_names[!var_names %in% "geom"] # remove "geometry" from names vector

    sf::st_agr(.census) = "constant"
    sf::st_agr(isoch_rings) = "constant"

    # Empty matrix for areal weighted values
    areal_weights <- matrix(nrow = nrow(this_tag), ncol = length(var_names))

    for(x in 1:nrow(isoch_rings)){
      # Intersect CDA and isochrone rings and calculate area and proportions of individual CDA of total area
      cda_int <- sf::st_intersection(.census, sf::st_buffer(isoch_rings[x,], 0.01)) %>%
        na.omit() %>%
        dplyr::mutate(area_cd = sf::st_area(.),
                      weight = as.numeric(area_cd/area))

      for(y in 1:length(var_names)){
        # Multiply variable values with areal proportion
        areal_weights[x,y] <- sum(cda_int[[var_names[y]]] * cda_int$weight)
      }
    }



    #### Census * weigths ####
    # Multiply areal weights with distance weights
    ring_values <- (areal_weights * dist_weights) %>%
      colSums(na.rm = T)

    if (!is.na(.tag)) {
      output <- c(.isochrones[[.tag]] %>% unique(), ring_values)
      names(output) <- c(.tag, var_names)
    } else {
      output <- ring_values
      names(output) <- var_names
    }

    return(output)
  }


  # 3. Parallel apply function ---------------------------------------------

  # Convert isochrones to list to enable mclapply
  isochrones_list <- isochrones %>%
    dplyr::group_by(!! rlang::parse_quo(tag, env = rlang::global_env())) %>%
    dplyr::group_split()

  if (cores > 1) {
    # WINDWOS
    if (Sys.info()[["sysname"]] == "Windows") {
      # Use mclapply for paralleling the isodistance function
      cl <- parallel::makeCluster(cores)
      parallel::clusterExport(cl, c("b", "m"), envir = environment())
      census_weightes <- parallel::parLapply(cl, isochrones_list, fun = this_census_weighting,
                                              .tag = tag, .time = time,
                                              .census = census, .b = b, .m = m)
      parallel::stopCluster(cl)
    }
    # Linux and macOS
    else {
      # Use mclapply for paralleling the isodistance function
      census_weightes <- parallel::mclapply(isochrones_list, this_census_weighting,
                                            .tag = tag, .time = time,
                                            .census = census, .b = b, .m = m,
                                            mc.cores = cores, mc.preschedule = FALSE)
    }
  } else {
    census_weightes <- lapply(isochrones_list, FUN = this_census_weighting,
                              .tag = tag, .time = time,
                              .census = census, .b = b, .m = m)
    }

  # Convert list to one tibble
  output_tibble <- DRIGLUCoSE::rbind_parallel(census_weightes, cores = cores) %>%
    as_tibble()

  invisible(gc())

  return(output_tibble)
}
