#' @title Correct Landsat L1C products
#'
#' @description The function corrects Landsat 1-8 L1C images to L2A.
#'     For the digital number (DN) to top of atmosphere (TOA) reflectance I used the metadata
#'     from the \code{MTL.txt} with the following equation:\cr
#'     TOA_RAD = rawDN * RADIANCE_MULT_BAND + RADIANCE_ADD_BAND /sin(SUN_ELEVATION * 180)\cr
#'     \cr
#'     At this point, only the following bands will be saved:\cr
#'     Red, Green, Blue, NIR, SWIR1, SWIR2
#'
#' @param l1c_path Full/relative path of L1C product. This can be both the original .tar.gz file or the unzipped file.
#' @param out_dir Directory where output L2A products will be placed.
#' @param proc_dir (optional) Directory where processing is applied. If NULL (default),
#'     processing is done in \code{l1c_path}-directory and output L2A product is then moved to \code{out_dir}.
#' @param sf_mask \code{sf} object; CRS from this sf object will be used to project all raster layers.
#'     This process is computationally expensive, therefore parallel computation is applied. The raster will also be cropped and
#'     masked to the bounding box of the \code{sf_mask} object.
#' @param bad_pixel Remove pixels with values of <0.
#' @param dark_pixel The Dark Object Subtraction method assumes that the darkest parts of an image
#'     (water, artificial structures) should be black, if not for the effects of atmospheric scatter.
#'     The lowest value of each band will therefore be subtracted.
#' @param indices (optional) Character string or vector of indices to calculate.\cr
#'     \code{MNDWI}: Modified Normalized Difference Water Index: \href{https://doi.org/10.1080/01431160600589179}{Xu H.}\cr
#'     \code{NDVI}: Normalized Difference Vegetation Index:
#'     \href{https://gisgeography.com/ndvi-normalized-difference-vegetation-index/}{GIS Geography}
#' @param cores The number of cores to use; only necessary if \code{sf_mask} is provided.
#' @param maxmemory numeric; Maximum number of bytes to read into memory. If a process is expected to require more
#'     than this value, \code{\link[raster]{canProcessInMemory}} will return \code{FALSE.}
#'
#' @return L2A.grd file in \code{out_dir.}
#' @export
#'
#' @import raster
#' @importFrom dplyr if_else
LS_L1C <- function(l1c_path = NULL, out_dir = NULL, proc_dir = NULL, sf_mask = NA,
                   bad_pixel = TRUE, dark_pixel = TRUE,
                   indices = c("MNDWI", "NDVI"),
                   cores = 1L, maxmemory = 1e+08) {

  l1c_name <- basename(l1c_path)
  l1c_name <- dplyr::if_else(endsWith(l1c_name, ".tar.gz"),
                             gsub(".tar.gz", "", l1c_name), l1c_name)

  # l1c_path
  if (is.null(l1c_path)) {
    stop("l1c_path is NULL.")
  } else if (endsWith(basename(l1c_path), ".tar.gz")) {
    if (file.exists(l1c_path)) {
      stop("l1c_path is no valid file path to a .tar.gz file.")
    } else {
      stop("l1c_path is no valid directory.")
    }
  }

  # out_dir
  if (is.null(out_dir)) stop("out_dir is NULL.")

  # proc_dir
  rm_proc_dir <- FALSE
  if(is.null(proc_dir)) {
    if (endsWith(basename(l1c_path), ".tar.gz")) {
      proc_dir <- file.path(dirname(l1c_path), "proc_dir")
    } else {
      proc_dir <- file.path(dirname(l1c_path), l1c_name, "proc_dir")
    }
    rm_proc_dir <- TRUE
  }
  dir.create(file.path(proc_dir, l1c_name), recursive = T, showWarnings = F)

  # cores
  if (cores < 1L) stop("Number of cores must be 1 or greater.")

  # Raster options
  raster::rasterOptions(tmpdir = file.path(proc_dir, l1c_name), datatype = 'FLT4S', overwrite = T)
  raster::rasterOptions(maxmemory = maxmemory)


  # LANDSAT Bands ---------------------------------------------------
  LS_MSS <- data.frame(matrix(c(1:4), nrow = 1, ncol = 4))
  names(LS_MSS) <- c("Green", "Red", "NIR", "NIR2")

  LS_4 <- data.frame(matrix(c(1:7), nrow = 1, ncol = 7))
  names(LS_4) <- c("Blue", "Green", "Red", "NIR", "SWIR1", "Thermal", "SWIR2")

  LS_5 <- LS_4

  LS_7 <- data.frame(matrix(c(1:8), nrow = 1, ncol = 8))
  names(LS_7) <- c("Blue", "Green", "Red", "NIR", "SWIR1", "Thermal", "SWIR2", "Pan")

  LS_8 <- data.frame(matrix(c(1:11), nrow = 1, ncol = 11))
  names(LS_8) <- c("Coastal", "Blue", "Green", "Red", "NIR", "SWIR1", "SWIR2", "Pan", "Cirrus", "TIRS1", "TIRS2")

  # Untar ----------------------------------------------------------
  dir.create(file.path(proc_dir, l1c_name, "L1C"), recursive = T, showWarnings = F)

  if (endsWith(l1c_path, ".tar.gz")) {
    untar(l1c_path, exdir = file.path(proc_dir, l1c_name, "L1C"))
  } else {
    file.copy(from = dir(l1c_path, full.names = TRUE),
              to = file.path(proc_dir, l1c_name, "L1C"))
  }

  # Read in Landsat Szene ------------------------------------------
  # Get sensor from this_szene file name
  sensor <- strsplit(l1c_name, "_")[[1]][1]

  # Get Bands Blue, Green, Red, NIR, SWIR1, SWIR2
  tif_vector <- dir(file.path(proc_dir, l1c_name, "L1C"), pattern = ".TIF$", recursive = T, full.names = T)
  band_list = switch (
    sensor,
    "LC08" = tif_vector[4:9],
    "LE07" = tif_vector[c(1:5, 8)],
    "LT05" = tif_vector[c(1:5, 7)],
    "LT04" = tif_vector[c(1:5, 7)],
    "LM05" = tif_vector[1:4],
    "LM02" = tif_vector[1:4]
  )

  # Metadata from _MTL.txt
  meta_dat <- dir(file.path(proc_dir, l1c_name, "L1C"), pattern = "_MTL.txt$", full.names = T)
  meta_matrix <- read.csv(meta_dat, sep = "=", stringsAsFactors=F) %>%
    as.matrix()

  # sf_mask --------------------------------------------------------
  if(is(sf_mask, "sf")) {
    if (nrow(sf_mask) == 0) {
      stop("x musst contain at least one feature")
    }
    proc_rast <-  function(x, shp, cores) {
      raster::beginCluster(cores)

      tmp_raster <- raster::raster(x) %>%
        raster::projectRaster(crs = rgdal::make_EPSG() %>%
                                dplyr::filter(code == sf::st_crs(shp)$epsg) %>%
                                dplyr::pull(prj4)) %>%
        raster::crop(raster::extent(shp)) %>%
        raster::mask(sf::st_as_sf(sf::st_as_sfc(sf::st_bbox(shp))))

      raster::endCluster()

      return(tmp_raster)
    }

    cat("Project raster"); cat("\n")
    LS_stack <- lapply(band_list, proc_rast, shp = sf_mask, cores = cores) %>%
      raster::stack()
  } else {
    LS_stack <- raster::stack(band_list)
  }

  if (sensor == "LM02" || sensor == "LM05") {
    names(LS_stack) <- c("Green", "Red", "NIR", "NIR2")
  } else {
    names(LS_stack) <- c("Blue", "Green", "Red", "NIR", "SWIR1", "SWIR2")
  }

  # Preprocessing -------------------------------------------------
  # 1. Eliminate bad pixel
  if (bad_pixel) {
    for (band in names(LS_stack)) {
      LS_stack[[band]] <- raster::reclassify(LS_stack[[band]], rcl=c(-Inf,0,NA), right=F)
      Sys.sleep(0.1)
    }
  }


  # 2. DN to TOA Reflectance - RAD = rawDN * RADIANCE_MULT_BAND + RADIANCE_ADD_BAND /sin(SEA)
  cat("DN to TOA Reflectance"); cat("\n")
  sensor_bands = switch (
    sensor,
    "LC08" = LS_8,
    "LE07" = LS_7,
    "LT05" = LS_5,
    "LT04" = LS_4,
    "LM02" = LS_MSS,
    "LM05" = LS_MSS
  )
  tmp_stack <- raster::stack()
  for (band in names(LS_stack)) {
    ref_add_line <- grep(pattern = paste0('REFLECTANCE_ADD_BAND_', sensor_bands[[band]], sep = ""), meta_matrix)
    ref_add <- as.numeric(meta_matrix[ref_add_line, 2])
    ref_multi_line <- grep(pattern = paste0('REFLECTANCE_MULT_BAND_', sensor_bands[[band]], sep = ""), meta_matrix)
    ref_multi <- as.numeric(meta_matrix[ref_multi_line, 2])
    sea_line <- grep(pattern = "SUN_ELEVATION", meta_matrix)
    sea <- as.numeric(meta_matrix[sea_line, 2])
    TOA_raster <- (LS_stack[[band]] * ref_multi + ref_add) / sin(sea * pi/180)
    tmp_stack <- raster::stack(tmp_stack, TOA_raster)
    saz_line <- grep(pattern = "SUN_AZIMUTH", meta_matrix)
    saz <- as.numeric(meta_matrix[saz_line, 2])
    Sys.sleep(0.1)
  }

  LS_stack <- tmp_stack

  # 3. Subtract dark pixel (subtract min value from each band respectively)
  if (dark_pixel) {
    for (band in names(LS_stack)) {
      LS_stack[[band]] <- LS_stack[[band]] - raster::minValue(LS_stack[[band]])
      invisible(gc())
      Sys.sleep(0.1)
    }
  }

  # Apply functions -----------------------------------------------
  if ("MNDWI" %in% indices) {
    # Calculate NDWI and remove false values
    calcMNDWI <- function(Green, NIR) return((Green - NIR) / (Green + NIR))

    NDWI_layer <- raster::overlay(LS_stack$Green, LS_stack$NIR, fun = calcMNDWI) %>%
      raster::reclassify(rcl = c(-Inf, -1, NA), right = F) %>%
      raster::reclassify(rcl = c(1, Inf, NA), right = T)

    names(NDWI_layer) <- "NDWI"
    LS_stack <- raster::stack(LS_stack, NDWI_layer)
  }

  if ("NDVI" %in% indices) {
    # Calculate NDVI and remove false values
    calcNDVI <- function(NIR, VIS) return((NIR - VIS) / (NIR + VIS))

    NDVI_layer <- raster::overlay(LS_stack$NIR, LS_stack$Red, fun = calcNDVI) %>%
      raster::reclassify(rcl = c(-Inf, -1, NA), right = F) %>%
      raster::reclassify(rcl = c(1, Inf, NA), right = T)

    names(NDVI_layer) <- "NDVI"
    LS_stack <- raster::stack(LS_stack, NDVI_layer)
  }

  # Write raster -------------------------------------------------
  if (!dir.exists(out_dir)) {
    message("out_dir will be created.")
    dir.create(out_dir, recursive = TRUE)
  }
  raster::writeRaster(LS_stack, file.path(out_dir, paste0(l1c_name, ".grd")), format = 'raster', overwrite = T)

  # Remove files from Rtmp
  if (rm_proc_dir) {
    unlink(proc_dir, recursive = TRUE)
  } else {
    unlink(file.path(proc_dir, l1c_name), recursive = TRUE)
  }

  invisible(gc())

  return(LS_stack)
}
