#' @title Download and clean road features
#'
#' @description Download road features using the OSM overpass API \code{\link[osmdata]{opq}}.
#'     Simple topology cleaning is applied using \code{\link[nngeo]{st_segments}}.
#'
#' @param x object of class \code{sf}.
#' @param dist numeric; maximum number of minutes of the isochrones.
#' @param speed numeric or character; either numeric value of speed or string containing the column name, that indicates the walking speed.
#' @param cores the number of cores to use.
#' @param remove_features character vector containing feature keys that should be excluded from the analysis (e.g. motorway)
#' @param split_segments logical; should topology cleaning with \code{\link[nngeo]{st_segments}} be used.
#'
#' @return object of class \code{sf} containing road features.
#' @export
#'
#' @import sf
#' @importFrom osmdata opq
#' @importFrom osmdata add_osm_feature
#' @importFrom osmdata osmdata_sf
#' @importFrom osmdata osm_poly2line
#' @importFrom dplyr select
#' @importFrom dplyr rename
#' @importFrom dplyr filter
#' @importFrom nngeo st_segments
#' @importFrom methods is
#'
#' @examples
#' \dontrun{
#' data(Erlangen)
#' osm_roads(Erlangen, dist = 20, speed = "Speed", cores = 4)
#' }
osm_roads <- function(x, dist, speed, cores = 1L,
                      remove_features = c("motorway", "motorway_link", "trunk", "trunk_link", "raceway"),
                      split_segments = TRUE) {

  # 1. Check input -------------------------------------------------------
  # x
  if (!is(x, "sf")) {
    stop("x must be sf object")
  } else if (nrow(x) == 0) {
    stop("x musst contain at least one feature")
  }

  # dist
  if (dist <= 0) stop("dist must be greater than 0.")

  # speed
  if (is.character(speed)) {
    if (speed %in% names(x)) {
      speed = as.numeric(x[[speed]])
      message("speed will be used from sf object")
    } else(
      stop("speed must be either numeric or a column of the sf object")
    )
  }

  # cores
  if (cores < 1L) stop("Number of cores must be 1 or greater.")

  # Check if location CRS is cartesian. This is required for the buffer
  is_cartesian <- x %>%
    sf::st_crs() %>%
    .[[2]] %>%
    grepl("CS[Cartesian,", ., fixed = T)

  if (!is_cartesian) stop("A cartesian CRS is required for applying buffers")
  rm(is_cartesian)


  # 2. Download OSM data -------------------------------------------------------
  x_bbox <- x %>%
    sf::st_buffer(dist * speed) %>%
    sf::st_transform(4326) %>%
    sf::st_union() %>%
    sf::st_bbox() %>%
    as.vector()

  error_count <- 1
  while (error_count <= 5) {
    .error <- try(
      capture.output(
        osm_roads <- osmdata::opq(bbox = x_bbox) %>%
          osmdata::add_osm_feature(key = 'highway') %>%
          osmdata::osmdata_sf() %>%
          osmdata::osm_poly2line()
      )
    )

    if(class(.error) != "try-error") break

    error_count <- error_count + 1
    invisible(gc()); Sys.sleep(c(1, 5, 10, 10, 15, 30)[error_count])
  }

  if (class(.error) == "try-error") stop("Runtime error.")
  if (is.na(osm_roads)) stop("No features downloaded from www.openstreetmap.org.")

  osm_roads <- osm_roads$osm_lines %>%
    dplyr::select(highway) %>%
    dplyr::filter(!highway %in% remove_features) %>%
    dplyr::rename(geom = geometry) %>%
    st_transform(crs = sf::st_crs(x))

  # 3. Topology cleaning -------------------------------------------------------
  if (split_segments) {
    if (cores > 1) {
      # ---- WINDWOS ----
      if (Sys.info()[["sysname"]] == "Windows") {

        cl <- parallel::makeCluster(cores)
        osm_roads <- suppressWarnings(split(osm_roads, seq(from = 1, to = nrow(osm_roads), by = 200)))
        osm_roads <- parallel::parLapply(cl, osm_roads, fun = function(x){
          x %>% sf::st_cast("LINESTRING") %>% nngeo::st_segments(progress = FALSE)
        })
        parallel::stopCluster(cl)
      }
      # ---- Linux and macOS ----
      else {
        osm_roads <- suppressWarnings(split(osm_roads, seq(from = 1, to = nrow(osm_roads), by = 200))) %>%
          parallel::mclapply(function(x){
            x %>% sf::st_cast("LINESTRING") %>% nngeo::st_segments(progress = FALSE)
          },
          mc.cores = cores, mc.preschedule = TRUE)
      }
    } else {
      osm_roads <- suppressWarnings(split(osm_roads, seq(from = 1, to = nrow(osm_roads), by = 200)))
      osm_roads <- lapply(osm_roads, FUN = function(x){
        x %>% sf::st_cast("LINESTRING") %>% nngeo::st_segments(progress = FALSE)
      })
    }

    osm_roads <- DRIGLUCoSE::rbind_parallel(osm_roads)

    if ("result" %in% names(osm_roads)) {
      osm_roads <- osm_roads %>% dplyr::rename(geom = "result")
    }
  }


  invisible(gc())
  return(osm_roads)
}
