#' @title Download and clean road features
#'
#' @description Download road features using the OSM overpass API \code{\link[osmdata]{opq}}.
#'     Simple topology cleaning is applyed using \code{\link[nngeo]{st_split_junctions}}.
#'
#' @param x object of class \code{sf}.
#' @param dist numeric; maximum number of minutes of the isochrones.
#' @param speed numeric or character; either numeric value of speed or string containing the column name, that indicates the walking speed.
#' @param cores the number of cores to use.
#' @param remove_features character vector containing feature keys that should be exluded from the analysis (e.g. motorway)
#'
#' @return object of class \code{sf} containing road features.
#' @export
#'
#' @import sf
#' @import osmdata
#' @importFrom dplyr select
#' @importFrom dplyr rename
#' @importFrom dplyr filter
#' @importFrom nngeo st_split_junctions
#' @importFrom nngeo st_segments
#' @importFrom methods is
#'
#' @examples
#' \dontrun{
#' data(Erlangen)
#' osm_roads(Erlangen, dist = 20, speed = "Speed", cores = 4)
#' }
osm_roads <- function(x, dist, speed, cores = 1L,
                      remove_features = c("motorway", "motorway_link", "trunk", "trunk_link", "raceway")) {

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
    st_crs() %>%
    .[[2]] %>%
    grepl("CS[Cartesian,", ., fixed = T)

  if (!is_cartesian) stop("A cartesian CRS is required for applying buffers")
  rm(is_cartesian)


  # 2. Download OSM data -------------------------------------------------------
  x_bbox <- x %>%
    st_buffer(dist * speed) %>%
    st_transform(4326) %>%
    st_union() %>%
    st_bbox() %>%
    as.vector()

  osm_roads <- opq(bbox = x_bbox) %>%
    add_osm_feature(key = 'highway') %>%
    osmdata_sf() %>%
    osm_poly2line()

  osm_roads <- osm_roads$osm_lines %>%
    dplyr::select(highway) %>%
    dplyr::filter(!highway %in% remove_features) %>%
    dplyr::rename(geom = geometry) %>%
    st_transform(crs = st_crs(x))

  # 3. Topology cleaning -------------------------------------------------------
  # ---- WINDWOS ----
  if (Sys.info()[["sysname"]] == "Windows") {

    cl <- parallel::makeCluster(cores)
    osm_roads <- suppressWarnings(split(osm_roads, seq(from = 1, to = nrow(osm_roads), by = 200)))
    osm_roads <- parallel::parLapply(cl, osm_roads, fun = function(x){
      x %>% st_cast("LINESTRING") %>% nngeo::st_segments(progress = FALSE)
    })
    parallel::stopCluster(cl)

    osm_roads <- DRI.GLUCoSE::rbind_parallel(osm_roads)

    if ("result" %in% names(osm_roads)) {
      osm_roads <- osm_roads %>% dplyr::rename(geom = "result")
    }
  }
  # ---- Linux and macOS ----
  else {
    osm_roads <- suppressWarnings(split(osm_roads, seq(from = 1, to = nrow(osm_roads), by = 200))) %>%
      mclapply(function(x){
        x %>% st_cast("LINESTRING") %>% nngeo::st_segments(progress = FALSE)
      },
      mc.cores = cores, mc.preschedule = TRUE) %>%
      rbind.parallel(cores = cores)

    if ("result" %in% names(osm_roads)) {
      osm_roads <- osm_roads %>% rename(geom = "result")
    }
  }

  invisible(gc())
  return(osm_roads)
}
