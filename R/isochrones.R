#' @title Calculate isochrones
#'
#' @description Calculates isochrones from \code{\link[DRIGLUCoSE]{isodistances}} by applying a off-road buffer to all polyline segments.
#'
#' @param x object of class \code{sf} containing isodistances, derived from the \code{\link[DRIGLUCoSE]{isodistances}} function.
#' @param buffer numeric; buffer distance to be applied on the polyline features of the isodistance object.
#' @param cores the number of cores to use.
#' @param remove_holes logical; should \code{\link[nngeo]{st_remove_holes}} function be applied?
#'
#' @return An \code{sf MULTIPOLYGON} of isodistances is returned. The sf object contains two fields:
#'     tag (id of the original point feature) and time (range of the segments).
#' @export
#'
#' @importFrom rlang parse_quo
#' @importFrom sf st_buffer
#' @importFrom sf st_union
#' @importFrom dplyr group_by
#' @importFrom dplyr group_split
#' @importFrom parallel makeCluster
#' @importFrom parallel parLapply
#' @importFrom parallel stopCluster
#' @importFrom parallel mclapply
#' @importFrom data.table rbindlist
#' @importFrom rlang caller_env
isochrones <- function(x, tag = "tag", buffer = 30, cores = 1, remove_holes = FALSE) {

  this_isochrones <- function(x, buffer, tag, remove_holes) {
    require(magrittr)
    if (remove_holes) {
      x %>%
        dplyr::group_by(time) %>%
        dplyr::summarise(!! rlang::parse_quo(tag, env = rlang::global_env()), time,
                         geom = sf::st_cast(geom, "LINESTRING") %>% sf::st_buffer(buffer) %>% sf::st_union() %>% nngeo::st_remove_holes()) %>%
        dplyr::ungroup() %>%
        sf::st_cast("MULTIPOLYGON")
    } else {
      x %>%
        dplyr::group_by(time) %>%
        dplyr::summarise(!! rlang::parse_quo(tag, env = rlang::global_env()), time,
                         geom = sf::st_cast(geom, "LINESTRING") %>% sf::st_buffer(buffer) %>% sf::st_union()) %>%
        dplyr::ungroup() %>%
        sf::st_cast("MULTIPOLYGON")
    }
  }

  # Convert x to list to enable parLapply/mclapply
  x_list <- x %>%
    dplyr::group_by(!! rlang::parse_quo(tag, env = rlang::global_env())) %>%
    dplyr::group_split()

  if (cores > 1) {
    # ---- WINDWOS ----
    if (Sys.info()[["sysname"]] == "Windows") {
      # Use mclapply for paralleling the isochrones function
      cl <- parallel::makeCluster(cores)
      isochs <- parallel::parLapply(cl, x_list, fun = this_isochrones, buffer = buffer, tag = tag, remove_holes = remove_holes)
      parallel::stopCluster(cl)
    }
    # ---- Linux and macOS ----
    else {
      # Use mclapply for paralleling the isochrones function
      isochs <- parallel::mclapply(x_list, this_isochrones, buffer = buffer, tag = tag, remove_holes = remove_holes,
                                   mc.cores = cores, mc.preschedule = TRUE)
    }
  } else {
    isochs <- lapply(x_list, FUN = this_isochrones, buffer = buffer, tag = tag, remove_holes = remove_holes)
  }

  isochs <- sf::st_as_sf(data.table::rbindlist(isochs) %>% dplyr::as_tibble()) %>%
    sf::st_make_valid()
  class(isochs) <- c(class(isochs), "isochrone")

  invisible(gc())
  return(isochs)
}
