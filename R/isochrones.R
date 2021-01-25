#' @title Calculate isochrones
#'
#' @description Calculates isochrones from \code{\link[DRIGLUCoSE]{isodistances}} by applying a off-road buffer to all polyline segments.
#'
#' @param x object of class \code{sf} containing isodistances, derived from the \code{\link[DRIGLUCoSE]{isodistances}} function.
#' @param buffer numeric; buffer distance to be applied on the polyline features of the isodistance object.
#' @param cores the number of cores to use.
#'
#' @return An \code{sf MULTIPOLYGON} of isodistances is returned. The sf object contains two fields:
#'     tag (id of the original point feature) and time (range of the segments).
#' @export
#'
#' @importFrom rlang parse_quosure
#' @importFrom sf st_buffer
#' @importFrom sf st_union
#' @importFrom dplyr group_by
#' @importFrom dplyr group_split
#' @importFrom parallel makeCluster
#' @importFrom parallel parLapply
#' @importFrom parallel stopCluster
#' @importFrom parallel mclapply
isochrones <- function(x, tag = "tag", buffer = 30, cores = 1) {

  this_isochrones <- function(x, buffer) {
    x %>% sf::st_buffer(buffer) %>% sf::st_union(by_feature = TRUE) #%>% nngeo::st_remove_holes()
  }

  # Convert x to list to enable parLapply/mclapply
  x_list <- x %>%
    dplyr::group_by(!! rlang::parse_quosure(tag)) %>%
    dplyr::group_split()

  if (cores > 1) {
    # ---- WINDWOS ----
    if (Sys.info()[["sysname"]] == "Windows") {
      # Use mclapply for paralleling the isochrones function
      cl <- parallel::makeCluster(cores)
      isochs <- parallel::parLapply(cl, x_list, fun = this_isochrones, buffer = buffer)
      parallel::stopCluster(cl)
    }
    # ---- Linux and macOS ----
    else {
      # Use mclapply for paralleling the isochrones function
      isochs <- mclapply(x_list, this_isochrones, buffer = buffer,
                         mc.cores = cores, mc.preschedule = TRUE)
    }
  } else {
    isochs <- lapply(x_list, FUN = this_isochrones, buffer = buffer)
  }

  isochs <- DRIGLUCoSE::rbind_parallel(isochs, cores = cores)
  class(isochs) <- c(class(isochs), "isochrone")

  invisible(gc())
  return(isochs)
}
