#' @title Calculate isochrones
#'
#' @description Calculates isochrones from \code{\link[DRIGLUCoSE]{isodistance}} by applying a off-road buffer to all polyline segments.
#'
#' @param x object of class \code{sf} containing isodistances, derived from the \code{\link[DRIGLUCoSE]{isodistance}} function.
#' @param buffer numeric; buffer distance to be applied on the polyline features of the isodistance object.
#' @param cores the number of cores to use.
#'
#' @return An \code{sf MULTIPOLYGON} of isodistances is returned. The sf object contains two fields:
#'     tag (id of the original point feature) and time (range of the segments).
#' @export
#'
#' @importFrom sf st_buffer
#' @importFrom sf st_union
#' @importFrom parallel makeCluster
#' @importFrom parallel parLapply
#' @importFrom parallel stopCluster
#' @importFrom parallel mclapply
isochrones <- function(x, buffer = 30, cores = 1) {

  this_isochrones <- function(x, buffer) {
    x %>% sf::st_buffer(buffer) %>% sf::st_union(by_feature = TRUE) #%>% nngeo::st_remove_holes()
  }

  # Convert x to list to enable parLapply/mclapply
  x_list <- suppressWarnings(split(x, seq(nrow(x))))

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

  isochs <- do.call(rbind, isochs)

  invisible(gc())
  return(isochs)
}
