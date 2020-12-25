#' @title Parallel rbind
#'
#' @description Helper function to allow parallel \code{\link{rbind}}
#'
#' @param X a \code{list} object.
#' @param cores the number of cores to use.
#'
#' @return
#' @export

#' @importFrom parallel makeCluster
#' @importFrom parallel parLapply
#' @importFrom parallel stopCluster
rbind_parallel <- function(X, cores = 1L) {
  if (cores < 1L) stop("Number of cores must be 1 or greater.")

  do.call.rbind <- function(x) {do.call(rbind, x)}

  cl <- parallel::makeCluster(cores)
  X.split <- suppressWarnings(split(X, rep(1:cores, length(X) + 1)[1:length(X)]))
  X.join <- parallel::parLapply(cl, X.split, do.call.rbind)
  parallel::stopCluster(cl)

  X.out <- do.call(rbind, X.join)
  return(X.out)
}
