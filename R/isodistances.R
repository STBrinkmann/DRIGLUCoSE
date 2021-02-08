#' @title Calculate isodistances
#'
#' @description Based on a road network (\code{\link[DRIGLUCoSE]{osm_roads}}), this function calculates isodistances.
#'     An isodistance is a polyline of equal distance from a given point. All street segments belonging to a range will be summarized.
#'
#' @param x object of class \code{sf} of the origin point(s). Must contain one or more \code{POINT} features.
#' @param road_network object of class \code{sf} containing road features, derived from the \code{\link[DRIGLUCoSE]{osm_roads}} function.
#' @param speed numeric or character; either numeric value of speed (meters/minute) or string containing the column name, that indicates the speed.
#' @param tag character or NA; string containing the column name, that indicates the unique tag column.
#' @param isochrones_seq a numeric vector of isochrone values (in minutes).
#' @param cores the number of cores to use.
#'
#' @return An \code{sf MULTILINESTRING} of isodistances is returned. The sf object contains two fields:\cr
#'     tag (id of the original point feature) and time (range of the segments).
#' @export
#'
#' @import sf
#' @import dplyr
#' @import sfnetworks
#'
#' @importFrom rlang parse_quo
#' @importFrom nabor knn
#' @importFrom tidygraph node_distance_from
#' @importFrom units set_units
#' @importFrom parallel makeCluster
#' @importFrom parallel parLapply
#' @importFrom parallel stopCluster
#' @importFrom parallel mclapply
isodistances <- function(x, road_network, speed = 78.5, tag = NA, isochrones_seq = c(5, 10, 15, 20), cores = 1) {

  # 1. Check input -------------------------------------------------------
  # x
  if (!is(x, "sf")) {
    stop("x must be sf object.")
  } else if (nrow(x) == 0) {
    stop("x musst contain at least one feature.")
  } else {
    # Get geometry column name
    geometry_col_name <- lapply(x, is, "sfc") %>%
      unlist() %>%
      which() %>%
      names()

    if (length(geometry_col_name) > 1) {
      warning("x contains more than one geometry column. Only the first one will be used.")
      geometry_col_name <- dplyr::first(geometry_col_name)
    }

    if (geometry_col_name != "geom") {
      x <- rename(x, geom = geometry_col_name)
    }

    # Check if geometry column only contains POINT features
    sf_class <- x %>%
      dplyr::pull(geom) %>%
      sf::st_geometry_type() %>%
      as.character() %>%
      unique()

    if((length(sf_class) > 1) || (sf_class != "POINT")) stop("x must contain only POINT features.")
  }

  # road_network
  if (!is(road_network, "sf")) {
    stop("road_network must be sf object.")
  } else if (nrow(road_network) == 0) {
    stop("road_network musst contain at least one feature.")
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
  } else if (!tag %in% names(x)) {
    stop("Tag must be a column of the sf object.")
  }

  # speed
  if (is.character(speed)) {
    if (!speed %in% names(x)) {
      stop("speed must be either numeric or a column of the sf object")
    } else {
      message("speed will be used from sf object")
    }
  }

  # cores
  if (cores < 1L) stop("Number of cores must be 1 or greater.")

  # 2. street network function ------------------------------------------
  this_isodistance <- function(x, road_network, speed, tag, isochrones_seq) {
    # Get speed from input
    if (is.character(speed)) {
      speed = as.numeric(x[[speed]])
    }

    # clean road_network
    sf::st_geometry(road_network) = sf::st_geometry(road_network) %>%
      lapply(function(x) round(x, 0)) %>%
      sf::st_sfc(crs = sf::st_crs(road_network))

    # Build sfNetwork
    network <- sfnetworks::as_sfnetwork(road_network, directed = FALSE) %>%
      sfnetworks::activate("edges") %>%
      dplyr::mutate(weight = sfnetworks::edge_length(),
                    ID = 1:n(),
                    speed = units::set_units(speed, "m/min"),
                    time = weight / speed)

    # Get node for x
    coords_o <- x %>%
      sf::st_coordinates() %>%
      matrix(ncol = 2)

    nodes <- network %>%
      sfnetworks::activate(nodes) %>%
      sf::st_as_sf()

    coords <- nodes %>%
      sf::st_coordinates()

    node_index_o <- nabor::knn(data = coords, query = coords_o, k = 1)

    # Calculate Isodistances
    for (i in seq_along(isochrones_seq)) {
      if (i == 1) {
        output <- network %>%
          sfnetworks::activate(nodes) %>%
          dplyr::filter(tidygraph::node_distance_from(node_index_o$nn.idx, weights = time) <= isochrones_seq[i]) %>%
          sfnetworks::activate(edges) %>%
          sf::st_as_sf() %>%
          dplyr::mutate(time = isochrones_seq[i]) %>%
          dplyr::select(ID, time, geom)
      } else {
        output <- network %>%
          sfnetworks::activate(nodes) %>%
          dplyr::filter(tidygraph::node_distance_from(node_index_o$nn.idx, weights = time) <= isochrones_seq[i]) %>%
          sfnetworks::activate(edges) %>%
          dplyr::filter(!ID %in% output$ID) %>%
          sf::st_as_sf() %>%
          dplyr::mutate(time = isochrones_seq[i]) %>%
          dplyr::select(ID, time, geom) %>%
          dplyr::add_row(output, .)
      }
    }

    # Group by time (eg 5, 10, 15, 20 min)
    output <- output %>%
      dplyr::select(-ID) %>%
      dplyr::group_by(time) %>%
      dplyr::summarize(geom = sf::st_union(geom)) %>%
      dplyr::ungroup()


    if (!is.na(tag)) {
      this_tag <- x %>%
        dplyr::pull(tag)

      output <- output %>%
        dplyr::mutate(!! rlang::parse_quo(tag, env = globalenv()) := this_tag) %>%
        dplyr::select(!! rlang::parse_quo(tag, env = globalenv()), time)
    }
    return(output)
  }

  # 3. Parallel apply street network function ---------------------------

  # Convert x to list to enable mclapply
  x_list <- suppressWarnings(split(x, seq(nrow(x))))

  if (cores > 1) {
    # ---- WINDWOS ----
    if (Sys.info()[["sysname"]] == "Windows") {
      # Use mclapply for paralleling the isodistance function
      cl <- parallel::makeCluster(cores)
      isodistances <- parallel::parLapply(cl, x_list, fun = this_isodistance,
                                          road_network = road_network, isochrones_seq = isochrones_seq, speed = speed, tag = tag)
      parallel::stopCluster(cl)
    }
    # ---- Linux and macOS ----
    else {
      # Use mclapply for paralleling the isodistance function
      isodistances <- parallel::mclapply(x_list, this_isodistance,
                                         road_network = road_network, isochrones_seq = isochrones_seq, speed = speed, tag = tag,
                                         mc.cores = cores, mc.preschedule = FALSE)
    }
  } else {
    isodistances <- lapply(x_list, FUN = this_isodistance,
                           road_network = road_network, isochrones_seq = isochrones_seq, speed = speed, tag = tag)
  }


  # Filter outputs that are not a sf object
  is_sf_indices <- lapply(isodistances, is, "sf") %>%
    unlist() %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(row = 1:n()) %>%
    dplyr::filter(value == TRUE) %>%
    dplyr::pull(row)

  # Convert list to one sf
  isodistances <- isodistances[is_sf_indices] %>%
    do.call(rbind, .)

  invisible(gc())
  return(isodistances)
}
