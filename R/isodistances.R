#' @title Calculate isodistances
#'
#' @description Based on a road network (\code{\link[DRIGLUCoSE]{osm_roads}}), this function calculates isodistances.
#'     An isodistance is a polyline of equal distance from a given point. All street segments belonging to a range will be summarized.
#'
#' @param x object of class \code{sf} of the origin point(s). Must contain one or more \code{POINT} features.
#' @param road_network object of class \code{sf} containing road features, derived from the \code{\link[DRIGLUCoSE]{osm_roads}} function.
#' @param tag character or NA; string containing the column name, that indicates the unique tag column.
#' @param isochrones_seq a numeric vector of isochrone values (in minutes).
#' @param speed numeric or character; either numeric value of speed (meters/minute) or string containing the column name, that indicates the speed.
#' @param cores the number of cores to use.
#'
#' @return An \code{sf MULTILINESTRING} of isodistances is returned. The sf object contains two fields:\cr
#'     tag (id of the original point feature) and time (range of the segments).
#' @export
#'
#' @import sf
#' @import dplyr
#'
#' @importFrom rlang parse_quosure
#' @importFrom igraph neighbors
#' @importFrom tidygraph activate
#' @importFrom nabor knn
#' @importFrom parallel makeCluster
#' @importFrom parallel parLapply
#' @importFrom parallel stopCluster
#' @importFrom parallel mclapply
isodistances <- function(x, road_network, tag = NA, isochrones_seq = c(5, 10, 15, 20), speed, cores = 1) {

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

    x <- dplyr::rename(x, geom = geometry_col_name)

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
    }
  }

  # cores
  if (cores < 1L) stop("Number of cores must be 1 or greater.")

  # Check if location CRS is cartesian. This is required for the buffer
  is_cartesian <- x %>%
    sf::st_crs() %>%
    .[[2]] %>%
    grepl("CS[Cartesian,", ., fixed = T)

  if (!is_cartesian) stop("A cartesian CRS is required for applying buffers")
  rm(sf_class, is_cartesian)


  # 2. street network function ------------------------------------------
  this_isodistance <- function(x, road_network, isochrones_seq, speed, tag) {
    # Speed
    if (is.character(speed)) {
      if (speed %in% names(x)) {
        speed = as.numeric(x[[speed]])
        message("speed will be used from sf object")
      } else (
        stop("speed must be either numeric or a column of the sf object")
      )
    }

    #### 1. Edges and nodes ####
    # Give each edge a unique index
    this_buffer <- x %>%
      sf::st_buffer(max(isochrones_seq) * speed)

    st_agr(road_network) = "constant"

    edges <- road_network %>%
      sf::st_intersection(this_buffer$geom) %>%
      sf::st_cast("MULTILINESTRING") %>%
      sf::st_cast("LINESTRING", warn = F) %>%
      dplyr::mutate(edgeID = c(1:n()))


    # Create nodes at the start and end point of each edge
    nodes <- edges %>%
      sf::st_coordinates() %>%
      dplyr::as_tibble() %>%
      dplyr::rename(edgeID = L1) %>%
      dplyr::group_by(edgeID) %>%
      dplyr::slice(c(1, n())) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(start_end = rep(c('start', 'end'), times = dplyr::n()/2))


    # Give each node a unique index
    nodes <- nodes %>%
      dplyr::mutate(xy = paste(.$X, .$Y)) %>%
      dplyr::mutate(nodeID = dplyr::group_indices(., factor(xy, levels = unique(xy)))) %>%
      dplyr::select(-xy)


    # Combine the node indices with the edges
    source_nodes <- nodes %>%
      dplyr::filter(start_end == 'start') %>%
      dplyr::pull(nodeID)

    target_nodes <- nodes %>%
      dplyr::filter(start_end == 'end') %>%
      dplyr::pull(nodeID)

    edges = edges %>%
      dplyr::mutate(from = source_nodes, to = target_nodes)


    # Remove duplicate nodes
    nodes <- nodes %>%
      dplyr::distinct(nodeID, .keep_all = TRUE) %>%
      dplyr::select(-c(edgeID, start_end)) %>%
      sf::st_as_sf(coords = c('X', 'Y')) %>%
      sf::st_set_crs(sf::st_crs(edges))


    # Convert to tbl_graph
    graph <- tidygraph::tbl_graph(nodes = nodes, edges = dplyr::as_tibble(edges), directed = FALSE) %>%
      tidygraph::activate(edges) %>%
      dplyr::mutate(length = sf::st_length(geom))


    #### Network Analysis ####
    # 2. Coordinates of the origin
    coords_o <- x %>%
      sf::st_coordinates() %>%
      matrix(ncol = 2)

    # Coordinates of all nodes in the network
    nodes <- graph %>%
      tidygraph::activate(nodes) %>%
      dplyr::as_tibble() %>%
      sf::st_as_sf()

    # Calculate nearest points on the network.
    coords <- nodes %>%
      sf::st_coordinates()

    node_index_o <- nabor::knn(data = coords, query = coords_o, k = 1)
    node_o <- nodes[node_index_o$nn.idx, ]


    #### Street Network ####
    max_length <- speed * max(isochrones_seq)

    # Tibble to save nodes that need to be checked
    outer_nodes <- dplyr::tibble(nodeID = as.integer(),
                                 length = as.double())

    # Get neighboring nodes of the origin
    first_nodes <- igraph::neighbors(graph, (node_o$nodeID)) %>%
      as.numeric() %>%
      dplyr::as_tibble() %>%
      dplyr::rename(nodeID = value) %>%
      dplyr::inner_join(nodes, ., by = "nodeID")

    # Get edges that go between first_nodes and origin and sort by length
    first_edges <- edges[(edges$from == node_o$nodeID & edges$to %in% first_nodes$nodeID) |
                           (edges$from %in% first_nodes$nodeID & edges$to == node_o$nodeID), ] %>%
      dplyr::mutate(length = sf::st_length(geom) %>% as.numeric()) %>%
      dplyr::select(edgeID, length, highway) %>%
      sf::st_join(first_nodes, by = geom)

    # Create star_network to store all edges
    star_network <- first_edges[0,]

    # Append to collection that need to be searched
    outer_nodes <- outer_nodes %>%
      rbind(first_edges %>% dplyr::as_tibble() %>% dplyr::select(nodeID, length)) %>%
      dplyr::arrange(length)

    while (nrow(outer_nodes) > 0) {
      # Calculate new nodes
      new_nodes <- igraph::neighbors(graph, outer_nodes[1,]$nodeID) %>%
        as.numeric() %>%
        dplyr::as_tibble() %>%
        dplyr::rename(nodeID = value) %>%
        dplyr::inner_join(nodes, ., by = "nodeID")

      # Get edges that go between first_nodes and origin and sort by length
      new_edges <- edges[(edges$from == outer_nodes[1,]$nodeID & edges$to %in% new_nodes$nodeID) |
                           (edges$from %in% new_nodes$nodeID & edges$to == outer_nodes[1,]$nodeID),] %>%
        dplyr::mutate(length = sf::st_length(geom) %>% as.numeric()) %>%
        dplyr::select(edgeID, length, highway) %>%
        sf::st_join(new_nodes, by = geom)

      # Remove edges, that are already in the star-network and longer than max_length
      new_edges <- new_edges %>%
        dplyr::mutate(length = length + outer_nodes[1,]$length) %>%
        dplyr::filter(length <= max_length & !(edgeID %in% star_network$edgeID))

      # Add new_edges to star_network
      star_network <- star_network %>%
        rbind(new_edges)

      # Remove first entry of outer_nodes
      outer_nodes[1,] <- NA
      outer_nodes <- na.omit(outer_nodes)

      # Append to collection that need to be searched
      outer_nodes <- new_edges %>%
        dplyr::as_tibble() %>%
        dplyr::select(nodeID, length) %>%
        dplyr::filter(!nodeID %in% outer_nodes$nodeID) %>%
        rbind(outer_nodes) %>%
        dplyr::arrange(length)
    }

    # Group by time (eg 5, 10, 15, 20 min)
    star_network <- star_network %>%
      dplyr::mutate(time = as.integer(NA))

    for (j in seq_along(isochrones_seq)) {
      max_length <- speed * isochrones_seq[j]

      star_network[star_network$length <= max_length & is.na(star_network$time), ] <- star_network %>%
        dplyr::filter(length <= max_length & is.na(time)) %>%
        dplyr::mutate(time = isochrones_seq[j])
    }

    star_network <- star_network %>%
      dplyr::mutate(time = as.factor(time)) %>%
      dplyr::group_by(time) %>%
      dplyr::summarize(geom = sf::st_union(geom)) %>%
      dplyr::select(time)

    if (nrow(star_network) > 0) {
      for (j in 1:nrow(star_network)) {
        star_network[j,]$geom <- star_network[1:j,] %>%
          sf::st_union()
      }
    }

    if (!is.na(tag)) {
      this_tag <- x %>%
        dplyr::as_tibble() %>%
        dplyr::select(!! rlang::parse_quosure(tag)) %>%
        unlist() %>%
        as.vector()

      star_network <- star_network %>%
        dplyr::mutate(!! rlang::parse_quosure(tag) := this_tag) %>%
        dplyr::select(!! rlang::parse_quosure(tag), time)
    }

    invisible(gc())
    return(star_network)
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
    dplyr::select(row) %>%
    unlist() %>%
    as.vector()

  # Convert list to one sf
  isodistances <- isodistances[is_sf_indices] %>%
    do.call(rbind, .)

  invisible(gc())
  return(isodistances)
}
