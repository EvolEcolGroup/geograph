#' Create a new gGraph object from a custom discrete global grid
#'
#' @description
#' \code{createNewGraph} constructs a new \linkS4class{gGraph} object based on a
#' discrete global grid system (DGGS) with a user-defined spatial resolution.
#' The graph is restricted to a geographic bounding box defined by the user.
#'
#' @param geo_box A geographic bounding box. With either a named numeric vector 
#' with \code{xmin}, \code{xmax}, \code{ymin}, \code{ymax} or an object of class 
#' \code{bbox} or \code{sf}. Coordinates must be in longitude/latitude (EPSG:4326).
#' @param spacing A positive numeric value giving the desired spacing (in km)
#' of the discrete global grid. The closest available DGGS resolution is used.
#' @param \dots Further arguments (currently unused).
#' @return A \linkS4class{gGraph} object representing the DGGS restricted to the
#' specified geographic region. The \code{@nodes.attr} slot is empty.
#'
#' @export
createNewGraph <- function(geo_box, spacing, ...) {
  
  if (!is.numeric(spacing) ||
      length(spacing) != 1 ||
      is.na(spacing) ||
      spacing <= 0) {
    stop("`spacing` must be a single positive numeric value (in km).")
  }
  
  # get the boundaries of the region in the format we need
  if (inherits(geo_box, "sf")) {
    bbox <- sf::st_bbox(geo_box)
  } else if (inherits(geo_box, "bbox")) { #@TODO look if we need to convert crs! 
    bbox <- geo_box
  } else if (is.numeric(geo_box) && all(c("xmin","xmax","ymin","ymax") %in% names(geo_box))) {
    bbox <- sf::st_bbox(geo_box, crs = 4326)
  } else {
    stop("geo_box must be a bbox, sf object, or named numeric vector.")
  }
  
  #construct the gird (for the whole world) using the specified spacing
  dggs <- dggridR::dgconstruct(
    spacing = spacing,
    metric = TRUE,
    resround = "down"
  )
  
  grid_sf <- dgrectgrid(
    dggs,
    minlon = bbox["xmin"],
    maxlon = bbox["xmax"],
    minlat = bbox["ymin"],
    maxlat = bbox["ymax"],
    cellsize = 0.1
  )
  
  # get the coords argument from the centers of the grid cells
  centers <- dggridR::dgSEQNUM_to_GEO(dggs, grid_sf$seqnum)
  
  coords <- data.frame(
    lon = centers$lon,
    lat = centers$lat
  )
  
  # get the neighbours using spdep
  xy <- as.matrix(coords)
  nb <- spdep::dnearneigh( #@TODO switch to sf at some point here 
    xy,
    d1 = 0,
    d2 = spacing * 1.1,
    longlat = TRUE
  )
  
  neighbours <- unclass(nb)
  
  # build graphNEL object
  node_ids <- as.character(seq_len(nrow(coords)))
  
  edgeL <- lapply(seq_along(neighbours), function(i) {
    list(edges = node_ids[neighbours[[i]]])
  })
  names(edgeL) <- node_ids
  
  gNEL <- new(
    "graphNEL",
    nodes = node_ids,
    edgeL = edgeL,
    edgemode = "undirected"
  )
  
  #create the gGraph object
  
  new_ggraph <- new(
    "gGraph",
    graphNEL = gNEL,
    coords = coords,
    nodes.attr = data.frame(row.names = node_ids),
    meta = list(costs = NULL, colors = NULL),
    neighbours = neighbours
  )
  
  return(new_ggraph)
}
