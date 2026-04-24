#' @title Make a new gGraph object from a custom discrete global grid
#'
#' @description This function constructs a new [`gGraph`] object based on a
#' discrete global grid system (DGGS) with a user-defined spatial resolution.
#' The graph is restricted to a geographic bounding box defined by the user.
#'
#' @param geo.box A geographic bounding box. With either a named numeric vector
#' with `xmin`, `xmax`, `ymin`, `ymax` or an object of class
#' `bbox` or `sf`. Coordinates must be in longitude/latitude (EPSG:4326).
#' @param spacing A positive numeric value giving the desired spacing (in km)
#' of the discrete global grid. The closest available DGGS resolution is used.
#' @param ... Additional arguments passed to other methods (currently not used).
#' @return A [`gGraph`] object representing the DGGS restricted to the
#' specified geographic region. The `@nodes.attr` slot is empty.
#' @details The function constructs a discrete global grid using the `dggridR` package,
#' then identifies neighboring grid cells using the `spdep` package, and finally builds a
#' [`gGraph`] object with the resulting graph structure and node coordinates.
#' @examples
#' # Define a geographic bounding box (e.g., for a region in Europe)
#' geo.box <- c(xmin = -10, xmax = 30, ymin = 35, ymax = 60)
#' # Create a gGraph with a spacing of 1000 km
#' ggraph <- createNewGraph(geo.box = geo.box, spacing = 1000)
#' plot(ggraph, edge = TRUE)
#' @export
#'

createNewGraph <- function(geo.box, spacing, ...) {
  if (!is.numeric(spacing) ||
    length(spacing) != 1 ||
    is.na(spacing) ||
    spacing <= 0) {
    stop("`spacing` must be a single positive numeric value (in km).")
  }

  # get the boundaries of the region in the format we need
  if (inherits(geo.box, "sf")) {
    bbox <- sf::st_bbox(geo.box)
  } else if (inherits(geo.box, "bbox")) { # TODO look if we need to convert crs!
    bbox <- geo.box
  } else if (is.numeric(geo.box) && all(c("xmin", "xmax", "ymin", "ymax") %in% names(geo.box))) {
    bbox <- sf::st_bbox(geo.box, crs = 4326)
  } else {
    stop("geo.box must be a bbox, sf object, or named numeric vector.")
  }

  # construct the gird (for the whole world) using the specified spacing
  dggs <- dggridR::dgconstruct(
    spacing = spacing,
    metric = TRUE,
    resround = "down"
  )

  resolution <- dggs$res

  cell.size <- dggridR::dggetres(dggs) |>
    dplyr::filter(.data$res == resolution) |>
    dplyr::pull(dplyr::all_of("spacing_km"))

  grid.sf <- dggridR::dgrectgrid(
    dggs,
    minlon = bbox["xmin"],
    maxlon = bbox["xmax"],
    minlat = bbox["ymin"],
    maxlat = bbox["ymax"],
    cellsize = cell.size / (111 * 3) # TODO need to find a way to make this dependent on the spacing
  )

  # get the coords argument from the centers of the grid cells
  centers <- dggridR::dgSEQNUM_to_GEO(dggs, grid.sf$seqnum)

  coords <- data.frame(
    lon = centers$lon,
    lat = centers$lat
  )

  # get the neighbours using spdep
  xy <- as.matrix(coords)
  nb <- spdep::dnearneigh( # TODO switch to sf at some point here
    xy,
    d1 = 0,
    d2 = cell.size * 1.5, # TODO make this more robust
    longlat = TRUE
  )

  neighbours <- unclass(nb)

  # build graphNEL object
  node.ids <- as.character(seq_len(nrow(coords)))

  edgeL <- lapply(seq_along(neighbours), function(i) {
    list(edges = node.ids[neighbours[[i]]])
  })
  names(edgeL) <- node.ids

  gNEL <- new(
    "graphNEL",
    nodes = node.ids,
    edgeL = edgeL,
    edgemode = "undirected"
  )

  # create the gGraph object

  newGraph <- new(
    "gGraph",
    graphNEL = gNEL,
    coords = coords,
    nodes.attr = data.frame(row.names = node.ids),
    meta = list(costs = NULL, colors = NULL),
    neighbours = neighbours
  )

  return(newGraph)
}
