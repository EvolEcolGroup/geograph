#' @title Make a new gGraph object from a custom hexagonal grid
#'
#' @description This function constructs a new [`gGraph`] object based on a hexagonal
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
#' # Create a gGraph with a spacing of 300 km
#' ggraph <- makeHexGrid(geo.box = geo.box, spacing = 300)
#' plot(ggraph, edge = TRUE)
#' @export
#'

makeHexGrid <- function(geo.box, spacing, ...) {
  if (!is.numeric(spacing) ||
    length(spacing) != 1 ||
    is.na(spacing) ||
    spacing <= 0) {
    stop("`spacing` must be a single positive numeric value (in km).")
  }

  # get the boundaries of the region in the format we need
  if (inherits(geo.box, "sf")) {
    if (!is.na(sf::st_crs(geo.box)) && sf::st_crs(geo.box) != sf::st_crs(4326)) {
      stop("geo.box must be in longitude/latitude (EPSG:4326).")
    }
    bbox <- sf::st_bbox(geo.box)
  } else if (inherits(geo.box, "bbox")) {
    if (!is.na(sf::st_crs(geo.box)) && sf::st_crs(geo.box) != sf::st_crs(4326)) {
      stop("geo.box must be in longitude/latitude (EPSG:4326).")
    }
    bbox <- geo.box
  } else if (is.numeric(geo.box) && all(c("xmin", "xmax", "ymin", "ymax") %in% names(geo.box))) {
    bbox <- geo.box[c("xmin", "xmax", "ymin", "ymax")]
  } else {
    stop("geo.box must be a bbox, sf object, or named numeric vector.")
  }
  
  # check the validity of coordinates
  if (bbox["xmin"] < -180 || bbox["xmin"] > 180 ||
      bbox["xmax"] < -180 || bbox["xmax"] > 180) {
    stop("Longitude values in geo.box must be between -180 and 180.")
  }
  if (bbox["ymin"] < -90 || bbox["ymax"] > 90) {
    stop("Latitude values in geo.box must be between -90 and 90.")
  }
  if (bbox["ymin"] >= bbox["ymax"]) {
    stop("ymin must be less than ymax in geo.box.")
  }

  # construct the gird (for the whole world) using the specified spacing
  dggs <- dggridR::dgconstruct(
    spacing = spacing,
    metric = TRUE,
    resround = "down"
  )
  
  # find all grid cells within the bounding box
  resolution <- dggs$res

  cell.size <- dggridR::dggetres(dggs) %>%
    dplyr::filter(.data$res == resolution) %>%
    dplyr::pull(dplyr::all_of("spacing_km"))

  samp.cellsize <- cell.size / 333 #at least 9 points per cell
  
  samp <- .make_sample_points(
    xmin     = bbox["xmin"], xmax = bbox["xmax"],
    ymin     = bbox["ymin"], ymax = bbox["ymax"],
    cellsize = samp.cellsize
  )
  
  # get the boundary coordinates of cells 
  seqnums <- dggridR::dgGEO_to_SEQNUM(dggs, samp$lon, samp$lat)$seqnum
  seqnums <- unique(seqnums)
  grid.sf <- dggridR::dgcellstogrid(dggs, seqnums)
  

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
    d2 = cell.size * 1.5,
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


#' Generate a dense grid of sample points across a bounding box
#'
#' Internal helper for [`makeHexGrid`]. Produces a regular grid of longitude /
#' latitude points covering the requested region. When `xmin` is greater than
#' `xmax`, the region is assumed to cross the antimeridian and the longitude
#' span is split into `[xmin, 180]` and `[-180, xmax]`.
#'
#' @param xmin,xmax longitude limits. `xmin > xmax` indicates a dateline
#'   crossing.
#' @param ymin,ymax latitude limits.
#' @param cellsize spacing in degrees between sample points.
#' @return a `data.frame` with columns `lon` and `lat`.
#' @noRd
.make_sample_points <- function(xmin, xmax, ymin, ymax, cellsize) {
  if (xmin > xmax) {
    ## include 180 and xmax explicitly so edge/seam cells are sampled
    lon <- c(seq(xmin, 180, by = cellsize), 180,
             seq(-180, xmax, by = cellsize), xmax)
    lon <- sort(unique(lon))
  } else {
    lon <- sort(unique(c(seq(xmin, xmax, by = cellsize), xmax)))
  }
  lat <- sort(unique(c(seq(ymin, ymax, by = cellsize), ymax)))
  expand.grid(lon = lon, lat = lat)
}
