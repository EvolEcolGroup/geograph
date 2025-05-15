#' Convert a terra object into a gGraph object
#'
#' This function converts a terra object into a gGraph object. The first layer
#' of the terra object is used to define the attribute of the nodes in the gGraph
#' object. The coordinates of the nodes are taken from the terra raster. Use
#' an equidistant or equal area projection for the terra object to ensure that the
#' distances between the nodes are correct.
#' @param x A terra `spatRaster` object
#' @param ... Additional arguments to be passed to the `gGraph` constructor
#' @return A `gGraph` object
#' @export

as.gGraph <- function(x, ...) {
  # only run this is terra is installed
  if (!requireNamespace("terra", quietly = TRUE)) {
    stop("The terra package is required for this function. Please install it.")
  }
  # check that x is a spatRaster
  if (!inherits(x, "SpatRaster")) {
    stop("x must be a terra SpatRaster object")
  }
  
  coords <- terra::crds(x)
  neighbour_matrix <- terra::adjacent(x, 1:nrow(coords))
  # convert the matrix into a list, with each element including a row of the matrix
  neighbour_list <- lapply(1:nrow(neighbour_matrix), function(i) {
    this_neighbours <- neighbour_matrix[i, ]
    # remove elements that are NaN
    this_neighbours <- this_neighbours[!is.na(this_neighbours)]
    list(edges = this_neighbours, weights = rep(1, length(this_neighbours)))
  })
  names(neighbour_list) <- 1:nrow(neighbour_matrix)
  graph <- graph::graphNEL(
    nodes = names(neighbour_list),
    edgeL = neighbour_list
  )
  nodes.attr <- as.data.frame(x)
  graph_test <- new("gGraph",
    coords = coords,
    nodes.attr = nodes.attr,
    graph = graph,
    ...
  )
  return(graph_test)
}


# set.seed(1)
# raster <- terra::rast(matrix(rbinom(size = 1, n=100,prob = 0.5),
#                              nrow=10, ncol=10), crs="+proj=utm +zone=1 +datum=WGS84")
# names(raster) <- "land_mask"
