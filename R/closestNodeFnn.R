#' Building a nearest neighbor index for a gGraph
#'
#' This is a helper function to build a nearest neighbor index for a gGraph object. 
#' It extracts the coordinates of the nodes in the graph and converts them to Cartesian 
#' coordinates for efficient nearest neighbor searches in 'closestNodeFNN'
#' 
#' @param x A [`gGraph`] object
#' 
#' @return A list containing the nodes and their corresponding Cartesian coordinates
#' @export

buildNnIndex <- function(x) {
  coords <- getCoords(x)
  lon <- coords[, 1] * pi / 180
  lat <- coords[, 2] * pi / 180
  xyz <- cbind(cos(lat) * cos(lon),
               cos(lat) * sin(lon),
               sin(lat))
  list(nodes = getNodes(x), xyz = xyz)
}

#' Sped up version of closestNode using FNN::get.knnx
#' 
#' This function finds the closest node in a gGraph to a given set of coordinates 
#' using the FNN package for fast nearest neighbor searches. 
#' It takes an index built by `buildNnIndex` and a set of coordinates, 
#' converts them to Cartesian coordinates, and returns the closest nodes.
#' 
#' @param index A list containing the nodes and their corresponding Cartesian coordinates,
#'             typically created by `buildNnIndex`
#' @param loc A matrix or data frame of coordinates (longitude, latitude) to find the closest nodes for
#' 
#' @return A named vector of the closest nodes corresponding to the input coordinates
#' @export
#' @examples
#' idx.fnn <- buildNnIndex(worldgraph.40k)
#' result.fnn <- closestNodeFnn(idx.fnn, hgdp@coords)

closestNodeFnn <- function(index, loc) {
  loc <- as.matrix(as.data.frame(loc))
  lon <- loc[, 1] * pi / 180
  lat <- loc[, 2] * pi / 180
  qxyz <- cbind(cos(lat) * cos(lon),
                cos(lat) * sin(lon),
                sin(lat))
  nn_idx <- FNN::get.knnx(index$xyz, qxyz, k = 1)$nn.index[, 1]
  res <- index$nodes[nn_idx]
  names(res) <- rownames(loc)
  res
}
