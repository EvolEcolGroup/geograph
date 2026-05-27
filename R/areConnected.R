#' Test if a set of nodes form a connected set
#'
#' This function tests if a set of nodes form a connected set on a
#'  [`gGraph`] object.
#'
#'  @details
#'  This function is very similar to [isConnected()], but it allows
#'  the user to specify a subset of nodes to test for connectivity,
#'  whereas [isConnected()] tests if all nodes in the object form a
#'  connected set. Note that [isConnected()] is a method for both
#'  [gGraph] and [gData] objects, whereas [areConnected()] is
#'  only a method for [gGraph] objects.
#'
#'  This is an implementation of the [graph::isConnected()] function for
#'  data classes in `geoGraph`.
#'
#' @param x a [gGraph] object
#' @param nodes a vector of node names
#' @return a single logical value, being TRUE if nodes form a connected set.
#' @family connectivity_functions
#' @examples
#' # create a small square graph
#' test_graph <- makeSquareGrid(25, lon.range = c(1, 5), lat.range = c(1, 5))
#' # test that the function correctly identifies connected sets
#' # 1, 9, and 10 are connected
#' areConnected(test_graph, nodes = c("1", "9", "10"))
#' # even though they are not neighbours
#' areNeighbours(V1 = "1", V2 = "9", graph = test_graph)
#' areNeighbours(V1 = "1", V2 = "10", graph = test_graph)
#' @export
areConnected <- function(x, nodes) { # x is a gGraph
  ## some checks ##
  ## if(!require(RBGL)) stop("RBGL package is required.") not needed
  if (!is.gGraph(x)) stop("x is not a valid gGraph object")
  if (!all(nodes %in% getNodes(x))) stop("Some specified nodes were not found in the gGraph object.")
  nodes <- unique(nodes)

  ## get connected sets ##
  ## !! use RBGL::connectedComp from RBGL rather than connComp from graph
  ## 100 times faster
  connected.sets <- RBGL::connectedComp(getGraph(x))

  ## just keep sets > 1 node
  temp <- sapply(connected.sets, length)
  reOrd <- order(temp, decreasing = TRUE) # sets ordered in decreasing size
  temp <- temp[reOrd]
  if (min(temp) == 1) {
    connected.sets <- connected.sets[reOrd][1:(which.min(temp) - 1)]
  }

  names(connected.sets) <- paste("set", seq_along(connected.sets))
  
  res <- sapply(connected.sets, function(e) all(nodes %in% e))
  res <- any(res)

  return(res)
}
