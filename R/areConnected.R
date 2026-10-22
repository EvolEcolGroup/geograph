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

  ## get connected sets (with more than one node) ##
  connected.sets <- .connectedSets(getGraph(x))

  res <- vapply(connected.sets, function(e) all(nodes %in% e), logical(1))
  res <- any(res)

  return(res)
}


#' internal: connected sets of a graph with more than one node, by decreasing
#' size and named "set 1", "set 2", ... (an empty list if there is none)
#' @noRd
.connectedSets <- function(graph) {
  ## !! use RBGL::connectedComp from RBGL rather than connComp from graph
  ## 100 times faster
  connected.sets <- RBGL::connectedComp(graph)
  connected.sets <- connected.sets[order(lengths(connected.sets), decreasing = TRUE)]
  connected.sets <- connected.sets[lengths(connected.sets) > 1]
  names(connected.sets) <- sprintf("set %d", seq_along(connected.sets))
  connected.sets
}
