#' Tests connectivity between pairs of nodes
#'
#' Tests connectivity between pairs of nodes. This function
#' tests if two nodes are directly connected with an edge (i.e. if they are neighbours).
#'
#' @param V1 A vector of node names.
#' @param V2 A vector of node names of the same length as `V1`.
#' @param graph An object of class [gGraph] or [`graph::graphNEL`] (usually
#' in the `@graph` slot of a [gGraph] object.
#' @return a vector of logical, having one value for
#' each pair of nodes.
#' @export
#' @examples
#' # create a small square graph
#' test_graph <- makeGrid(25, lon.range = c(1,5), lat.range = c(1,5))
#' # get the coordinates of the first 10 nodes
#' getCoords(test_graph)[1:10, ]
#' # test that the function correctly identifies neighbours
#' # 1 and 2 are neighbours, but 1 and 9 are not
#' areNeighbours(V1 = c("1","1"), V2 = c("2","9"), graph = test_graph)
#' @family connectivity_functions

areNeighbours <- function(V1, V2, graph) {
  # check that the two vectors are character vectors of the same length
  V1 <- as.character(V1)
  V2 <- as.character(V2)
  if (length(V1) != length(V2)) {
    stop("V1 and V2 have different lengths.")
  }
  # check that graph is a valid gGraph or graphNEL object
  if (inherits(graph, "gGraph")) {
    graph <- getGraph(graph) # extract graphNEL object from gGraph object
  }
  if (!inherits(graph, "graphNEL")) {
    stop("graph is not a valid gGraph or graphNEL object.")
  }

  edg <- edges(graph)

  ## function testing if two nodes are directly connected
  f1 <- function(A, B) {
    return(any(edg[[A]] == B))
  }

  res <- mapply(function(x, y) f1(x, y), V1, V2)

  names(res) <- paste(V1, V2, sep = "->")

  return(res)
}
