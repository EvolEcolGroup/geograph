#' Keep only the largest connected set
#'
#' This function removes all nodes that are not part of the largest connected set.
#'
#' @param x a [gGraph] object
#' @returns a [gGraph] object with only the nodes from the largest set remaining
#' @examples
#' max_set <- keepMaxConnectedSet(worldgraph.10k)
#' plot(max_set)
#'
#' @export

keepMaxConnectedSet <- function(x) {
  myGraph <- getGraph(x)
  connected.sets <- RBGL::connectedComp(myGraph)
  # find the largest set (the first one in case of ties)
  maxSet <- connected.sets[[which.max(lengths(connected.sets))]]
  # keep the nodes of the largest set, in their original order; nodes without
  # edges are dropped, so a set of a single node (edgeless graph) gives an
  # empty graph
  idx <- if (length(maxSet) > 1) sort(match(maxSet, nodes(myGraph))) else integer(0)
  res <- x[idx]
  # the edges of the largest set have always been stored with their weights:
  # a graph without stored costs gets weights of 1
  if (length(myGraph@edgeData@data) == 0) {
    et <- .edgeTable(res@graph, weights = FALSE)
    res@graph <- .graphFromTable(nodes(res@graph), et$from, et$to, w = rep(1, length(et$from)))
  }

  return(res)
}
