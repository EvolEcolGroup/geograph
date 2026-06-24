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
  # find the largest set
  maxSet <- connected.sets[[which.max(lapply(connected.sets, length))]]
  maxSet.idx <- match(maxSet, nodes(myGraph))
  maxSet.idx <- maxSet.idx[!is.na(maxSet.idx)]
  # all cells NOT in the largest set need to be removed

  # get the edges from the graph
  edgeW <- edgeWeights(myGraph)
  edgeL <- edgeL(myGraph)

  # We create a new list and then copy over only the edges for which we have a node
  # from the largest set
  newEdgeL <- list()
  for (i in seq_along(edgeL)) {
    newEdgeL[[i]] <- list()
    # if the source is in the set, we keep its edges but remove any destination not in the set
    if (i %in% maxSet.idx) {
        keep <- edgeL[[i]]$edges %in% maxSet.idx
        newEdgeL[[i]]$edges <- edgeL[[i]]$edges[keep]
        newEdgeL[[i]]$weights <- edgeW[[i]][keep]
    } else { # we remove this edge
      newEdgeL[[i]]$edges <- numeric(0)
      newEdgeL[[i]]$weights <- numeric(0)
    }
  }
  names(newEdgeL) <- nodes(myGraph) # items of the list must be named

  newGraph <- new("graphNEL", nodes = nodes(myGraph), edgeL = newEdgeL)
  res <- x
  res@graph <- newGraph

  res <- dropDeadNodes(res)

  return(res)
}
