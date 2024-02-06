#' Keep only the largest connected set
#' 
#' This function removes all nodes that are not part of the largest connected set.
#' 
#' @param x a [gGraph] object
#' @value a [gGraph] object with only the nodes from the largest set remaining
#' @examples
#' max_set <- keepMaxConnectedSet(worldgraph.40k)
#' plot(max_set)
#'
#' @export

keepMaxConnectedSet <- function (x){
  myGraph <- getGraph(x)
  connected_sets <- RBGL::connectedComp(myGraph)
  # find the largest set
  max_set <- connected_sets[[which.max(lapply(connected_sets, length))]]
  max_set <- as.numeric(max_set)
  # all cells NOT in the largest set need to be removed
  
  # get the edges from the graph
  edgeW <- edgeWeights(myGraph)
  edgeL <- edgeL(myGraph)
  
  # We create a new list and then copy over only the edges for which we have a node
  # from the largest set
  newEdgeL <- list()
  for (i in 1:length(edgeL)) {
    newEdgeL[[i]] <- list()
    # if the source is in the set, we keep its edges but remove any destination not in the set
    if (i %in% max_set){
      newEdgeL[[i]]$edges <- edgeL[[i]]$edges[edgeL[[i]]$edges %in% max_set]
      newEdgeL[[i]]$weights <- edgeW[[i]][edgeL[[i]]$edges %in% max_set]
    } else { #we remove this edge
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
