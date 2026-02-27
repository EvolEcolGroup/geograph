#' Tests connectivity between pairs of nodes
#'
#' Tests connectivity between pairs of nodes. This function
#' tests if two nodes are directly connected with an edge (i.e. if they are neighbours).
#'
#' @param V1 A vector of node names.
#' @param V2 A vector of node names of the same length as `V1`.
#' @param graph An object of class \code{graphNEL}.
#' @return a vector of logical, having one value for
#' each pair of nodes.
#' @export

areNeighbours <- function(V1, V2, graph) {
  V1 <- as.character(V1)
  V2 <- as.character(V2)
  if (length(V1) != length(V2)) stop("V1 and V2 have different lengths.")

  edg <- edges(graph)

  ## function testing if two nodes are directly connected
  f1 <- function(A, B) {
    return(any(edg[[A]] == B))
  }

  res <- mapply(function(x, y) f1(x, y), V1, V2)

  return(res)
} # end areNeighbours



