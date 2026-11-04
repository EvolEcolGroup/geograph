#################
## dropDeadEdges
#################


#' Get rid of some 'dead' edges or nodes
#'
#' The functions \code{dropDeadEdges} and \code{dropDeadNodes} are used to
#' remove 'dead edges' and 'dead nodes'.\cr
#'
#' Dead edges are edges associated to a prohibitive cost, that is, edges that
#' no longer imply connectivity between two nodes.\cr
#'
#' Dead nodes are nodes that are not connected to any other node, thus not
#' having any role in the connectivity of a graph.\cr
#'
#'
#' @aliases dropDeadEdges dropDeadNodes
#' @param x a valid \linkS4class{gGraph}.
#' @param thres a numeric value indicating the threshold cost for an edge to be
#' removed. All costs strictly greater than \code{thres} will be removed.
#' @return A \linkS4class{gGraph} object.

#' @keywords utilities methods
#' @export
#' @examples
#' \dontrun{
#' plot(worldgraph.10k, reset = TRUE)
#' x <- dropDeadNodes(worldgraph.10k)
#' plot(x)
#' }
#'
dropDeadEdges <- function(x, thres) { # x is a gGraph object
  if (!is.gGraph(x)) stop("x is not a valid gGraph object.")
  ## no costs stored: no edge is dead (hasCosts() would also be FALSE for
  ## uniform costs, which can all be above thres)
  if (length(getGraph(x)@edgeData@data) == 0) {
    return(x)
  }

  ## keep edges with weights under the threshold
  myGraph <- getGraph(x)
  et <- .edgeTable(myGraph)
  toKeep <- et$w <= thres
  ## each edge stores the weight of its reverse, as when the graph was rebuilt
  ## from its edge list (the same value, unless weights are asymmetric)
  w <- .reverseWeights(et$from, et$to, et$w, length(nodes(myGraph)))
  w <- .nestedWeights(w[toKeep], et$from[toKeep], length(nodes(myGraph)))
  newGraph <- .graphFromTable(nodes(myGraph), et$from[toKeep], et$to[toKeep], w)
  res <- x
  res@graph <- newGraph

  return(res)
} # end dropDeadEdges


#################
## dropDeadNodes
#################
#' @export
dropDeadNodes <- function(x) { # x is a gGraph object
  if (!is.gGraph(x)) stop("x is not a valid gGraph object.")

  ## positions of the nodes with edges, in order of first appearance in the
  ## edge list (increasing for an undirected graph)
  et <- .edgeTable(getGraph(x), weights = FALSE)
  res <- x[unique(c(et$from, et$to))]

  return(res)
} # end dropDeadNodes
