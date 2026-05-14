#' Add and remove edges from a gGraph object
#'
#' The function \code{setEdges} allows one to add or remove edges in a
#' \linkS4class{gGraph} by directly specifying the relevant nodes, as a list or
#' a data.frame. This low-level function is called by \code{geo.add.edges} and
#' \code{geo.remove.edges}.
#'
#'
#' @param x a valid \linkS4class{gGraph} object.
#' @param add a list or a dataframe containing node names of edges to be added.
#' The first element of the list (or column of the data.frame) gives starting
#' nodes of edges; the second gives ending nodes. Hence, the nodes of the i-th
#' edge are \code{add[[1]][i]} and \code{add[[2]][i]} if \code{add} is a list,
#' and \code{add[i,]} if \code{add} is a data.frame.
#' @param remove same as \code{add} argument, but edges are removed.
#' @param costs a numeric vector providing costs of the edges to be added.
#' \code{costs[i]} is the weight of the i-th edge.
#' @param \dots other arguments passed to other methods (currently unused).
#' @return A \linkS4class{gGraph} object with newly added or removed edges.
#' 
#' @seealso [`getEdges`] to retrieve edges.
#'   [`geo.add.edges`] and [`geo.remove.edges`] for interactive versions.
#' @keywords utilities methods
#' @examples
#' # check which nodes are neighbours of node "1"
#' head(getEdges(worldgraph.10k, res.type = "matNames"))
#'
#' # remove an edge between two neighbouring nodes
#' node.from <- "1"
#' node.to   <- getEdges(worldgraph.10k, res.type = "matNames")[1, 2]
#'
#' x <- setEdges(worldgraph.10k,
#'               remove = data.frame(from = node.from, to = node.to))
#'
#' # verify the edge is gone
#' areNeighbours(node.from, node.to, getGraph(x))
#'
#' # add it back
#' x <- setEdges(x, add = data.frame(from = node.from, to = node.to))
#' areNeighbours(node.from, node.to, getGraph(x))
#' @export
setGeneric("setEdges", function(x, ...) {
  standardGeneric("setEdges")
})


#' @export
#' @describeIn setEdges Method for gGraph object
setMethod("setEdges", "gGraph", function(x, add = NULL, remove = NULL, costs = NULL, ...) {
  ## some checks
  if (is.null(add) & is.null(remove)) {
    return(x)
  }
  
  if (!is.null(add)) { ## add edges ##
    add <- as.data.frame(add)
    if (ncol(add) != 2) stop("add does not have two columns")
    from <- as.character(add[[1]])
    to <- as.character(add[[2]])
    if (!all(unique(c(from, to)) %in% getNodes(x))) stop("unknown specified nodes") # unknown nodes
    if (is.null(costs)) {
      costs <- rep(1, length(from))
    }
    
    myGraph <- suppressWarnings(addEdge(from = from, to = to, graph = x@graph, weights = costs))
  } else { ## remove edges ##
    remove <- as.data.frame(remove)
    if (ncol(remove) != 2) stop("remove does not have two columns")
    from <- as.character(remove[[1]])
    to <- as.character(remove[[2]])
    if (!all(unique(c(from, to)) %in% getNodes(x))) stop("unknown specified nodes") # unknown nodes
    
    ## avoid attempts to removing non-existing edges
    temp <- areNeighbours(from, to, x@graph)
    myGraph <- removeEdge(from = from[temp], to = to[temp], graph = x@graph)
  }
  
  ##  subx <- deparse(substitute(x))
  res <- x
  res@graph <- myGraph
  
  ## remember this action
  curCall <- match.call()
  ## newHist <- new("gGraphHistory", res@history, cmd=curCall, comments="Modified edges using setEdges.")
  ## res@history <- newHist
  
  ## make assignement
  ## parEnv <- parent.frame()
  ## assign(subx, res, parEnv)
  
  return(res)
}) # end setEdges
