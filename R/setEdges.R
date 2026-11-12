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
#' node.to <- getEdges(worldgraph.10k, res.type = "matNames")[1, 2]
#'
#' x <- setEdges(worldgraph.10k,
#'   remove = data.frame(from = node.from, to = node.to)
#' )
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
  if (is.null(add) && is.null(remove)) {
    return(x)
  }

  if (!is.null(add) && !is.null(remove)) {
    stop("Only one of `add` or `remove` can be specified per call.")
  }

  if (!is.null(add)) { ## add edges ##
    add <- as.data.frame(add)
    if (ncol(add) != 2) stop("add does not have two columns")
    from <- as.character(add[[1]])
    to <- as.character(add[[2]])
    if (!all(unique(c(from, to)) %in% getNodes(x))) stop("unknown specified nodes") # unknown nodes
    if (is.null(costs)) {
      costs <- rep(1, length(from))
    } else if (length(costs) == 1L) {
      costs <- rep(costs, length(from))
    } else if (length(costs) != length(from)) {
      stop("`costs` must have length 1 or match the number of edges in `add`.")
    }

    ## append the new neighbours to the edge list of each node (both ends for an
    ## undirected graph), without duplicates; much faster than graph::addEdge(),
    ## which looks up each node by name
    myGraph <- x@graph
    node.names <- nodes(myGraph)
    from.idx <- match(from, node.names)
    to.idx <- match(to, node.names)
    if (isDirected(myGraph)) {
      new.nbrs <- split(to.idx, factor(from.idx, levels = seq_along(node.names)))
    } else {
      new.nbrs <- split(c(to.idx, from.idx), factor(c(from.idx, to.idx), levels = seq_along(node.names)))
    }
    for (i in which(lengths(new.nbrs) > 0)) {
      myGraph@edgeL[[i]]$edges <- unique(c(myGraph@edgeL[[i]]$edges, new.nbrs[[i]]))
    }
    if (!("weight" %in% names(edgeDataDefaults(myGraph)))) {
      edgeDataDefaults(myGraph, attr = "weight") <- 1L
    }
    edgeData(myGraph, from = from, to = to, attr = "weight") <- costs
  } else { ## remove edges ##
    remove <- as.data.frame(remove)
    if (ncol(remove) != 2) stop("remove does not have two columns")
    from <- as.character(remove[[1]])
    to <- as.character(remove[[2]])
    if (!all(unique(c(from, to)) %in% getNodes(x))) stop("unknown specified nodes") # unknown nodes

    ## avoid attempts to removing non-existing edges
    temp <- areNeighbours(from, to, x@graph)
    ## remove the edges (both directions for an undirected graph) and their
    ## weights; much faster than graph::removeEdge()
    from <- from[temp]
    to <- to[temp]
    myGraph <- x@graph
    if (!isDirected(myGraph)) {
      from.both <- c(from, to)
      to <- c(to, from)
      from <- from.both
    }
    removeAttrDataItem(myGraph@edgeData, x = paste(from, to, sep = "|")) <- NULL
    node.names <- nodes(myGraph)
    to.remove <- split(match(to, node.names), factor(match(from, node.names), levels = seq_along(node.names)))
    for (i in which(lengths(to.remove) > 0)) {
      k <- match(to.remove[[i]], myGraph@edgeL[[i]]$edges)
      k <- k[!is.na(k)]
      if (length(k) > 0) myGraph@edgeL[[i]]$edges <- myGraph@edgeL[[i]]$edges[-k]
    }
    }

  res <- x
  res@graph <- myGraph

  return(res)
}) # end setEdges
