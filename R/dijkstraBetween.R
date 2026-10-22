#' @title Find the shortest path between nodes in a graph
#' @description This function finds the shortest path between nodes in a graph
#'   using Dijkstra's algorithm. It can be applied to both gGraph and gData
#'   objects.
#' @param x A [`gGraph`] or [`gData`] object.
#' @param from A character vector of starting node IDs.
#' @param to A character vector of ending node IDs. If missing, or identical
#'   to `from`, paths are computed between all pairs of nodes in `from`.
#' @param ... Additional arguments passed to other methods (currently not used).
#' @return A [`gPath`] object:
#'   - if `to` is missing or identical to `from` (and has more than one
#'     node), one path for each pair of nodes `from[i]`, `from[j]` with
#'     `i < j`, in the order used by [`dist`] objects; this is always the case
#'     for the [`gData`] method, which uses all locations of `x`;
#'   - otherwise, one path from each node of `from` to each node of `to`
#'     (`length(from) * length(to)` paths, `from` varying slowest).
#'
#'   [`gPath2dist`] turns these into a [`dist`] object or a
#'   `length(from) x length(to)` matrix, respectively.
#' @details The function uses the RBGL package to compute the shortest paths. It
#'   checks for the connectivity of the graph and handles cases where there are
#'   duplicated paths.
#' @examples
#' ## select a few populations from the HGDP dataset
#' hgdp.sub <- hgdp[getData(hgdp)$Population %in%
#'   c("French", "Balochi", "BantuKenya", "Papuan", "Pima")]
#' hgdp.path <- dijkstraBetween(hgdp.sub) # compute shortest path
#' @family dijkstra_methods
#' @export
setGeneric("dijkstraBetween", function(x, ...) {
  standardGeneric("dijkstraBetween")
})

#####################
## method for gGraph
#####################
#' @export
#' @describeIn dijkstraBetween Method for gGraph
setMethod("dijkstraBetween", "gGraph", function(x, from, to) {
  ## some checks ##
  if (!is.gGraph(x)) stop("x is not a valid gGraph object")
  if (missing(to)) {
    to <- from
  }

  ## check for empty inputs first
  if (length(from) == 0 || length(to) == 0) {
    stop("`from` and `to` must be non-empty.")
  }

  if (!all(from %in% getNodes(x))) stop("Some starting nodes are not in x.")
  if (!all(to %in% getNodes(x))) stop("Some ending nodes are not in x.")

  ## check connectivity ##
  if (!areConnected(x, unique(c(from, to)))) stop("Not all nodes are connected by the graph.")

  myGraph <- getGraph(x)

  ## build indices of the pairs of nodes to connect ##
  if (identical(from, to) && length(from) > 1) {
    ## all pairs i < j of a single set of nodes, in the order of a 'dist'
    type <- "pairwise"
    pairs <- utils::combn(length(from), 2)
    start <- from[pairs[1, ]]
    finish <- from[pairs[2, ]]
  } else {
    ## each node of 'from' to each node of 'to'
    type <- "cross"
    start <- rep(from, each = length(to))
    finish <- rep(to, times = length(from))
  }

  ## wrap ##
  ## ! sp.between does not return duplicated paths, and groups them by start:
  ## put them back in the requested order, duplicating them if needed
  res <- RBGL::sp.between(myGraph, start = start, finish = finish)
  res <- res[paste(start, finish, sep = ":")]


  ## make it a class "gPath" (output + xy coords) ##
  allNodes <- unique(unlist(lapply(res, function(e) e$path_detail)))
  attr(res, "xy") <- getCoords(x)[allNodes, ]
  attr(res, "type") <- type
  if (type == "cross") {
    attr(res, "nodes") <- list(from = from, to = to)
  }
  class(res) <- "gPath"

  return(res)
}) # end dijkstraBetween for gGraph


#####################
## method for gData
#####################
#' @describeIn dijkstraBetween Method for gData
#' @export
setMethod("dijkstraBetween", "gData", function(x) {
  # we transform the gData object to gGraph, extracting the nodes from the gData object.
  # The node ids are found in the @nodes.id of the gData object: in this case we
  # can call  getNodes().
  # Then simply pass the new gGraph object to the method for gGraph.

  ## some checks ##
  if (!is.gData(x)) stop("x is not a valid gData object")
  if (!exists(x@gGraph.name, envir = .GlobalEnv)) stop(paste("gGraph object", x@gGraph.name, "not found."))
  if (length(x@nodes.id) == 0) stop("No assigned nodes (x@nodes.id is empty).")

  ## build the wrapper ##
  # @TODO check labels to keep
  myGraph <- get(x@gGraph.name, envir = .GlobalEnv)
  myNodes <- getNodes(x)
  dijkstraBetween(myGraph, from = myNodes, to = myNodes)
}) # end dijkstraBetween for gData
