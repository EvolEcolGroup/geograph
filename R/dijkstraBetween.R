#' @title Find the shortest path between nodes in a graph
#' @description This function finds the shortest path between nodes in a graph
#'   using Dijkstra's algorithm. It can be applied to both gGraph and gData
#'   objects.
#' @param x A [`gGraph`] or [`gData`] object.
#' @param from A character vector of starting node IDs.
#' @param to A character vector of ending node IDs.
#' @param ... Additional arguments passed to other methods (currently not used).
#' @return A [`gPath`] object.
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

  ## check for empty inputs first
  if (length(from) == 0 || length(to) == 0) {
    stop("`from` and `to` must be non-empty.")
  }

  if (!all(from %in% getNodes(x))) stop("Some starting nodes are not in x.")
  if (!all(to %in% getNodes(x))) stop("Some ending nodes are not in x.")

  ## check connectivity ##
  if (!areConnected(x, unique(c(from, to)))) stop("Not all nodes are connected by the graph.")

  ## build the wrapper ##
  myGraph <- getGraph(x)
  ## recycle from and to
  maxLength <- max(length(from), length(to))
  from <- rep(from, length = maxLength)
  to <- rep(to, length = maxLength)

  ## build indices of all pairwise combinations ##
  if (maxLength > 1) {
    pairIdStart <- integer()
    pairIdStop <- integer()

    for (i in seq_len(maxLength)) {
      j <- i
      while ((j <- j + 1) < (maxLength + 1)) {
        pairIdStart <- c(pairIdStart, i)
        pairIdStop <- c(pairIdStop, j)
      }
    }
  } else {
    pairIdStart <- pairIdStop <- 1
  }

  ## wrap ##
  ## ! sp.between does not return duplicated paths
  res <- RBGL::sp.between(myGraph, start = from[pairIdStart], finish = to[pairIdStop])


  ## handle duplicated paths ##
  if (length(res) < length(pairIdStart)) { # res should have all requested combinations
    fromTo <- paste(from[pairIdStart], to[pairIdStop], sep = ":") # all different paths
    res <- res[fromTo]
  }


  ## make it a class "gPath" (output + xy coords) ##
  allNodes <- unique(unlist(lapply(res, function(e) e$path_detail)))
  attr(res, "xy") <- getCoords(x)[allNodes, ]
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
