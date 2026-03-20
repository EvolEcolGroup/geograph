#' Test if a set of nodes form a connected set
#'
#' This function tests if a set of nodes form a connected set on a
#' [`gData`] or [`gGraph`] object.
#'
#' @details
#' This function is a method for both [gData] and [gGraph] objects.
#' For a [gData] object, it tests if all the nodes in the object form a
#' connected set on the associated [gGraph] object. For a [gGraph]
#' object, it tests if all nodes in the graph form a connected set.
#' Use [areConnected()] if you want to test if a specific subset
#' of nodes form a connected set.
#' @param object a [gData] or [gGraph] object
#' @param \dots other arguments passed to other methods.
#' @return a single logical value, being TRUE if nodes form a connected set.
#' @family connectivity_functions
#' @examples
#' # in the world graph, there are nodes that are not connected
#' isConnected(worldgraph.10k)
#' # all the nodes in the hgdp gData object are connected
#' isConnected(hgdp)
#' @export
#' @importFrom graph isConnected
#' @rdname isConnected
setMethod("isConnected", "gData", function(object, ...) {
  ## checks ##
  x <- object
  if (!is.gData(x)) stop("'object' is not a valid gData object.")
  if (!exists(x@gGraph.name, envir = .GlobalEnv)) stop(paste("gGraph object", x@gGraph.name, "not found."))


  ## set args for areConnected ##
  myGraph <- get(x@gGraph.name, envir = .GlobalEnv)
  myNodes <- getNodes(x)

  ## wrapper ##
  res <- areConnected(myGraph, myNodes)

  ## return res ##
  return(res)
}) # end isConnected for gData


## the GENERIC of this method is given in package 'graph'
#' @rdname isConnected
#' @export
setMethod("isConnected", "gGraph", function(object, ...) {
  ## checks ##
  if (!is.gGraph(object)) stop("'object' is not a valid gGraph object.")

  ## set args for areConnected ##
  myNodes <- getNodes(object)
  ## wrapper ##
  res <- areConnected(object, myNodes)

  ## return res ##
  return(res)
}) # end isConnected for gGraph

