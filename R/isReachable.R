#' Tests if location reachable from nodes
#'
#' Tests if one location (actually, the closest node to
#' it) is reachable from the set of nodes of a [gData] object.
#'
#' @param x a [gData] object.
#' @param loc location, specified as a list of two components giving
#' respectively the longitude and the latitude. Alternatively, it can be a
#' matrix-like object with one row and two columns.
#' @return a named boolean vector, with one element per node of the input
#' [gData] object, and names corresponding to node names.
#' @export
#' @examples
#' # Select African populations Mandenka, Yoruba, and San
#' hgdp.sub <- hgdp[getData(hgdp)$Population %in%
#'   c("Mandenka", "Yoruba", "San")]
#' # Get a location that is reachable
#' location <- getCoords(hgdp[getData(hgdp)$Population == "BantuKenya"])
#' # Check these are reachable
#' isReachable(x = hgdp.sub, loc = location)
#' @family connectivity_functions

isReachable <- function(x, loc) { # x is a gData object
  ## checks ##
  if (!is.gData(x)) stop("x is not a valid gData object.")

  mygGraph <- get(x@gGraph.name, envir = .GlobalEnv)


  ## get connected sets (with more than one node) ##
  connected.sets <- .connectedSets(getGraph(x))


  ## check which set contains refNode ##
  refNode <- closestNode(mygGraph, loc)
  temp <- vapply(connected.sets, function(e) refNode %in% e, logical(1))
  if (!any(temp)) {
    warning("The reference node is not connected to any node.")
    return(FALSE)
  }
  refSet <- connected.sets[[which(temp)]]

  ## check reachability for each node ##
  myNodes <- getNodes(x)
  res <- myNodes %in% refSet
  names(res) <- myNodes

  ## return res ##
  return(res)
}
