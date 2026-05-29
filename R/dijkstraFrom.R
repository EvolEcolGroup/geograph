#' @title Find the minimum cost path
#' @description This function finds the shortest path from a given 'source' node
#'    to all other nodes in the graph using Dijkstra's algorithm.
#'    It can be applied to both gGraph and gData objects.
#' @param x A [`gGraph`] or [`gData`] object.
#' @param start a character string naming the 'source' node.
#' @return A [`gPath`] object.
#' @details The function uses the RBGL package to compute the shortest paths. It
#'   checks for the connectivity of the graph and handles cases where there are
#'   duplicated paths.
#' @examples
#' # Using a gData object:
#'
#' # select a few populations from the HGDP dataset
#' hgdp.sub <- hgdp[getData(hgdp)$Population %in%
#'   c("Orcadian", "Adygei", "Russian", "Basque")]
#'
#' # select a location of another HGDP population
#' french.hgdp <- hgdp[getData(hgdp)$Population %in%
#'   c("French")]
#'
#' # Choose an origin node
#' french <- getNodes(french.hgdp)
#'
#' my.path <- dijkstraFrom(hgdp.sub, french)
#'
#' @family dijkstra_methods
#' @export
setGeneric("dijkstraFrom", function(x, start) {
  standardGeneric("dijkstraFrom")
}, signature = c("x"))


#####################
## method for gGraph
#####################
#' @describeIn dijkstraFrom method for gGraph
#' @export
setMethod("dijkstraFrom", "gGraph", function(x, start) {
  ## some checks ##
  if (!is.gGraph(x)) stop("x is not a valid gGraph object")
  if (!all(start %in% getNodes(x))) stop("Starting node is not in x.")

  ## check connectivity ##
  if (!areConnected(x, getNodes(x))) stop("Not all nodes are connected by the graph.")

  ## build the wrapper ##
  myGraph <- getGraph(x)

  endNodes <- getNodes(x)[!getNodes(x) %in% start]

  res <- RBGL::sp.between(myGraph,
    start = start,
    finish = endNodes
  )

  ## sp.between uses unique(x@nodes.id) ##
  ## eventually have to duplicate paths ##
  temp <- gsub(".*:", "", names(res))
  res <- res[match(endNodes, temp)]


  ## make it a class "gPath" (output + xy coords) ##
  allNodes <- unique(unlist(lapply(res, function(e) e$path_detail)))
  ## res$xy <- getCoords(x)[allNodes,]
  attr(res, "xy") <- getCoords(x)[allNodes, ]
  class(res) <- "gPath"

  return(res)
}) # end dijkstraFrom for gGraph


####################
## method for gData
####################
#' @describeIn dijkstraFrom method for gData
#' @export
setMethod("dijkstraFrom", "gData", function(x, start) {
  ## some checks ##
  if (!is.gData(x)) stop("x is not a valid gData object")
  if (!exists(x@gGraph.name, envir = .GlobalEnv)) stop(paste("gGraph object", x@gGraph.name, "not found."))
  if (length(x@nodes.id) == 0) stop("No assigned nodes (x@nodes.id is empty).")
  if (!isConnected(x)) stop("Not all locations are connected by the graph")


  ## build the wrapper ##
  myGraph <- get(x@gGraph.name, envir = .GlobalEnv) # myGraph is a gGraph object
  coords <- getCoords(myGraph) # store xy for later
  myGraph <- getGraph(myGraph)

  ## wrap ##
  res <- RBGL::sp.between(myGraph, start = start, finish = x@nodes.id)


  ## sp.between uses unique(x@nodes.id) ##
  ## eventually have to duplicate paths ##
  temp <- gsub(".*:", "", names(res))
  res <- res[match(getNodes(x), temp)]


  ## make it a class "gPath" (output + xy coords) ##
  allNodes <- unique(unlist(lapply(res, function(e) e$path_detail)))
  ## res$xy <- getCoords(x)[allNodes,]
  attr(res, "xy") <- coords[allNodes, ]
  class(res) <- "gPath"
  return(res)
}) # end dijkstraFrom for gData
