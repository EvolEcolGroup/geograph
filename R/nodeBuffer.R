#' node buffer
#'
#' This function identifies all nodes that are reachable from a given
#' origin node within a specified maximum distance, where the distance
#' is defined as the cumulative least-cost path over weighted
#' edges (e.g. topographic cost).
#'
#' Internally, the function computes single-source shortest paths
#' (Dijkstra algorithm) from the origin node and returns all nodes
#' whose minimum path cost does not exceed the specified threshold.
#'
#' @param graph An \code{igraph} object representing the spatial graph.
#' @param origin Either a character string naming a node, or a numeric
#'   vector / list / data.frame of length 2 giving longitude and latitude.
#' @param max.distance Numeric. Maximum cumulative cost the feature is
#'   assumed to be able to diffuse.
#' @param map.distances Logical. If \code{TRUE}, return the Graph object with new
#'   a node attribute called 'difusion_area' indicating the diffusion area
#'   with TRUE for all nodes reachable in order to map it on the gGraph object.
#'   If \code{FALSE} (default), return only the vector of node IDs within
#'   the diffusion area.
#' @return
#' If \code{map.distances = FALSE}, a character vector of node IDs
#' reachable within \code{max.distance}.
#'
#' If \code{map.distances = TRUE}, the input \code{gGraph} object with
#' an added logical node attribute \code{diffusion_area}.
#'
#' @seealso \code{\link{dijkstraFrom}}, \code{\link{gPath2dist}}
#
#' @export
nodeBuffer <- function(graph,
                       origin,
                       max.distance,
                       map.distances = TRUE) {
  ## checks
  if (!is.gGraph(graph)) {
    stop("`graph` must be a valid gGraph object.")
  }

  ## check connectivity ##
  if (!areConnected(graph, getNodes(graph))) stop("Not all nodes are connected by the graph.")


  if (is.character(origin)) {
    if (!origin %in% getNodes(graph)) {
      stop("`origin` is not a node in `graph`.")
    }
    origin.node <- origin
  } else {
    ## assume spatial input → closest node
    origin.node <- closestNode(graph, loc = origin)

    if (length(origin.node) != 1) {
      stop("Could not resolve a unique origin node from `origin`.")
    }


    ## compute least-cost paths from origin
    paths <- dijkstraFrom(graph, start = origin.node)

    ## extract distances (named vector)
    dists <- gPath2dist(paths, res.type = "vector")

    ## associate distances with destination nodes
    dest.nodes <- sub(".*:", "", names(dists))
    names(dists) <- dest.nodes

    ## identify diffusion area
    in.area <- names(dists)[dists <= max.distance]

    if (!map.distances) {
      return(in.area)
    }

    ## map back onto graph as node attribute
    diffusion.flag <- getNodes(graph) %in% in.area
    names(diffusion.flag) <- getNodes(graph)

    graph@nodes.attr$diffusion_area <- diffusion.flag

    return(graph)
  }
}
