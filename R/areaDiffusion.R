#' area diffusion model (toy function)
#' 
#' This function identifies all nodes that are reachable from a given
#' origin node within a specified maximum diffusion distance, where
#' distance is defined as the cumulative least-cost path over weighted
#' edges (e.g. topographic cost).
#'
#' Internally, the function computes single-source shortest paths
#' (Dijkstra algorithm) from the origin node and returns all nodes
#' whose minimum path cost does not exceed the specified threshold.
#'
#' @param graph An \code{igraph} object representing the spatial graph.
#' @param origin Either a character string naming a node, or a numeric
#'   vector / list / data.frame of length 2 giving longitude and latitude.
#' @param max_distance Numeric. Maximum cumulative cost the feature is
#'   assumed to be able to diffuse.
#' @param weight_attr Character. Name of the edge attribute containing
#'   movement costs. Defaults to \code{"weight"}.
#' @param map_distances Logical. If \code{TRUE}, return the Grpah object with new 
#'   a node attribute called 'difusion_area' indicating the diffusion area
#'   with TRUE for all nodes reachable in order to map it on the gGraph object. 
#'   If \code{FALSE} (default), return only the vector of node IDs within 
#'   the diffusion area.
#' @return
#' If \code{map_distances = FALSE}, a character vector of node IDs
#' reachable within \code{max_distance}.
#'
#' If \code{map_distances = TRUE}, the input \code{gGraph} object with
#' an added logical node attribute \code{diffusion_area}.
#'
#' @seealso \code{\link{dijkstraFrom}}, \code{\link{gPath2dist}}
#
#' @export
arealDiffusion <- function(graph,
                           origin,
                           max_distance,
                           map_distances = TRUE) {
  
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
    origin_node <- origin
    
  } else {
    
    ## assume spatial input → closest node
    origin_node <- closestNode(graph, loc = origin)
    
    if (length(origin_node) != 1) {
      stop("Could not resolve a unique origin node from `origin`.")
    }
  
  
  ## compute least-cost paths from origin
  paths <- dijkstraFrom(graph, start = origin_node)
  
  ## extract distances (named vector)
  dists <- gPath2dist(paths, res.type = "vector")
  
  ## associate distances with destination nodes
  dest_nodes <- sub(".*:", "", names(dists))
  names(dists) <- dest_nodes
  
  ## identify diffusion area
  in_area <- names(dists)[dists <= max_distance]
  
  if (!map_distances) {
    return(in_area)
  }
  
  ## map back onto graph as node attribute
  diffusion_flag <- getNodes(graph) %in% in_area
  names(diffusion_flag) <- getNodes(graph)
  
  graph@nodes.attr$diffusion_area <- diffusion_flag
  
  return(graph)
  }
}
