#' Least-cost paths between two polygons
#'
#' @description
#' \code{polygonBetween} computes least-cost path distances between all nodes 
#' belonging to two polygons in a \code{gData} or or \code{gGraph} using \code{dijkstraBetween}. 
#' The polygons are specified via a node attribute layer. Optionally, only outline
#' nodes (nodes with at least one neighbor outside the polygon) are used to
#' reduce computation time.
#'
#' @param g A \code{gData} or \code{gGraph} object with edge costs already defined.
#' @param layer Character. Name of the node attribute layer containing polygon
#'   membership.
#' @param poly_i Character. Name of the first polygon.
#' @param poly_j Character. Name of the second polygon.
#' @param outline Logical. If \code{TRUE}, only outline nodes of each polygon
#'   (nodes with at least one neighbor outside the polygon) are used. Defaults
#'   to \code{TRUE}.
#'
#' @return A numeric vector containing the least-cost distances between nodes
#'   of the two polygons.
#'
#' @details
#' The function extracts all nodes belonging to the two specified polygons and
#' computes least-cost paths between them using \code{dijkstraBetween}.
#' If \code{outline = TRUE}, the computation is restricted to outline nodes of
#' each polygon, which can substantially reduce runtime for large polygons.
#'
#' @examples
#' \dontrun{
#' dist_vec <- polygonBetween(
#'   g = polyGraph_full,
#'   layer = "lang",
#'   poly_i = "Malay",
#'   poly_j = "Tagalog",
#'   outline = TRUE
#' )
#' }
#'
#' @importFrom geoGraph dijkstraBetween gPath2dist getNodesAttr getNodes
#' @export


polygonBetween <- function(g, layer, poly_i, poly_j, outline = TRUE) {
  
  #check if g is a gGraph or gData object
  if (!inherits(g, "gGraph") && !inherits(g, "gData")) {
    stop("Input g must be a gGraph or gData object.")
  }
  
  # get node attributes
  node_attr <- geoGraph::getNodesAttr(g)[[layer]]
  node_ids  <- geoGraph::getNodes(g)
  
  # extract nodes belonging to each polygon
  nodes_i <- node_ids[node_attr == poly_i]
  nodes_j <- node_ids[node_attr == poly_j]
  
  if (length(nodes_i) == 0 || length(nodes_j) == 0) {
    stop("One or both polygons contain no nodes.")
  }
  
  # neighbor list
  neigh_list <- g@graph@edgeL
  
  # function to compute outline nodes
  polygon_outline_nodes <- function(nodes_polygon) {
    
    outline_nodes <- c()
    
    for (node in nodes_polygon) {
      neighbors <- neigh_list[[node]]$edges
      
      if (!all(neighbors %in% nodes_polygon)) {
        outline_nodes <- c(outline_nodes, node)
      }
    }
    
    outline_nodes
  }
  
  # optionally restrict to outline nodes
  if (outline) {
    nodes_i <- polygon_outline_nodes(nodes_i)
    nodes_j <- polygon_outline_nodes(nodes_j)
    
    if (length(nodes_i) == 0 || length(nodes_j) == 0) {
      return(NA)
    }
  }
  
  # compute least-cost paths
  paths <- geoGraph::dijkstraBetween(g, from = nodes_i, to = nodes_j)
  return(paths)
}
