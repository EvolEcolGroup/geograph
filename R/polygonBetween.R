#' @title Least-cost paths between two polygons
#'
#' @description This function computes least-cost path distances between all nodes
#' belonging to two polygons in a [`gGraph`] using `dijkstraBetween.`
#' The polygons are specified via a node attribute layer. Optionally, only outline
#' nodes (nodes with at least one neighbor outside the polygon) are used to
#' reduce computation time.
#'
#' @param g [`gGraph`] object with edge costs already defined.
#' @param layer Character. Name of the node attribute layer containing polygon
#'   membership.
#' @param from Character. Name of the first polygon.
#' @param to Character. Name of the second polygon.
#' @param outline Logical. If `TRUE`, only outline nodes of each polygon
#'   (nodes with at least one neighbor outside the polygon) are used. Defaults
#'   to `TRUE`.
#'
#' @return A [`gPath`] object with the least-cost paths from each node of the
#'   first polygon to each node of the second (see [`dijkstraBetween`]); use
#'   [`gPath2dist`] to obtain the matrix of their costs. `NA` if `outline =
#'   TRUE` and one of the polygons has no outline node.
#'
#' @details
#' The function extracts all nodes belonging to the two specified polygons and
#' computes least-cost paths between them using `dijkstraBetween.`
#' If `outline = TRUE`, the computation is restricted to outline nodes of
#' each polygon, which can substantially reduce runtime for large polygons.
#'
#' @examples
#' world.countries <- rnaturalearth::ne_countries(
#'   scale = "medium",
#'   returnclass = "sf"
#' )
#' newGraph <- assignByPolygon(rawgraph.10k,
#'   layer = world.countries,
#'   attr = c("continent", "name")
#' )
#' test <- polygonBetween(newGraph, layer = "name", "Spain", "Germany", outline = TRUE)
#' plot(newGraph, col = NA, reset = TRUE)
#' plot(test, col = "red")
#'
#' @export


polygonBetween <- function(g, layer, from, to, outline = TRUE) {
  if (!inherits(g, "gGraph")) {
    stop("g must be a gGraph object; for a gData, use dijkstraBetween() on its gGraph.")
  }
  
  if (!layer %in% colnames(getNodesAttr(g))) {
    stop(sprintf("Layer '%s' not found in node attributes.", layer))
  }
  
  # get node attributes
  node.attr <- getNodesAttr(g)[[layer]]
  node.ids <- getNodes(g)
  
  # extract nodes belonging to each polygon
  nodes.from <- node.ids[node.attr %in% from]
  nodes.to <- node.ids[node.attr %in% to]
  
  if (length(nodes.from) == 0 || length(nodes.to) == 0) {
    stop("One or both polygons contain no nodes.")
  }
  
  # neighbour list, as node names (@edgeL stores positions, which differ
  # from node names once a graph has been subset)
  neighL <- graph::edges(getGraph(g))
  
  # function to compute outline nodes
  polygonOutlineNodes <- function(nodes.polygon) {
    is.outline <- vapply(nodes.polygon, function(node) {
      !all(neighL[[node]] %in% nodes.polygon)
    }, logical(1))
    nodes.polygon[is.outline]
  }
  
  # optionally restrict to outline nodes
  if (outline) {
    nodes.from <- polygonOutlineNodes(nodes.from)
    nodes.to <- polygonOutlineNodes(nodes.to)
    
    if (length(nodes.from) == 0 || length(nodes.to) == 0) {
      return(NA)
    }
  }
  
  # compute least-cost paths
  paths <- dijkstraBetween(g, from = nodes.from, to = nodes.to)
  return(paths)
}