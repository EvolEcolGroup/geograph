#' Find nodes reachable within a cost threshold
#'
#' The function `dijkstraBuffer` identifies all nodes reachable from a given
#' origin within a specified maximum cumulative cost, where the distance is
#' defined as the cumulative least-cost path over weighted edges.
#'
#' Internally, the function computes single-source shortest paths using
#' Dijkstra's algorithm from the origin node and returns all nodes whose
#' minimum path cost does not exceed the specified threshold.
#'
#' @param x a valid [`gGraph`] object with edge costs defined (see
#'   [`setCosts`]).
#' @param origin either a character string naming a node in `x`, or a
#'   `data.frame`, list, or numeric vector of length 2 giving longitude and
#'   latitude of the origin location.
#' @param d numeric. Maximum cumulative cost defining the reachable area.
#' @param res.type a character string indicating the output format:
#'   - `"nodes"`: a character vector of reachable node names (default).
#'   - `"gGraph"`: the input [`gGraph`] with a new logical node attribute
#'     `reachable` indicating which nodes fall within the cost threshold.
#' @param ... further arguments passed to other methods (currently unused).
#' @return A character vector of reachable node names when
#'   `res.type = "nodes"`, or a [`gGraph`] object with a new logical node
#'   attribute `reachable` when `res.type = "gGraph"`.
#' @seealso [`dijkstraFrom`] and [`gPath2dist`] for the underlying path
#'   computations. [`buffer`] for geographic distance-based buffers.
#'   [`setCosts`] to define edge costs before running `dijkstraBuffer`.
#' @family dijkstra_methods
#' @examples
#' ## zoom in to an area
#' plot(rawgraph.10k, reset = TRUE)
#' geo.zoomin(list(x = c(-6, 38), y = c(35, 73)))
#' x <- rawgraph.10k[isInArea(rawgraph.10k)]
#' ## get all nodes reachable within cost 10 from node the origin (here Zurich)
#' zurich <- data.frame(lon = 8.55, lat = 47.37)
#' ## set costs for the graph and calculate the buffer
#' graph <- setCosts(x, attr.name = "habitat", method = "mean")
#' x2 <- dijkstraBuffer(graph, origin = zurich, d = 10, res.type = "gGraph")
#' 
#' ## plot reachable nodes in dark blue, all others transparent
#' col.rules <- data.frame(
#'   reachable = c(TRUE, FALSE),
#'   color     = c("darkblue", "transparent")
#' )
#' plot(x2, col.rules = col.rules, reset = TRUE)
#' 
#' @export
dijkstraBuffer <- function(x, origin, d, res.type = c("nodes", "gGraph"), ...) {
  res.type <- match.arg(res.type)
  
  ## checks
  if (!is.gGraph(x)) stop("x must be a valid gGraph object.")
  if (!is.numeric(d) || length(d) != 1 || d <= 0) {
    stop("d must be a single positive number.")
  }
  
  ## resolve origin node
  if (is.character(origin)) {
    if (length(origin) != 1L) stop("origin must be a single node name.")
    if (!origin %in% getNodes(x)) stop("origin is not a node in x.")
    origin.node <- origin
  } else {
    origin.node <- closestNode(x, loc = origin)
    if (length(origin.node) != 1) {
      stop("Could not resolve a unique origin node from origin.")
    }
  }
  
  ## compute least-cost paths from origin
  paths <- dijkstraFrom(x, start = origin.node)
  dists <- gPath2dist(paths, res.type = "vector")
  
  ## extract destination node names
  dest.nodes   <- sub(".*:", "", names(dists))
  names(dists) <- dest.nodes
  in.reach     <- names(dists)[dists <= d]
  
  if (res.type == "nodes") {
    return(in.reach)
  }
  
  ## map back onto graph as node attribute
  reachable              <- getNodes(x) %in% in.reach
  names(reachable)       <- getNodes(x)
  x@nodes.attr$reachable <- reachable
  
  return(x)
}