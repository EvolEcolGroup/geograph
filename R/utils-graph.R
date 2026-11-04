## Internal helpers to work on graphNEL objects in integer space.
##
## The graph package stores a graphNEL as a list of neighbours per node
## (@edgeL) plus an environment-like list of edge attributes keyed by
## "from|to" (@edgeData@data). Working on node names (match(), name subsetting,
## subGraph(), the nested edge list form of new("graphNEL")) is quadratic or
## writes every weight twice; these helpers work on integer node positions
## instead.

#' Flat edge table of a graphNEL
#'
#' One element per *directed* edge (an undirected edge appears twice), in the
#' order of `g@edgeL`, which is also the order of `unlist(edgeWeights(g))` and
#' of `edgeMatrix(g, duplicates = TRUE)`.
#'
#' @param g a `graphNEL`.
#' @param weights logical; if `TRUE`, also return the edge weights.
#' @return a list with integer vectors `from` and `to` (node positions) and,
#'   if `weights = TRUE`, a numeric vector `w`.
#' @noRd
.edgeTable <- function(g, weights = TRUE) {
  el <- g@edgeL
  to <- lapply(el, `[[`, "edges")
  res <- list(
    from = rep.int(seq_along(el), lengths(to)),
    to = as.integer(unlist(to, use.names = FALSE))
  )
  if (weights) {
    w <- unlist(graph::edgeWeights(g), use.names = FALSE)
    res$w <- if (is.null(w)) numeric(0) else w
  }
  res
}


#' Build a graphNEL from a flat edge table
#'
#' The edge list is given to the `graphNEL` constructor as a list of character
#' vectors: with the nested `list(edges = , weights = )` form, `graph`
#' initialises every weight through `edgeData<-`, twice per undirected edge.
#'
#' @param nodes character vector of node names.
#' @param from,to integer node positions, one element per *directed* edge (for
#'   an undirected graph, each edge must appear in both directions, as returned
#'   by `.edgeTable()`).
#' @param w numeric weights, one per directed edge, or `NULL`. If `NULL`, the
#'   graph has no stored edge data (as `subGraph()` of an unweighted graph).
#'   Otherwise each directed edge `from -> to` gets exactly its own weight: the
#'   graph is switched to "directed" while assigning, so that `edgeData<-`
#'   writes the key `from|to` only (in undirected mode, it also writes `to|from`,
#'   which doubles the work and overwrites the weight of the reverse edge).
#' @param edgemode "undirected" or "directed".
#' @return a `graphNEL`.
#' @noRd
.graphFromTable <- function(nodes, from, to, w = NULL, edgemode = "undirected") {
  el <- split(nodes[to], factor(from, levels = seq_along(nodes)))
  names(el) <- nodes
  g <- new("graphNEL", nodes = nodes, edgeL = el, edgemode = edgemode)
  if (!is.null(w)) {
    graph::edgeDataDefaults(g, "weight") <- 1
    if (length(from) > 0) {
      ## set the slot directly: graph::edgemode<- also resets edgeRenderInfo
      g@graphData$edgemode <- "directed"
      graph::edgeData(g, from = nodes[from], to = nodes[to], attr = "weight") <- w
      g@graphData$edgemode <- edgemode
    }
  }
  g
}

