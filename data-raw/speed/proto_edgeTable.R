## Prototype internal helpers for working on graphNEL objects in integer space.
## See data-raw/speed_plan.md, item P0. Verified on worldgraph.40k / rawgraph.40k
## (identical nodes, edges() and edgeWeights() to the current code paths).
## NOT package code yet: copy into R/ (e.g. R/utils-graph.R) with roxygen @noRd.

## Flat edge table of a graphNEL: one row per *directed* edge (an undirected
## edge appears twice), in the order of @edgeL, which is also the order of
## unlist(edgeWeights(g)) and of edgeMatrix(g, duplicates = TRUE).
## from/to are integer node positions.
.edgeTable <- function(g, weights = TRUE) {
  el <- g@edgeL
  to <- lapply(el, `[[`, "edges")
  n <- lengths(to)
  res <- list(
    from = rep.int(seq_along(el), n),
    to = as.integer(unlist(to, use.names = FALSE))
  )
  if (weights) res$w <- unlist(graph::edgeWeights(g), use.names = FALSE)
  res
}

## Build a graphNEL from a flat directed edge table (as returned by .edgeTable).
## - edgeL is given as character vectors, so graph does NOT initialise default
##   weights (the nested list(edges = , weights = ) form makes graph call
##   edgeData<- on every edge, twice for undirected graphs: ~2x slower);
## - if w is NULL, the graph has no stored edge data (like subGraph() of an
##   unweighted graph, or new("graphNEL", edgeL = <character list>));
## - otherwise each directed edge gets exactly its own weight: the graph is
##   switched to "directed" while assigning, so edgeData<- writes from|to only.
##   With the undirected mode, edgeData<- also writes to|from, which is what
##   produces the swapped weights of setCosts(method = "function") today.
.graphFromTable <- function(nodes, from, to, w = NULL, edgemode = "undirected") {
  f <- factor(from, levels = seq_along(nodes))
  el <- lapply(split(to, f), function(e) nodes[e])
  names(el) <- nodes
  g <- new("graphNEL", nodes = nodes, edgeL = el, edgemode = edgemode)
  if (!is.null(w)) {
    graph::edgeDataDefaults(g, "weight") <- 1
    if (length(from) > 0) {
      g@graphData$edgemode <- "directed" # try graph::edgemode<- first, see plan
      graph::edgeData(g, from = nodes[from], to = nodes[to], attr = "weight") <- w
      g@graphData$edgemode <- edgemode
    }
  }
  g
}

## ---- examples of use (cores of the rewritten functions) -----------------------

setCosts_core <- function(g, nodeCosts, method = c("mean", "product")) {
  method <- match.arg(method)
  et <- .edgeTable(g, weights = FALSE)
  w <- switch(method,
    mean = (nodeCosts[et$from] + nodeCosts[et$to]) / 2,
    product = nodeCosts[et$from] * nodeCosts[et$to]
  )
  .graphFromTable(graph::nodes(g), et$from, et$to, w)
}

dropDeadEdges_core <- function(g, thres) {
  et <- .edgeTable(g)
  keep <- et$w <= thres
  .graphFromTable(graph::nodes(g), et$from[keep], et$to[keep], et$w[keep])
}

## subset a graphNEL to nodes 'idx' (integer or logical), replacing
## graph::subGraph(), which is O(kept x dropped nodes)
.subsetGraph <- function(g, idx) {
  nN <- graph::nodes(g)
  N <- length(nN)
  if (is.logical(idx)) idx <- which(rep_len(idx, N))
  newpos <- integer(N)
  newpos[idx] <- seq_along(idx)
  weighted <- length(g@edgeData@data) > 0
  et <- .edgeTable(g, weights = weighted)
  keep <- newpos[et$from] > 0L & newpos[et$to] > 0L
  .graphFromTable(nN[idx], newpos[et$from[keep]], newpos[et$to[keep]],
    if (weighted) et$w[keep] else NULL,
    edgemode = graph::edgemode(g)
  )
}

## areNeighbours, vectorised: numeric keys (from - 1) * N + to are exact
## below 2^53, i.e. for any realistic graph
areNeighbours_core <- function(V1, V2, g) {
  nN <- graph::nodes(g)
  N <- length(nN)
  et <- .edgeTable(g, weights = FALSE)
  i1 <- match(V1, nN)
  i2 <- match(V2, nN)
  res <- ((i1 - 1) * N + i2) %in% ((et$from - 1) * N + et$to)
  names(res) <- paste(V1, V2, sep = "->")
  res
}
