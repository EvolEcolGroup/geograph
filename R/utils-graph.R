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
      graph::edgeData(g, from = nodes[from], to = nodes[to], attr = "weight") <- unname(w)
      g@graphData$edgemode <- edgemode
    }
  }
  g
}




#' Weights of the reverse edges
#'
#' For each directed edge `from -> to` of an undirected graph, the weight of
#' `to -> from`. Building an undirected `graphNEL` from a nested edge list
#' (`list(edges = , weights = )`), as `setCosts()`, `dropDeadEdges()` and
#' `combineCosts()` used to do, stores for each edge the weight given for its
#' reverse; this reproduces it. It is the identity for symmetric weights, and
#' only matters for asymmetric ones (e.g. `setCosts(method = "function")` with
#' an asymmetric `FUN`).
#'
#' @param from,to,w an edge table, as returned by `.edgeTable()`.
#' @param n the number of nodes.
#' @return the weights of the reverse edges, in table order.
#' @noRd
.reverseWeights <- function(from, to, w, n) {
  w[match((to - 1) * n + from, (from - 1) * n + to)]
}


#' Weights as stored by the nested graphNEL constructor
#'
#' `new("graphNEL", edgeL = <list(edges = , weights = )>)` concatenates the
#' weights of all nodes with `unlist()`, nodes without edges contributing
#' `numeric(0)`, and refuses non-numeric weights. Functions that used to build
#' their graph this way use this to keep the type of the stored weights.
#'
#' @param w numeric weights, one per directed edge.
#' @param from the node positions of the edges.
#' @param n the number of nodes.
#' @return `w`, as double if it is integer and some node has no edge.
#' @noRd
.nestedWeights <- function(w, from, n) {
  if (!is.numeric(w)) stop("weights in edgeL must be numeric")
  if (is.integer(w) && length(unique(from)) < n) w <- as.double(w)
  w
}

#' Subset a graphNEL to some of its nodes
#'
#' Replacement for `graph::subGraph()`, which is O(kept x dropped nodes). Edges
#' between kept nodes are kept with their weights, if the graph has stored edge
#' data, and node data are carried over as in `subGraph()`. The nodes of the
#' result are in the order of `idx`.
#'
#' @param g a `graphNEL`.
#' @param idx integer positions (without duplicates or `NA`) of the nodes to
#'   keep.
#' @return a `graphNEL`.
#' @noRd
.subsetGraph <- function(g, idx) {
  nN <- graph::nodes(g)
  newpos <- integer(length(nN))
  newpos[idx] <- seq_along(idx)
  weighted <- length(g@edgeData@data) > 0
  et <- .edgeTable(g, weights = weighted)
  keep <- newpos[et$from] > 0L & newpos[et$to] > 0L
  res <- .graphFromTable(nN[idx], newpos[et$from[keep]], newpos[et$to[keep]],
    w = if (weighted) et$w[keep] else NULL,
    edgemode = graph::edgemode(g)
  )
  ## geoGraph keeps node attributes in @nodes.attr, but carry graph node data
  ## over as subGraph() does
  res@nodeData@defaults <- g@nodeData@defaults
  res@nodeData@data <- g@nodeData@data[match(nN[idx], names(g@nodeData@data), 0L)]
  res
}


#' Shortest paths between pairs of nodes
#'
#' Replacement for `RBGL::sp.between()`, which does a linear name lookup and
#' grows vectors for every path (and loops over all nodes for an unreachable
#' target). Paths from the same start are traced back from all their targets at
#' once, and step weights are looked up by numeric edge keys.
#'
#' @param g a `graphNEL` with nonnegative edge weights.
#' @param start,finish character vectors of node names, of the same length:
#'   one path is computed for each pair `start[k]`, `finish[k]`.
#' @param eW the edge weights, `unlist(edgeWeights(g), use.names = FALSE)`;
#'   computed if `NULL`.
#' @return a list with one element per pair, in the order of the pairs
#'   (duplicates kept), named `"start:finish"`, each element being the same as
#'   in `sp.between()`: `list(length, path_detail, length_detail)`. A path
#'   from a node to itself has `length = NA` and `length_detail = list(NA)`; an
#'   unreachable target has `path_detail = NA`.
#' @noRd
.shortestPaths <- function(g, start, finish, eW = NULL) {
  nN <- graph::nodes(g)
  N <- length(nN)
  if (length(start) != length(finish)) stop("start and finish must have the same length")
  s.idx <- match(start, nN)
  f.idx <- match(finish, nN)
  if (anyNA(s.idx) || anyNA(f.idx)) stop("all nodes must be in the graph")
  if (is.null(eW)) eW <- unlist(graph::edgeWeights(g), use.names = FALSE)
  if (any(eW < 0, na.rm = TRUE)) stop("edge weights must be nonnegative")
  directed <- graph::isDirected(g)
  em <- graph::edgeMatrix(g, duplicates = !directed)
  ## weight of the directed step u -> v, looked up by a numeric key (exact
  ## below 2^53, i.e. for any realistic graph)
  ekey <- (em[1, ] - 1) * N + em[2, ]

  ## node positions of each path, traced back from the targets
  P <- length(start)
  path.idx <- vector("list", P)
  for (s in unique(s.idx)) {
    pen <- RBGL::dijkstra.sp(g, nN[s], eW)$penult
    k <- which(s.idx == s)
    cur <- f.idx[k]
    steps <- list(cur)
    active <- cur != s
    while (any(active)) {
      nxt <- cur
      nxt[active] <- pen[cur[active]]
      stuck <- active & nxt == cur # unreachable: BGL sets penult[v] = v
      cur <- nxt
      active <- active & !stuck & cur != s
      steps[[length(steps) + 1L]] <- cur
    }
    M <- do.call(cbind, steps) # one row per target, walking back to s
    len <- max.col(M == s, ties.method = "first")
    reached <- M[cbind(seq_along(k), len)] == s
    for (j in seq_along(k)) {
      path.idx[[k[j]]] <- if (reached[j]) rev(M[j, seq_len(len[j])]) else NA_integer_
    }
  }

  ## weights of all steps of all paths at once
  lens <- lengths(path.idx)
  all.nodes <- unlist(path.idx, use.names = FALSE)
  last <- cumsum(lens)
  first <- last - lens + 1L
  u <- all.nodes[-last]
  v <- all.nodes[-first]
  w <- eW[match((u - 1) * N + v, ekey)]
  names(w) <- paste(nN[u], nN[v], sep = if (directed) "->" else "--")
  pid <- rep.int(seq_len(P), lens - 1L)
  ws <- split(w, factor(pid, levels = seq_len(P)))

  res <- vector("list", P)
  for (i in seq_len(P)) {
    wi <- if (lens[i] > 1L) ws[[i]] else NA
    res[[i]] <- list(
      length = sum(wi),
      path_detail = nN[path.idx[[i]]],
      length_detail = list(wi)
    )
  }
  names(res) <- paste(start, finish, sep = ":")
  res
}


#' Add edges to a graphNEL
#'
#' Same result as `graph::addEdge(from, to, g, weights)`, whose loops look up
#' the edge list of each node by name: new neighbours are appended to the edge
#' list of each `from` node, then (undirected graphs) of each `to` node,
#' without duplicates; the default weight is set to `1L` if the graph has
#' none, and the weights are assigned with `edgeData<-`.
#'
#' @param g a `graphNEL`.
#' @param from,to character vectors of node names (of the graph), of the same
#'   length.
#' @param weights numeric weights, one per edge.
#' @return a `graphNEL`.
#' @noRd
.addEdges <- function(g, from, to, weights) {
  gN <- graph::nodes(g)
  whF <- match(from, gN)
  whT <- match(to, gN)
  if (graph::isDirected(g)) {
    nb <- split(whT, factor(whF, levels = seq_along(gN)))
  } else {
    nb <- split(c(whT, whF), factor(c(whF, whT), levels = seq_along(gN)))
  }
  eL <- g@edgeL
  for (i in which(lengths(nb) > 0)) {
    eL[[i]]$edges <- unique(c(eL[[i]]$edges, nb[[i]]))
  }
  g@edgeL <- eL
  if (!("weight" %in% names(graph::edgeDataDefaults(g)))) {
    graph::edgeDataDefaults(g, attr = "weight") <- 1L
  }
  graph::edgeData(g, from = from, to = to, attr = "weight") <- weights
  g
}


#' Remove edges from a graphNEL
#'
#' Same result as `graph::removeEdge(from, to, g)` for existing edges, whose
#' loops look up the edge list of each node by name: the edges (both
#' directions for an undirected graph) and their data are removed.
#'
#' @param g a `graphNEL`.
#' @param from,to character vectors of node names, of the same length, giving
#'   existing edges.
#' @return a `graphNEL`.
#' @noRd
.removeEdges <- function(g, from, to) {
  if (!graph::isDirected(g)) {
    fromOrig <- from
    from <- c(fromOrig, to)
    to <- c(to, fromOrig)
  }
  graph::removeAttrDataItem(g@edgeData, x = paste(from, to, sep = "|")) <- NULL
  gN <- graph::nodes(g)
  kill <- split(match(to, gN), factor(match(from, gN), levels = seq_along(gN)))
  eL <- g@edgeL
  for (i in which(lengths(kill) > 0)) {
    k <- match(kill[[i]], eL[[i]]$edges)
    k <- k[!is.na(k)]
    if (length(k) > 0) eL[[i]]$edges <- eL[[i]]$edges[-k]
  }
  g@edgeL <- eL
  g
}
