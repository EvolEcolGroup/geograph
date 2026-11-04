## Prototype of a replacement for RBGL::sp.between(g, start, finish), see
## data-raw/speed_plan.md, item P1.1. NOT package code yet.
##
## Returns one element per requested pair (start[k], finish[k]), in the
## requested order, named "start:finish" (duplicates kept), each element being
## list(length, path_detail, length_detail) exactly as in sp.between().
## Verified identical() to sp.between() (re-ordered by pair) for one-to-many,
## pairwise, start == finish, duplicated pairs and unreachable targets.
## 'start' and 'finish' must have the same length (no recycling).
.shortestPaths <- function(g, start, finish, eW = NULL) {
  nN <- graph::nodes(g)
  N <- length(nN)
  s.idx <- match(start, nN)
  f.idx <- match(finish, nN)
  if (anyNA(s.idx) || anyNA(f.idx)) stop("all nodes must be in the graph")
  if (is.null(eW)) eW <- unlist(graph::edgeWeights(g), use.names = FALSE)
  if (any(eW < 0, na.rm = TRUE)) stop("edge weights must be nonnegative")
  em <- graph::edgeMatrix(g, duplicates = !graph::isDirected(g))
  ## weight of the directed step u -> v, looked up by a numeric key
  ekey <- (em[1, ] - 1) * N + em[2, ]
  sep <- if (graph::isDirected(g)) "->" else "--"

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

  ## per-step weights, all at once
  lens <- lengths(path.idx)
  all.nodes <- unlist(path.idx, use.names = FALSE)
  pid <- rep.int(seq_len(P), lens)
  last <- cumsum(lens)
  first <- last - lens + 1L
  is.from <- rep(TRUE, length(all.nodes)); is.from[last] <- FALSE
  is.to <- rep(TRUE, length(all.nodes)); is.to[first] <- FALSE
  u <- all.nodes[is.from]
  v <- all.nodes[is.to]
  w <- eW[match((u - 1) * N + v, ekey)]
  names(w) <- paste(nN[u], nN[v], sep = sep)
  ws <- split(w, factor(pid[is.from], levels = seq_len(P)))

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
