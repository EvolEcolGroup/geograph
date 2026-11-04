## Test-only helpers for the performance rewrites (see data-raw/speed_plan.md).

## Compare two graphNEL objects on what matters to geoGraph. Do not use
## identical() on the graphNEL objects themselves: the internal order of the
## @edgeData@data entries can differ while the graph is the same.
expect_same_graph <- function(a, b) {
  expect_identical(graph::nodes(a), graph::nodes(b))
  expect_identical(graph::edges(a), graph::edges(b))
  expect_identical(graph::edgeWeights(a), graph::edgeWeights(b))
  expect_identical(graph::edgemode(a), graph::edgemode(b))
  expect_identical(length(a@edgeData@data), length(b@edgeData@data))
}

## ---- previous implementations, used as references ---------------------------

## `[` on a gGraph, through graph::subGraph()
subsetRef <- function(x, i) {
  if (is.logical(i)) i <- rep(i, length = nrow(getCoords(x)))
  if (is.character(i)) i <- match(i, getNodes(x))
  res <- x
  res@coords <- res@coords[i, , drop = FALSE]
  res@nodes.attr <- res@nodes.attr[i, , drop = FALSE]
  res@graph <- graph::subGraph(graph::nodes(res@graph)[i], res@graph)
  res
}

dropDeadNodesRef <- function(x) {
  nodes.in.edges <- unique(as.vector(getEdges(x, res.type = "matNames")))
  subsetRef(x, nodes.in.edges)
}

keepMaxConnectedSetRef <- function(x) {
  myGraph <- getGraph(x)
  connected.sets <- RBGL::connectedComp(myGraph)
  maxSet <- connected.sets[[which.max(lapply(connected.sets, length))]]
  maxSet.idx <- match(maxSet, graph::nodes(myGraph))
  maxSet.idx <- maxSet.idx[!is.na(maxSet.idx)]
  edgeW <- graph::edgeWeights(myGraph)
  edgeL <- graph::edgeL(myGraph)
  newEdgeL <- list()
  for (i in seq_along(edgeL)) {
    newEdgeL[[i]] <- list()
    if (i %in% maxSet.idx) {
      keep <- edgeL[[i]]$edges %in% maxSet.idx
      newEdgeL[[i]]$edges <- edgeL[[i]]$edges[keep]
      newEdgeL[[i]]$weights <- edgeW[[i]][keep]
    } else {
      newEdgeL[[i]]$edges <- numeric(0)
      newEdgeL[[i]]$weights <- numeric(0)
    }
  }
  names(newEdgeL) <- graph::nodes(myGraph)
  res <- x
  res@graph <- new("graphNEL", nodes = graph::nodes(myGraph), edgeL = newEdgeL)
  dropDeadNodesRef(res)
}

## compare two gGraph objects (graph slot compared with expect_same_graph)
expect_same_gGraph <- function(a, b) {
  expect_identical(getCoords(a), getCoords(b))
  expect_identical(a@nodes.attr, b@nodes.attr)
  expect_identical(a@meta, b@meta)
  expect_same_graph(getGraph(a), getGraph(b))
}

## edge weights of setCosts(), computed from the node costs as before
setCostsGraphRef <- function(x, nodeCosts, method, FUN = NULL, ...) {
  EL <- getGraph(x)@edgeL
  for (i in seq_along(EL)) {
    EL[[i]]$weights <- switch(method,
      mean = (nodeCosts[i] + nodeCosts[EL[[i]]$edges]) / 2,
      product = nodeCosts[i] * nodeCosts[EL[[i]]$edges],
      "function" = FUN(nodeCosts[i], nodeCosts[EL[[i]]$edges], ...)
    )
  }
  new("graphNEL", nodes = getNodes(x), edgeL = EL)
}

dropDeadEdgesGraphRef <- function(x, thres) {
  myGraph <- getGraph(x)
  edgeW <- graph::edgeWeights(myGraph)
  edgeL <- graph::edgeL(myGraph)
  toKeep <- lapply(edgeW, function(v) v <= thres)
  newEdgeL <- list()
  for (i in seq_along(edgeL)) {
    newEdgeL[[i]] <- list()
    newEdgeL[[i]]$edges <- edgeL[[i]]$edges[toKeep[[i]]]
    newEdgeL[[i]]$weights <- edgeW[[i]][toKeep[[i]]]
  }
  names(newEdgeL) <- graph::nodes(myGraph)
  new("graphNEL", nodes = graph::nodes(myGraph), edgeL = newEdgeL)
}

combineCostsGraphRef <- function(x1, x2, method, FUN = NULL, ...) {
  myGraph1 <- getGraph(x1)
  edgeW1 <- graph::edgeWeights(myGraph1)
  edgeL1 <- graph::edgeL(myGraph1)
  edgeW2 <- graph::edgeWeights(getGraph(x2))
  newEdgeL <- list()
  for (i in seq_along(edgeL1)) {
    newEdgeL[[i]] <- list()
    newEdgeL[[i]]$edges <- edgeL1[[i]]$edges
    newEdgeL[[i]]$weights <- switch(method,
      sum = edgeW1[[i]] + edgeW2[[i]],
      product = edgeW1[[i]] * edgeW2[[i]],
      "function" = FUN(edgeW1[[i]], edgeW2[[i]], ...)
    )
  }
  names(newEdgeL) <- graph::nodes(myGraph1)
  new("graphNEL", nodes = graph::nodes(myGraph1), edgeL = newEdgeL)
}

setDistCostsGraphRef <- function(x) {
  E <- getEdges(x, res.type = "matNames")
  xy <- getCoords(x)
  w <- fields::rdist.earth.vec(xy[E[, 1], , drop = FALSE], xy[E[, 2], , drop = FALSE])
  graph::edgeData(x@graph, from = E[, 1], to = E[, 2], attr = "weight") <- w
  x@graph
}

getCostsAsIsUniqueRef <- function(x) {
  res <- graph::edgeWeights(x@graph)
  res <- lapply(names(res), function(node) {
    edges <- res[[node]]
    if (length(edges) > 0) edges[node < names(edges)] else edges
  })
  names(res) <- names(graph::edgeWeights(x@graph))
  res
}

## assignByRaster(), previous implementation: nearest node with
## sf::st_nearest_feature() (s2 off) on the cells of as.data.frame(raster)
assignByRasterRef <- function(graph, raster, layer.name = "raster_points", fun = "mean", na.rm = TRUE, ...) {
  raster.df <- as.data.frame(raster, xy = TRUE)
  colnames(raster.df) <- c("lon", "lat", "value")
  raster.sf <- sf::st_as_sf(raster.df, coords = c("lon", "lat"), crs = terra::crs(raster, proj = TRUE))
  node.coords <- as.data.frame(getCoords(graph))
  nodes.sf <- sf::st_as_sf(node.coords, coords = c("lon", "lat"), crs = 4326)
  raster.sf <- sf::st_transform(raster.sf, sf::st_crs(nodes.sf))
  old.s2 <- sf::sf_use_s2()
  on.exit(suppressMessages(sf::sf_use_s2(old.s2)), add = TRUE)
  suppressMessages(sf::sf_use_s2(FALSE))
  node.id <- suppressMessages(sf::st_nearest_feature(raster.sf, nodes.sf))
  ## values per node, in cell order (as group_by() + nest() + left_join())
  vals <- split(raster.sf$value, factor(node.id, levels = seq_len(nrow(node.coords))))
  collapsed <- .collapseNodeAttribute(unname(vals), fun = fun, na.rm = na.rm, ...)
  setNodesAttr(graph, attr.name = layer.name, values = collapsed)
}

areNeighboursRef <- function(V1, V2, graph) {
  V1 <- as.character(V1)
  V2 <- as.character(V2)
  if (inherits(graph, "gGraph")) graph <- getGraph(graph)
  edg <- graph::edges(graph)
  res <- mapply(function(x, y) any(edg[[x]] == y), V1, V2)
  names(res) <- paste(V1, V2, sep = "->")
  res
}

## stricter than expect_same_graph: also compares the stored edge data
## (sorted by key) and the edge data defaults
expect_same_edge_data <- function(a, b) {
  expect_same_graph(a, b)
  da <- a@edgeData@data
  db <- b@edgeData@data
  expect_identical(da[sort(as.character(names(da)))], db[sort(as.character(names(db)))])
  expect_identical(a@edgeData@defaults, b@edgeData@defaults)
  expect_identical(lapply(a@edgeL, `[[`, "edges"), lapply(b@edgeL, `[[`, "edges"))
}

## segments drawn by the previous plotEdges(), for the nodes 'toKeep' in the
## plotting area
plotEdgesSegmentsRef <- function(x, toKeep, useCosts, lwd = 1, lty = 1, maxLwd = 3) {
  keptCoords <- getCoords(x)[toKeep, , drop = FALSE]
  edges <- getEdges(x, res.type = "matNames", unique = TRUE)
  temp <- (edges[, 1] %in% rownames(keptCoords)) & (edges[, 2] %in% rownames(keptCoords))
  keptEdges <- edges[temp, , drop = FALSE]
  if (useCosts) {
    edges.w <- getCosts(x, res.type = "vector", unique = TRUE)[temp]
    lwd <- unname((1 - edges.w / max(edges.w)) * maxLwd) # names are not drawn
    lty <- rep(1, length(lwd))
    lty[lwd < 1e-5] <- 3
  }
  idx1 <- match(as.character(keptEdges[, 1]), rownames(keptCoords))
  idx2 <- match(as.character(keptEdges[, 2]), rownames(keptCoords))
  list(
    x0 = keptCoords[idx1, 1], y0 = keptCoords[idx1, 2], x1 = keptCoords[idx2, 1], y1 = keptCoords[idx2, 2],
    lwd = lwd, lty = lty
  )
}

ggraphEdgesDfRef <- function(g) {
  E <- getEdges(g, res.type = "matNames", unique = TRUE)
  co <- getCoords(g)
  data.frame(
    from = E[, 1], to = E[, 2],
    x = co[E[, 1], 1], y = co[E[, 1], 2],
    xend = co[E[, 2], 1], yend = co[E[, 2], 2]
  )
}

dfToSfLinesRef <- function(df) {
  lines <- lapply(seq_len(nrow(df)), function(i) {
    sf::st_linestring(rbind(c(df$x[i], df$y[i]), c(df$xend[i], df$yend[i])))
  })
  sf::st_sf(from = df$from, to = df$to, geometry = sf::st_sfc(lines, crs = 4326))
}

gpathDfRef <- function(gp) {
  xy <- attr(gp, "xy")
  do.call(rbind, lapply(seq_along(gp), function(k) {
    nd <- gp[[k]]$path_detail
    if (length(nd) < 2) {
      return(NULL)
    }
    data.frame(path_id = k, order = seq_along(nd), lon = xy[nd, 1], lat = xy[nd, 2])
  }))
}

dfToSfPathsRef <- function(df) {
  by.path <- split(df, df$path_id)
  lines <- lapply(by.path, function(p) sf::st_linestring(cbind(p$lon, p$lat)))
  sf::st_sf(path_id = names(by.path), geometry = sf::st_sfc(lines, crs = 4326))
}

plotGPathRef <- function(x, col = "rainbow", lwd = 3, seed = NULL, ...) {
  listNodes <- lapply(x, function(e) e$path_detail)
  xy <- attr(x, "xy")
  n.path <- length(listNodes)
  if (is.character(col) && col[1] == "rainbow") {
    if (!is.null(seed)) set.seed(seed)
    col <- sample(grDevices::rainbow(length(x)))
  }
  col <- rep(col, length = n.path)
  lwd <- rep(lwd, length = n.path)
  f1 <- function(vecNodes, col, lwd, ...) {
    N <- length(vecNodes)
    if (N < 2) return()
    from <- vecNodes[seq_len(N - 1)]
    to <- vecNodes[2:N]
    geo.segments(xy[from, 1], xy[from, 2], xy[to, 1], xy[to, 2], col = col, lwd = lwd, ...)
  }
  lapply(seq_along(listNodes), function(i) f1(listNodes[[i]], col = col[i], lwd = lwd[i], ...))
  invisible()
}

## all segments drawn by graphics::segments() while evaluating expr, one row
## per segment
drawnSegments <- function(expr) {
  drawn <- new.env()
  drawn$segs <- list()
  suppressMessages(trace(graphics::segments,
    bquote({
      n <- length(x0)
      seg <- data.frame(
        x0 = x0, y0 = y0, x1 = x1, y1 = y1,
        col = rep(col, length.out = n), lwd = rep(lwd, length.out = n), lty = rep(lty, length.out = n)
      )
      assign("segs", c(get("segs", envir = .(drawn)), list(seg)), envir = .(drawn))
    }),
    print = FALSE
  ))
  on.exit(suppressMessages(untrace(graphics::segments)))
  force(expr)
  res <- do.call(rbind, drawn$segs)
  rownames(res) <- NULL
  res
}

## outline nodes of a polygon, as computed by the previous polygonBetween()
polygonOutlineRef <- function(g, nodes.polygon) {
  neighL <- graph::edges(getGraph(g))
  is.outline <- vapply(nodes.polygon, function(node) !all(neighL[[node]] %in% nodes.polygon), logical(1))
  nodes.polygon[is.outline]
}

## nodes of buffer(), previous implementation
bufferNodesRef <- function(x, nodes, d) {
  EDGES <- graph::edges(getGraph(x))
  XY <- getCoords(x)
  find.buf.onenode <- function(node, d) {
    curNodes <- node
    res <- node
    visited.nodes <- node
    while (TRUE) {
      neig <- unlist(EDGES[curNodes])
      neig <- setdiff(neig, visited.nodes)
      visited.nodes <- c(visited.nodes, neig)
      temp <- fields::rdist.earth(XY[node, , drop = FALSE], XY[neig, , drop = FALSE], miles = FALSE, R = NULL)
      toKeep <- temp < d
      if (!any(toKeep)) break
      curNodes <- neig[toKeep]
      res <- c(res, neig[toKeep])
    }
    res
  }
  unique(unlist(lapply(nodes, find.buf.onenode, d)))
}

## colours of connectivityPlot(), previous implementation
connectivityColRef <- function(x, seed = NULL) {
  connected.sets <- .connectedSets(getGraph(x))
  nbSets <- length(connected.sets)
  myNodes <- getNodes(x)
  if (is.gGraph(x)) {
    if (!is.null(seed)) set.seed(seed)
    colSets <- sample(grDevices::rainbow(nbSets))
    col <- rep("lightgray", length(myNodes))
    names(col) <- myNodes
    for (i in seq_len(nbSets)) col[connected.sets[[i]]] <- colSets[i]
    return(col)
  }
  nbRelSets <- 0
  for (i in seq_len(nbSets)) if (any(myNodes %in% connected.sets[[i]])) nbRelSets <- nbRelSets + 1
  if (!is.null(seed)) set.seed(seed)
  colSets <- sample(grDevices::rainbow(nbRelSets))
  col <- rep("lightgray", length(myNodes))
  names(col) <- myNodes
  color.idx <- 0
  for (i in seq_len(nbSets)) {
    nodes.in.set <- names(col)[names(col) %in% connected.sets[[i]]]
    if (length(nodes.in.set) > 0) {
      color.idx <- color.idx + 1
      col[nodes.in.set] <- colSets[color.idx]
    }
  }
  col
}

getEdgesMatIdRef <- function(x, unique = FALSE) {
  res <- graph::edgeL(x@graph)
  col1 <- rep(seq_along(res), sapply(res, function(e) length(e$edges)))
  col2 <- unlist(res)
  res <- cbind(Vi = col1, Vj = col2)
  if (unique) res <- res[res[, 1] < res[, 2], , drop = FALSE]
  rownames(res) <- NULL
  res
}
