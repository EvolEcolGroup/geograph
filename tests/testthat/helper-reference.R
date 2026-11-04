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
