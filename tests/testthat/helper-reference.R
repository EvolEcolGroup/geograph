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
