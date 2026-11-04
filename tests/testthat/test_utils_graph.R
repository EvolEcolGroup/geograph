## round trip graph -> .edgeTable() -> .graphFromTable()
roundTrip <- function(g) {
  et <- .edgeTable(g, weights = length(g@edgeData@data) > 0)
  .graphFromTable(graph::nodes(g), et$from, et$to, et$w, edgemode = graph::edgemode(g))
}

test_that(".edgeTable follows the order of edgeL and edgeWeights", {
  g <- getGraph(worldgraph.10k)
  et <- .edgeTable(g)
  em <- graph::edgeMatrix(g, duplicates = TRUE)
  expect_identical(et$from, as.integer(em[1, ]))
  expect_identical(et$to, as.integer(em[2, ]))
  expect_identical(et$w, unlist(graph::edgeWeights(g), use.names = FALSE))
  expect_null(.edgeTable(g, weights = FALSE)$w)
})

test_that(".graphFromTable rebuilds weighted and unweighted graphs", {
  g <- getGraph(worldgraph.10k)
  res <- roundTrip(g)
  expect_true(validObject(res))
  expect_same_graph(res, g)
  expect_false(graph::isDirected(res))

  ## heterogeneous costs
  g <- getGraph(rawgraph.10k)
  expect_same_graph(roundTrip(g), g)

  ## no stored edge data
  g <- getGraph(dropCosts(rawgraph.10k))
  expect_length(g@edgeData@data, 0)
  res <- roundTrip(g)
  expect_same_graph(res, g)
  expect_length(res@edgeData@data, 0)
})

test_that(".graphFromTable handles small and edgeless graphs", {
  ## two nodes, one edge
  g <- new("graphNEL", nodes = c("a", "b"), edgemode = "undirected")
  g <- graph::addEdge("a", "b", g, 2.5)
  res <- roundTrip(g)
  expect_true(validObject(res))
  expect_same_graph(res, g)

  ## isolated node
  g <- new("graphNEL", nodes = c("a", "b", "c"), edgemode = "undirected")
  g <- graph::addEdge("a", "c", g, 3)
  res <- roundTrip(g)
  expect_same_graph(res, g)
  expect_identical(graph::degree(res), c(a = 1, b = 0, c = 1))

  ## no edges, with and without weights
  g <- new("graphNEL", nodes = c("a", "b", "c"), edgemode = "undirected")
  et <- .edgeTable(g)
  expect_identical(et$from, integer(0))
  expect_identical(et$to, integer(0))
  expect_identical(et$w, numeric(0))
  res <- .graphFromTable(graph::nodes(g), et$from, et$to)
  expect_true(validObject(res))
  expect_same_graph(res, g)
  res <- .graphFromTable(graph::nodes(g), et$from, et$to, et$w)
  expect_true(validObject(res))
  expect_identical(graph::nodes(res), graph::nodes(g))
  expect_equal(graph::numEdges(res), 0)
})

test_that(".graphFromTable keeps a directed graph directed, with its own weights", {
  g <- new("graphNEL", nodes = c("a", "b", "c"), edgemode = "directed")
  g <- graph::addEdge(c("a", "b", "c"), c("b", "a", "a"), g, c(1, 2, 3))
  res <- roundTrip(g)
  expect_true(validObject(res))
  expect_true(graph::isDirected(res))
  expect_same_graph(res, g)
})

test_that(".graphFromTable stores the weight of each direction separately", {
  ## asymmetric weights on an undirected graph: each directed edge keeps its
  ## own value (the undirected edgeData<- would overwrite the reverse edge)
  nodes <- c("a", "b", "c")
  res <- .graphFromTable(nodes, c(1L, 2L, 2L, 3L), c(2L, 1L, 3L, 2L), c(1, 2, 3, 4))
  expect_false(graph::isDirected(res))
  expect_identical(
    unlist(graph::edgeData(res, c("a", "b", "b", "c"), c("b", "a", "c", "b"), "weight"), use.names = FALSE),
    c(1, 2, 3, 4)
  )
  expect_length(res@edgeData@data, 4)
})
