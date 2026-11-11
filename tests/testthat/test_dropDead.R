test_that("dropDeadEdges removes edges whose costs are all above the threshold", {
  sq <- makeSquareGrid(n.lon = 3, n.lat = 3, lon.range = c(0, 2), lat.range = c(0, 2))
  x <- setCosts(sq, node.values = 5) # uniform costs: hasCosts() is FALSE
  expect_equal(nrow(getEdges(dropDeadEdges(x, thres = 1), res.type = "matNames")), 0L)
  expect_equal(nrow(getEdges(dropDeadEdges(x, thres = 5), res.type = "matNames")), 24L)
})

test_that("dropDeadEdges only removes edges with a cost above the threshold", {
  sq <- makeSquareGrid(n.lon = 3, n.lat = 3, lon.range = c(0, 2), lat.range = c(0, 2))
  x <- setCosts(sq, node.values = c(1, 1, 1, 1, 1, 1, 1, 1, 9), method = "product")
  res <- dropDeadEdges(x, thres = 2)
  # node 9 is only linked through costly edges
  expect_length(getEdges(res)[["9"]], 0L)
  expect_true(all(getCosts(res, res.type = "vector") <= 2))
})

test_that("dropDeadEdges leaves a graph without stored costs unchanged", {
  x <- new("gGraph", coords = cbind(lon = 1:3, lat = 1:3))
  expect_identical(dropDeadEdges(x, thres = 0), x)
})

test_that("dropDeadNodes gives the same result as the previous implementation", {
  skip_on_cran()
  for (x in list(worldgraph.10k, rawgraph.10k, dropCosts(worldgraph.10k))) {
    res <- dropDeadNodes(x)
    expectSameGGraph(res, dropDeadNodesRef(x))
  }
  x <- new("gGraph", coords = cbind(lon = 1:5, lat = 1:5))
  x@graph <- graph::addEdge(c("4", "2"), c("5", "4"), getGraph(x), c(1, 2))
  expectSameGGraph(dropDeadNodes(x), dropDeadNodesRef(x))
  expect_identical(getNodes(dropDeadNodes(x)), c("2", "4", "5"))
  ## edgeless graph
  x <- new("gGraph", coords = cbind(lon = 1:3, lat = 1:3))
  expectSameGGraph(dropDeadNodes(x), dropDeadNodesRef(x))
})

test_that("dropDeadEdges gives the same graph as the previous implementation", {
  skip_on_cran()
  set.seed(6)
  x <- setCosts(rawgraph.10k, node.values = runif(length(getNodes(rawgraph.10k))))
  th <- median(getCosts(x, res.type = "vector"))
  expectSameGraph(getGraph(dropDeadEdges(x, th)), dropDeadEdgesGraphRef(x, th))
  ## asymmetric weights: an edge is kept if either direction is under the threshold
  y <- suppressWarnings(setCosts(rawgraph.10k, node.values = runif(length(getNodes(rawgraph.10k))),
                                 method = "function", FUN = function(a, b) a + 2 * b
  ))
  th <- median(getCosts(y, res.type = "vector"))
  expect_true(validObject(getGraph(dropDeadEdges(y, th))))
})
