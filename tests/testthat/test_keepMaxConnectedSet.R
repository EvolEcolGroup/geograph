test_that("keepMaxConnectedSet drops all small sets", {
  # we start with worldgraph.10k, where Eurasia and Africa form the biggest set
  # check that the graph is not all connected
  expect_false(isConnected(worldgraph.10k))
  max.set <- keepMaxConnectedSet(worldgraph.10k)
  # the max set should be all connected
  expect_true(isConnected(max.set))
  # check that the max set does not include the Americas
  coords.max.set <- getCoords(max.set)
  expect_true(sum(coords.max.set[, 1] < -20) == 0)
})

test_that("keepMaxConnectedSet gives the same result as the previous implementation", {
  skip_on_cran()
  expectSameGGraph(keepMaxConnectedSet(worldgraph.10k), keepMaxConnectedSetRef(worldgraph.10k))
  expectSameGGraph(keepMaxConnectedSet(rawgraph.10k), keepMaxConnectedSetRef(rawgraph.10k))
  ## without stored costs, the result stores weights of 1
  u <- dropCosts(worldgraph.10k)
  res <- keepMaxConnectedSet(u)
  expectSameGGraph(res, keepMaxConnectedSetRef(u))
  expect_gt(length(getGraph(res)@edgeData@data), 0)
})

test_that("keepMaxConnectedSet keeps the first of tied sets, and handles edgeless graphs", {
  x <- new("gGraph", coords = cbind(lon = 1:7, lat = 1:7))
  g <- graph::addEdge(c("3", "5", "1"), c("4", "6", "7"), getGraph(x), c(2, 3, 4))
  x@graph <- g
  res <- keepMaxConnectedSet(x)
  expectSameGGraph(res, keepMaxConnectedSetRef(x))
  expect_identical(getNodes(res), c("1", "7"))

  ## edgeless: every set is a single node without edges, nothing is kept
  x <- new("gGraph", coords = cbind(lon = 1:3, lat = 1:3))
  res <- keepMaxConnectedSet(x)
  expectSameGGraph(res, keepMaxConnectedSetRef(x))
  expect_length(getNodes(res), 0)
})
