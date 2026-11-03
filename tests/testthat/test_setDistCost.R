test_that("setDistCosts returns a gGraph object", {
  result <- setDistCosts(rawgraph.10k)
  expect_true(is.gGraph(result))
})

test_that("setDistCosts sets costs (hasCosts returns TRUE)", {
  result <- setDistCosts(rawgraph.10k)
  expect_true(hasCosts(result))
})

test_that("setDistCosts produces positive finite costs", {
  result <- setDistCosts(rawgraph.10k)
  costs <- getCosts(result, res.type = "vector")
  expect_true(all(is.finite(costs)))
  expect_true(all(costs > 0))
})

test_that("setDistCosts is symmetric (A->B same cost as B->A)", {
  result <- setDistCosts(rawgraph.10k)

  E <- getEdges(result, res.type = "matNames")

  # Find a pair where both directions exist
  edge.pairs <- paste(E[, 1], E[, 2], sep = "|")
  reverse.pairs <- paste(E[, 2], E[, 1], sep = "|")
  bidirectional.idx <- which(edge.pairs %in% reverse.pairs)[1]

  pair.fwd <- edge.pairs[bidirectional.idx]
  pair.rev <- paste(E[bidirectional.idx, 2], E[bidirectional.idx, 1], sep = "|")
  result <- setDistCosts(rawgraph.10k)

  data <- result@graph@edgeData@data
  expect_equal(data[[pair.fwd]]$weight, data[[pair.rev]]$weight)
})

test_that("setDistCosts errors on a non-gGraph input", {
  expect_error(setDistCosts("not_a_graph"))
  expect_error(setDistCosts(42))
})

test_that("setDistCosts gives the great circle distance in miles of each edge", {
  x <- new("gGraph", coords = cbind(lon = c(0, 1, 0), lat = c(0, 0, 1)))
  x <- setEdges(x, add = data.frame(c("1", "1"), c("2", "3")))
  x <- setDistCosts(x)
  costs <- getCosts(x, res.type = "vector")
  xy <- getCoords(x)
  expected <- fields::rdist.earth(xy, miles = TRUE)
  expect_equal(unname(costs["1.2"]), expected[1, 2])
  expect_equal(unname(costs["1.3"]), expected[1, 3])
  expect_equal(unname(costs["2.1"]), expected[2, 1])
})

test_that("setDistCosts works on a graph with a single edge", {
  x <- new("gGraph", coords = cbind(lon = c(0, 1), lat = c(0, 0)))
  x <- setEdges(x, add = data.frame("1", "2"))
  expect_true(all(getCosts(setDistCosts(x), res.type = "vector") > 60))
})
