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
