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
  edge_pairs <- paste(E[, 1], E[, 2], sep = "|")
  reverse_pairs <- paste(E[, 2], E[, 1], sep = "|")
  bidirectional_idx <- which(edge_pairs %in% reverse_pairs)[1]

  pair_fwd <- edge_pairs[bidirectional_idx]
  pair_rev <- paste(E[bidirectional_idx, 2], E[bidirectional_idx, 1], sep = "|")
  result <- setDistCosts(rawgraph.10k)

  data <- result@graph@edgeData@data
  expect_equal(data[[pair_fwd]]$weight, data[[pair_rev]]$weight)
})

test_that("setDistCosts errors on a non-gGraph input", {
  expect_error(setDistCosts("not_a_graph"))
  expect_error(setDistCosts(42))
})
