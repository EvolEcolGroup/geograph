test_that("combineCosts sum doubles costs of identical graphs", {
  result <- combineCosts(rawgraph.10k, rawgraph.10k, method = "sum")
  original.costs <- getCosts(rawgraph.10k, res.type = "vector")
  combined.costs <- getCosts(result, res.type = "vector")
  expect_equal(unname(combined.costs), unname(original.costs * 2))
})

test_that("combineCosts product squares costs of identical graphs", {
  result <- combineCosts(rawgraph.10k, rawgraph.10k, method = "product")
  original.costs <- getCosts(rawgraph.10k, res.type = "vector")
  combined.costs <- getCosts(result, res.type = "vector")
  expect_equal(unname(combined.costs), unname(original.costs^2))
})

test_that("combineCosts function method applies FUN correctly", {
  result <- combineCosts(rawgraph.10k, rawgraph.10k,
    method = "function",
    FUN    = function(x1, x2) x1 + x2
  )
  original.costs <- getCosts(rawgraph.10k, res.type = "vector")
  combined.costs <- getCosts(result, res.type = "vector")
  expect_equal(unname(combined.costs), unname(original.costs * 2))
})

test_that("combineCosts errors when method is function but FUN is NULL", {
  expect_error(
    combineCosts(rawgraph.10k, rawgraph.10k, method = "function"),
    "FUN needs to be defined"
  )
})

test_that("combineCosts errors when graphs have different nodes", {
  x1 <- rawgraph.10k
  x2 <- rawgraph.40k
  expect_error(
    combineCosts(x1, x2),
    "the graphs differ in the edges"
  )
})

test_that("combineCosts rejects graphs with different edges without recycling", {
  sq <- makeSquareGrid(n.lon = 3, n.lat = 3, lon.range = c(0, 2), lat.range = c(0, 2))
  a <- setCosts(sq, node.values = 1:9)
  b <- setEdges(a, remove = data.frame("1", "2"))
  expect_no_warning(
    expect_error(combineCosts(a, b), "the graphs differ in the edges they have")
  )
})

test_that("combineCosts rejects graphs with different node names", {
  sq <- makeSquareGrid(n.lon = 3, n.lat = 3, lon.range = c(0, 2), lat.range = c(0, 2))
  a <- setCosts(sq, node.values = 1:9)
  b <- a # same structure, different node names
  new.names <- paste0("n", 1:9)
  rownames(b@coords) <- new.names
  graph::nodes(b@graph) <- new.names
  expect_true(validObject(b))
  expect_error(combineCosts(a, b), "the graphs differ in the edges they have")
})
