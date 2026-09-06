test_that("setNodesAttr errors if x is not a gGraph", {
  expect_error(
    setNodesAttr(list(), attr.name = "habitat", values = 1),
    "x is not a valid gGraph object"
  )
})

test_that("setNodesAttr errors if attr.name is not a single character string", {
  expect_error(
    setNodesAttr(rawgraph.10k, attr.name = 123, values = rep(1, length(getNodes(rawgraph.10k)))),
    "`attr.name` must be a single character string"
  )
  expect_error(
    setNodesAttr(rawgraph.10k, attr.name = c("a", "b"), values = rep(1, length(getNodes(rawgraph.10k)))),
    "`attr.name` must be a single character string"
  )
})

test_that("setNodesAttr errors if values length does not match node count", {
  expect_error(
    setNodesAttr(rawgraph.10k, attr.name = "test", values = c(1, 2, 3)),
    "lengths must match exactly"
  )
})

test_that("setNodesAttr returns a gGraph with the new attribute", {
  n <- length(getNodes(rawgraph.10k))
  result <- setNodesAttr(rawgraph.10k, attr.name = "test", values = rep(1, n))

  expect_s4_class(result, "gGraph")
  expect_true("test" %in% colnames(getNodesAttr(result)))
})

test_that("setNodesAttr correctly stores the values", {
  n <- length(getNodes(rawgraph.10k))
  vals <- rep(42, n)
  result <- setNodesAttr(rawgraph.10k, attr.name = "test", values = vals)

  expect_equal(getNodesAttr(result)$test, vals)
})

test_that("setNodesAttr replaces an existing attribute", {
  n <- length(getNodes(rawgraph.10k))
  result <- setNodesAttr(rawgraph.10k, attr.name = "habitat", values = rep("land", n))

  expect_true(all(getNodesAttr(result)$habitat == "land"))
})
