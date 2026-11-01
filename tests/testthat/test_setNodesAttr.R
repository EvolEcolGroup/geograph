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

test_that("node attributes can be set and read on a gGraph created without any", {
  x <- makeSquareGrid(n.lon = 2, n.lat = 2, lon.range = c(0, 1), lat.range = c(0, 1))
  expect_equal(nrow(getNodesAttr(x)), 4L)
  expect_equal(ncol(getNodesAttr(x)), 0L)

  x <- setNodesAttr(x, attr.name = "code", values = 1:4)
  expect_equal(getNodesAttr(x, attr.name = "code")$code, 1:4)
  expect_true(validObject(x))
})

test_that("node attributes can be set on legacy gGraphs with an empty nodes.attr", {
  x <- makeSquareGrid(n.lon = 2, n.lat = 2, lon.range = c(0, 1), lat.range = c(0, 1))
  x@nodes.attr <- data.frame() # as stored by older versions of the constructor
  expect_equal(nrow(getNodesAttr(x)), 4L)
  x <- setNodesAttr(x, attr.name = "code", values = 1:4)
  expect_equal(getNodesAttr(x)$code, 1:4)
})

test_that("functions relying on node attributes work on a gGraph without any", {
  x <- new("gGraph", coords = cbind(lon = c(0, 1, 2), lat = c(0, 0, 0)))
  x <- setEdges(x, add = data.frame(c("1", "2"), c("2", "3")))
  buf <- buffer(x, "1", 150, res.type = "gGraph")
  expect_equal(getNodesAttr(buf)$buffer, c(TRUE, TRUE, FALSE))
  expect_s3_class(ggplot2::ggplot() + geom_ggraph(data = x), "ggplot")
})

test_that("a hexagonal grid without attributes can be subset and queried", {
  hex <- suppressMessages(
    makeHexGrid(c(xmin = -10, xmax = 30, ymin = 35, ymax = 60), spacing = 1000)
  )
  sub <- hex[1:3]
  expect_equal(length(getNodes(sub)), 3L)
  expect_true(validObject(sub))
  expect_equal(dim(getNodesAttr(hex)), c(length(getNodes(hex)), 0L))
})
