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
