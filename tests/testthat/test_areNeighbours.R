test_that("areNeighbours works correctly", {
  # create a custom square grid
  testGraph <- makeSquareGrid(25, lon.range = c(1, 5), lat.range = c(1, 5))
  # test that the function correctly identifies neighbours
  expect_true(areNeighbours(V1 = "1", V2 = "2", graph = testGraph))
  # now do the same with the graphNEL part
  expect_true(areNeighbours(V1 = "1", V2 = "2", graph = getGraph(testGraph)))
  # test that the function correctly identifies non-neighbours
  expect_false(areNeighbours(V1 = "1", V2 = "9", graph = testGraph))
  # test that the function correctly identifies non-neighbours with graphNEL part
  expect_false(areNeighbours(V1 = "1", V2 = "9", graph = getGraph(testGraph)))
  # test that the function correctly identifies neighbours in the other direction
  expect_true(areNeighbours(V1 = "2", V2 = "1", graph = testGraph))
  # more than one pair
  expect_equal(unname(areNeighbours(
    V1 = c("1", "1"), V2 = c("2", "9"),
    graph = testGraph
  )), c(TRUE, FALSE))
})

test_that("areNeighbours gives the same result as the previous implementation", {
  set.seed(9)
  g <- worldgraph.10k
  E0 <- getEdges(g, res.type = "matNames")
  nn <- E0[sample(nrow(E0), 2000), ]
  nn[1:50, 2] <- sample(getNodes(g), 50)
  nn[51, 1] <- "not_a_node"
  nn[52, 2] <- "not_a_node"
  nn[53, 1] <- NA
  nn[54, 2] <- NA
  nn[55, ] <- c(nn[56, 1], NA)
  expect_identical(areNeighbours(nn[, 1], nn[, 2], g), areNeighboursRef(nn[, 1], nn[, 2], g))
  expect_identical(areNeighbours(character(0), character(0), g), areNeighboursRef(character(0), character(0), g))
  ## numeric node names and a directed graph
  expect_identical(areNeighbours(c(1, 2), c(2, 1), makeSquareGrid(9)), areNeighboursRef(c(1, 2), c(2, 1), makeSquareGrid(9)))
  d <- graph::addEdge(c("a", "b"), c("b", "c"), new("graphNEL", nodes = c("a", "b", "c"), edgemode = "directed"))
  expect_identical(areNeighbours(c("a", "b", "c"), c("b", "a", "b"), d), areNeighboursRef(c("a", "b", "c"), c("b", "a", "b"), d))
})
