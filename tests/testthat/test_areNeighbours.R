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
