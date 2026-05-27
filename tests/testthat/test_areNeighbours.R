test_that("areNeighbours works correctly", {
  # create a custom square grid
  test_graph <- makeSquareGrid(25, lon.range = c(1, 5), lat.range = c(1, 5))
  # test that the function correctly identifies neighbours
  expect_true(areNeighbours(V1 = "1", V2 = "2", graph = test_graph))
  # now do the same with the graphNEL part
  expect_true(areNeighbours(V1 = "1", V2 = "2", graph = getGraph(test_graph)))
  # test that the function correctly identifies non-neighbours
  expect_false(areNeighbours(V1 = "1", V2 = "9", graph = test_graph))
  # test that the function correctly identifies non-neighbours with graphNEL part
  expect_false(areNeighbours(V1 = "1", V2 = "9", graph = getGraph(test_graph)))
  # test that the function correctly identifies neighbours in the other direction
  expect_true(areNeighbours(V1 = "2", V2 = "1", graph = test_graph))
  # more than one pair
  expect_equal(unname(areNeighbours(
    V1 = c("1", "1"), V2 = c("2", "9"),
    graph = test_graph
  )), c(TRUE, FALSE))
})
