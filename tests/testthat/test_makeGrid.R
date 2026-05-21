## input validation
test_that("makeGrid errors and warns correctly on bad inputs", {
  # no size or n.lon/n.lat
  expect_error(makeGrid(), "Please provide either size or n.lon/n.lat")
  
  # grid too small
  expect_warning(
    makeGrid(3, lon.range = c(1, 5), lat.range = c(1, 5)),
    "Minimum grid size is 4"
  )
  
  # longitude out of range
  expect_warning(
    makeGrid(25, lon.range = c(-200, 0), lat.range = c(1, 5)),
    "Setting lowest longitude to -180"
  )
  
  # latitude out of range
  expect_warning(
    makeGrid(25, lon.range = c(1, 5), lat.range = c(-100, 5)),
    "Setting lowest latitude to -90"
  )
})

## structure
test_that("makeGrid creates a gGraph with correct number of nodes", {
  result <- makeGrid(25, lon.range = c(1, 5), lat.range = c(1, 5))
  expect_s4_class(result, "gGraph")
  expect_equal(length(getNodes(result)), 25L)
})

test_that("makeGrid nodes never have more than 4 neighbours", {
  result     <- makeGrid(100, lon.range = c(1, 10), lat.range = c(1, 10))
  neighbours <- result@graph@edgeL
  n.neighbours <- sapply(neighbours, function(e) length(e$edges))
  expect_true(all(n.neighbours <= 4))
})

test_that("makeGrid has correct number of corner and edge nodes", {
  result       <- makeGrid(25, lon.range = c(1, 5), lat.range = c(1, 5))
  neighbours   <- result@graph@edgeL
  n.neighbours <- sapply(neighbours, function(e) length(e$edges))
  
  # 4 corner nodes with 2 neighbours
  expect_equal(sum(n.neighbours == 2), 4L)
  
  # 12 edge nodes with 3 neighbours
  expect_equal(sum(n.neighbours == 3), 12L)
})

test_that("makeGrid has correct neighbour assignment", {
  result <- makeGrid(25, lon.range = c(1, 5), lat.range = c(1, 5))
  # node 1 (top-left corner) should be connected to nodes 2 and 6
  expect_true(areNeighbours("1", "2", getGraph(result)))
  expect_true(areNeighbours("1", "6", getGraph(result)))
  expect_false(areNeighbours("1", "3", getGraph(result)))
})
