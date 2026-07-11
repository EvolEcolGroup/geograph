test_that("find land correctly", {
  # create a gGraph with one sea node and one land node
  myCoords <- data.frame(lon = c(-24, 37), lat = c(31, 55))
  obj <- new("gGraph", coords = myCoords)
  obj <- findLand(obj)
  # check that we classified the nodes correctly
  expect_true(all(obj@nodes.attr$habitat == c("sea", "land")))

  # error if we pass an incorrect class
  expect_error(
    findLand("blah"),
    "unable to find an inherited method"
  )

  # tests error for NAs as coordinates (this only applies to matrices and data.frames)
  # gGraph objects should fail when creted with NA coordinates
  na.coords <- data.frame(long = c(-24, NA), lat = c(31, 55))
  expect_error(
    findLand(na.coords),
    "Matrix contains NA"
  )
})


# Test whether findLand generates an error with invalid matrix object
test_that("co-ordinate format", {
  # Create co-ordinates matrix
  obj <- matrix(c(-24, 31, 37, 55), nrow = 2, ncol = 2, byrow = TRUE)
  obj <- findLand(obj)

  # check for factor output
  expect_true(inherits(obj, "factor"))
  # check for correct output
  expect_equal(obj, factor(c("sea", "land")))

  # Create co-ordinates matrix with NA
  na.matrix <- matrix(c(-24, NA, 37, 55), nrow = 2, ncol = 2, byrow = TRUE)
  # NA entries are recognized and produce error in plot
  expect_true(is.na(na.matrix[1, 2]))
  expect_error(plotEdges(na.matrix))

  # NA produces error in findLand
  expect_error(findLand(na.matrix))
})

test_that("findLand errors when shape is NULL", {
  coords <- matrix(c(10, 50), ncol = 2)
  expect_error(findLand(coords, shape = NULL), "cannot be NULL")
})

test_that("findLand rejects multi-element character shape", {
  coords <- matrix(c(10, 50), ncol = 2)
  expect_error(
    findLand(coords, shape = c("world", "moon")),
    "shape must be a sf object"
  )
})
