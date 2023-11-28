
test_that("Empty constructors work", {
  x <- new("gGraph")
  expect_true(inherits(x,"gGraph"))
  y <- new("gData")
  expect_true(inherits(y,"gData"))
})

test_that("Contructors fails with invalid coordinates",{
  NACoords <- data.frame(long = c(-24, NA), lat =  c(31,55))
  #Create gGraph with NA's
  expect_error(new("gGraph", coords = NACoords),
               "Argument coords includes NAs")

})