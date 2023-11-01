
test_that("Empty constructors work", {
  x <- new("gGraph")
  expect_true(inherits(x,"gGraph"))
  y <- new("gData")
  expect_true(inherits(y,"gData"))
})
