library("geoGraph")
context("Test classes")

test_that("Empty constructors work", {
  skip_on_cran()
  x <- new("gGraph")
  y <- new("gData")
  expect_is(x, "gGraph")
  expect_is(y, "gData")
})
