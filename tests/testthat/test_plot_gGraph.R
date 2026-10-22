test_that("plotEdges works when a single edge is visible", {
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  x <- new("gGraph", coords = cbind(lon = c(0, 1, 5), lat = c(0, 1, 5)))
  x <- setEdges(x, add = data.frame("1", "2"))
  plot(x, reset = TRUE, shape = NULL)
  expect_no_error(plotEdges(x))
  expect_no_error(plot(x, reset = TRUE, shape = NULL, edges = TRUE))
})
