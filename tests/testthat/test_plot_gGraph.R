test_that("plotEdges works when a single edge is visible", {
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  x <- new("gGraph", coords = cbind(lon = c(0, 1, 5), lat = c(0, 1, 5)))
  x <- setEdges(x, add = data.frame("1", "2"))
  plot(x, reset = TRUE, shape = NULL)
  expect_no_error(plotEdges(x))
  expect_no_error(plot(x, reset = TRUE, shape = NULL, edges = TRUE))
})

test_that(".nodeColors recycles colours over all nodes before subsetting", {
  x <- new("gGraph", coords = cbind(lon = 0:3, lat = rep(0, 4)))
  cols <- c("red", "green", "blue", "black")
  expect_equal(unname(.nodeColors(x, c(2, 3), col = cols)), c("green", "blue"))
  expect_equal(unname(.nodeColors(x, c("2", "3"), col = cols)), c("green", "blue"))
  expect_equal(unname(.nodeColors(x, 2:4, col = "orange")), rep("orange", 3))
  expect_equal(.nodeColors(x, 2:3), "red")
})

test_that(".nodeColors uses colour rules when no colour is given", {
  x <- worldgraph.10k[1:5]
  expected <- getColors(x, nodes = 2:3, attr.name = "habitat")
  expect_equal(.nodeColors(x, 2:3), expected)
  rules <- data.frame(habitat = c("land", "sea"), color = c("black", "white"))
  expect_equal(
    .nodeColors(x, 2:3, col.rules = rules),
    getColors(x, nodes = 2:3, attr.name = "habitat", col.rules = rules)
  )
})

test_that("points draws per-node colours on the right nodes when zoomed in", {
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  x <- new("gGraph", coords = cbind(lon = 0:3, lat = rep(0, 4)))
  plot(x, reset = TRUE, shape = NULL)
  .zoomlog.up(c(0.6, 2.4, -1, 1)) # zoom in: nodes 2 and 3 visible
  plot(x, shape = NULL)
  # record the colours actually passed to the graphics device
  drawn <- new.env()
  trace("plot.xy",
    bquote(assign("col", col, envir = .(drawn))),
    print = FALSE, where = graphics::points.default
  )
  on.exit(untrace("plot.xy", where = graphics::points.default), add = TRUE)
  points(x, col = c("red", "green", "blue", "black"))
  expect_equal(unname(drawn$col), c("green", "blue"))
})
