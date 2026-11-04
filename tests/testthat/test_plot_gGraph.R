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

test_that("plot without a shape draws the nodes of the new area, not the previous one", {
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  x <- new("gGraph", coords = cbind(lon = 0:9, lat = rep(0, 10)))
  plot(x, reset = TRUE, shape = NULL)
  .zoomlog.up(c(0.6, 2.4, -1, 1)) # zoom in on nodes 2 and 3
  plot(x, shape = NULL)

  # replot the whole graph and count the points drawn
  drawn <- new.env()
  trace("plot.xy",
    bquote(assign("n", length(xy$x), envir = .(drawn))),
    print = FALSE, where = graphics::plot.default
  )
  on.exit(untrace("plot.xy", where = graphics::plot.default), add = TRUE)
  plot(x, reset = TRUE, shape = NULL)
  expect_equal(drawn$n, 10L)
})

test_that("plotEdges draws the same segments as the previous implementation", {
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  drawn <- new.env()
  trace(graphics::segments,
    bquote(assign("args", list(x0 = x0, y0 = y0, x1 = x1, y1 = y1, lwd = lwd, lty = lty), envir = .(drawn))),
    print = FALSE
  )
  on.exit(untrace(graphics::segments), add = TRUE)
  set.seed(11)
  costly <- setCosts(rawgraph.10k, node.values = runif(length(getNodes(rawgraph.10k))))
  for (x in list(worldgraph.10k, rawgraph.10k, costly)) {
    plot(x, reset = TRUE, shape = NULL)
    .zoomlog.up(c(-10, 40, 30, 70)) # Europe
    plot(x, shape = NULL)
    toKeep <- isInArea(x, reg = "current", res.type = "integer", quiet = TRUE)
    for (useCosts in c(TRUE, FALSE)) {
      plotEdges(x, useCosts = useCosts)
      expect_identical(drawn$args, plotEdgesSegmentsRef(x, toKeep, useCosts))
    }
  }
})
