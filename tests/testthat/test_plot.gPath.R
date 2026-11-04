test_that("plot.gPath draws paths that connect the plotted points", {
  hgdp.sub <- hgdp[getData(hgdp)$Population %in%
                     c("French", "Balochi", "BantuKenya", "Papuan", "Pima")]
  hgdp.path <- dijkstraBetween(hgdp.sub)

  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  plot(worldgraph.40k, col = "NA")
  points(hgdp.sub, col.nodes = "black", pch.nodes = 19)
  expect_no_error((plot(hgdp.path, col = "blue", lwd = 1)))

  endpoints <- unique(unlist(strsplit(names(hgdp.path), ":", fixed = TRUE)))
  expect_setequal(endpoints, unique(hgdp.sub@nodes.id))
})

test_that("print.gPath outputs correct number of paths", {
  hgdp.sub <- hgdp[getData(hgdp)$Population %in%
                     c("French", "Balochi", "BantuKenya", "Papuan", "Pima")]
  hgdp.path <- dijkstraBetween(hgdp.sub)

  expect_output(print(hgdp.path), "number of paths: 10")
})

test_that("print.gPath errors with extra arguments", {
  hgdp.sub <- hgdp[getData(hgdp)$Population %in%
                     c("French", "Balochi", "BantuKenya", "Papuan", "Pima")]
  hgdp.path <- dijkstraBetween(hgdp.sub)

  expect_error(
    print(hgdp.path, extra = "arg"),
    "additional parameters were passed"
  )
})

test_that("print.gPath shows truncated path names for long gPath", {
  hgdp.sub <- hgdp[getData(hgdp)$Population %in%
                     c("French", "Balochi", "BantuKenya", "Papuan", "Pima")]
  hgdp.path <- dijkstraBetween(hgdp.sub)

  expect_output(print(hgdp.path), "\\.\\.\\.")
})

test_that("plot.gPath draws the same segments as the previous implementation", {
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  sortRows <- function(d) {
    d <- d[do.call(order, d), ]
    rownames(d) <- NULL
    d
  }
  plot(worldgraph.40k, reset = TRUE, shape = NULL)
  ## paths that do not cross the antimeridian: same segments in the same order
  hgdp.sub <- hgdp[getData(hgdp)$Population %in% c("French", "Balochi", "BantuKenya", "Orcadian")]
  p <- dijkstraBetween(hgdp.sub)
  expect_identical(drawnSegments(plot(p, seed = 1)), drawnSegments(plotGPathRef(p, seed = 1)))
  expect_identical(
    drawnSegments(plot(p, col = c("red", "blue"), lwd = 1:3)),
    drawnSegments(plotGPathRef(p, col = c("red", "blue"), lwd = 1:3))
  )

  ## paths crossing the antimeridian: their split segments are drawn after
  ## the others, the set of segments is the same
  hgdp.sub <- hgdp[getData(hgdp)$Population %in% c("French", "Balochi", "BantuKenya", "Papuan", "Pima")]
  p <- dijkstraBetween(hgdp.sub)
  a <- drawnSegments(plot(p, seed = 1))
  b <- drawnSegments(plotGPathRef(p, seed = 1))
  expect_true(any(b$lty == "3"))
  expect_identical(sortRows(a), sortRows(b))
})
