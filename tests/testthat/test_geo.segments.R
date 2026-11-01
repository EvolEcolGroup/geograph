test_that("geo.segments keeps per-segment colours with their segments", {
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  plot(0, xlim = c(-180, 180), ylim = c(-90, 90))

  # record each call to graphics::segments
  rec <- new.env()
  rec$calls <- list()
  trace("segments",
    bquote(assign("calls",
      c(get("calls", envir = .(rec)), list(list(x0 = x0, col = col))),
      envir = .(rec)
    )),
    print = FALSE, where = graphics::plot.default
  )
  on.exit(untrace("segments", where = graphics::plot.default), add = TRUE)

  # the first segment crosses the antimeridian, the second does not
  geo.segments(
    x0 = c(-170, 0), y0 = c(0, 0), x1 = c(170, 10), y1 = c(0, 0),
    col = c("red", "blue")
  )
  expect_length(rec$calls, 2L)
  expect_equal(rec$calls[[1]]$col, "blue") # ordinary segment
  expect_equal(rec$calls[[2]]$col, c("red", "red")) # two halves of the crossing one
})
