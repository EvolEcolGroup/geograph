## setup
hgdp.sub  <- hgdp[getData(hgdp)$Population %in%
                    c("French", "Balochi", "BantuKenya", "Papuan", "Pima")]
hgdp.path <- dijkstraBetween(hgdp.sub)

test_that("plot.gPath works with custom color and lwd", {
  pdf(NULL)
  expect_no_error(plot(hgdp.path, col = "blue", lwd = 1))
  dev.off()
})

test_that("plot.gPath handles single-node paths without error", {
  # self-path has length_detail NA and path of length 1 — should silently skip
  hgdp.same <- hgdp[getData(hgdp)$Region == "MIDDLE_EAST"]
  pdf(NULL)
  expect_no_error(plot(dijkstraBetween(hgdp.same)))
  dev.off()
})

## --- print ---
test_that("print.gPath outputs correct number of paths", {
  expect_output(print(hgdp.path), "number of paths: 10")
})

test_that("print.gPath errors with extra arguments", {
  expect_error(
    print(hgdp.path, extra = "arg"),
    "additional parameters were passed"
  )
})

test_that("print.gPath shows truncated path names for long gPath", {
  expect_output(print(hgdp.path), "\\.\\.\\.")
})