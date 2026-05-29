test_that("plot.gPath works with custom color and lwd", {
  hgdp.sub <- hgdp[getData(hgdp)$Population %in%
    c("French", "Balochi", "BantuKenya", "Papuan", "Pima")]
  hgdp.path <- dijkstraBetween(hgdp.sub)

  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  plot(worldgraph.40k)
  expect_no_error(plot(hgdp.path, col = "blue", lwd = 1))
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
