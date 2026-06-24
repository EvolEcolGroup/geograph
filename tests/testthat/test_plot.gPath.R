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
