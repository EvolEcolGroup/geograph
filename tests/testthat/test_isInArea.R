test_that("isInArea with explicit reg list gives correct nodes", {
  europe <- list(x = c(-6, 38), y = c(35, 73))
  result <- isInArea(worldgraph.10k, reg = europe, quiet = TRUE)
  expect_true(is.logical(result))
  expect_true(sum(result) > 0)
  # all returned nodes should be within the bounding box
  coords <- getCoords(worldgraph.10k)[result, ]
  expect_true(all(coords[, "lon"] >= -6 & coords[, "lon"] <= 38))
  expect_true(all(coords[, "lat"] >= 35 & coords[, "lat"] <= 73))
})

test_that("isInArea errors when reg list has wrong length", {
  expect_error(
    isInArea(worldgraph.10k, reg = list(x = c(-6, 38)), quiet = TRUE),
    "reg is not a list of length 2"
  )
})

test_that("isInArea returns NA for invalid reg", {
  result <- isInArea(getCoords(worldgraph.10k), reg = "invalid")
  expect_true(is.na(result))
})

test_that("isInArea works with gData input and returns correct nodes", {
  europe <- list(x = c(-6, 38), y = c(35, 73))
  result <- isInArea(hgdp, reg = europe, quiet = TRUE)
  expect_true(is.logical(result))
  expect_equal(length(result), length(getNodes(hgdp)))
  # all nodes within the bbox should have coordinates inside it
  coords <- getCoords(hgdp)[result, ]
  expect_true(all(coords[, "lon"] >= -6 & coords[, "lon"] <= 38))
  expect_true(all(coords[, "lat"] >= 35 & coords[, "lat"] <= 73))
})

test_that("isInArea works with data.frame input and returns correct nodes", {
  coords.df  <- as.data.frame(getCoords(worldgraph.10k))
  europe     <- list(x = c(-6, 38), y = c(35, 73))
  result.df  <- isInArea(coords.df,            reg = europe, quiet = TRUE)
  result.mat <- isInArea(as.matrix(coords.df), reg = europe, quiet = TRUE)
  # data.frame and matrix methods should give identical results
  expect_equal(result.df, result.mat)
  # all nodes within the bbox should have coordinates inside it
  coords.in <- coords.df[result.df, ]
  expect_true(all(coords.in[, "lon"] >= -6 & coords.in[, "lon"] <= 38))
  expect_true(all(coords.in[, "lat"] >= 35 & coords.in[, "lat"] <= 73))
})

test_that("isInArea gives same nodes from zoom device and explicit bbox", {
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  
  ## zoom into Europe on the device
  plot(worldgraph.10k, reset = TRUE)
  geo.zoomin(list(x = c(-6, 38), y = c(35, 73)))
  
  ## get nodes from current plot and capture the reproducible call
  msg <- capture.output(
    result.device <- isInArea(worldgraph.10k, reg = "current"),
    type = "message"
  )
  
  ## extract the reg = list(...) part from the message and evaluate it
  reg.line <- msg[grepl("Reproducible call:", msg)][1]
  reg.string <- sub(".*Reproducible call: ", "", reg.line)
  reg.bbox   <- eval(parse(text = reg.string))
  
  ## get nodes using the reproducible call from the message
  result.bbox <- isInArea(worldgraph.10k, reg = reg.bbox, quiet = TRUE)
  
  expect_equal(result.device, result.bbox)
})
