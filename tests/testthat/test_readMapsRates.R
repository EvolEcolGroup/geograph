mcmcpath <- system.file("extdata", "maps", package = "geoGraph")
mapsGraph <- readMapsGrid(mcmcpath)
log10m <- as.matrix(utils::read.table(file.path(mcmcpath, "mRates.txt")))
log10q <- as.matrix(utils::read.table(file.path(mcmcpath, "qRates.txt")))

attr.names <- c(
  "m_mean", "log10m_mean", "log10m_sd", "m_p_above", "m_sign",
  "log10N_mean", "log10N_sd", "N_p_above"
)


## node attributes
test_that("readMapsRates adds posterior summaries as node attributes", {
  result <- readMapsRates(mapsGraph, mcmcpath)
  attrs <- getNodesAttr(result)

  expect_s4_class(result, "gGraph")
  expect_true(validObject(result))
  expect_equal(names(attrs), c(attr.names, "m_colour"))
  expect_equal(rownames(attrs), getNodes(result))
  expect_equal(unname(attrs$m_mean), unname(colMeans(10^log10m)))
  expect_equal(unname(attrs$log10m_mean), unname(colMeans(log10m)))
  expect_equal(unname(attrs$log10N_mean), unname(colMeans(-log10(2) - log10q)))
})

test_that("readMapsRates computes p_above and the sign classes per draw", {
  attrs <- getNodesAttr(readMapsRates(mapsGraph, mcmcpath))
  # deme 1: below the map average in every draw; deme 4: above in every draw
  above <- sapply(seq_len(ncol(log10m)), function(j) {
    mean(log10m[, j] > rowMeans(log10m))
  })
  expect_equal(unname(attrs$m_p_above), above)
  expect_equal(attrs$m_sign[c(1, 4)], c("below", "above"))
  expect_true(all(attrs$m_sign[attrs$m_p_above >= 0.05 & attrs$m_p_above <= 0.95] == "uncertain"))
  # population size runs opposite to the coalescent rate
  expect_true(cor(attrs$log10N_mean, colMeans(log10q)) < -0.99)
})

test_that("readMapsRates keeps several runs apart with suffixes", {
  result <- readMapsRates(mapsGraph, mcmcpath, suffix = "2_5", costs = FALSE, colors = FALSE)
  result <- readMapsRates(result, mcmcpath, suffix = "10_Inf", costs = FALSE, colors = FALSE)
  expect_equal(
    names(getNodesAttr(result)),
    c(paste0(attr.names, "_2_5"), paste0(attr.names, "_10_Inf"))
  )
})


## costs and colors
test_that("readMapsRates sets edge costs to the inverse MAPS edge rate", {
  result <- readMapsRates(mapsGraph, mcmcpath)
  m <- getNodesAttr(result)$m_mean
  w <- graph::edgeWeights(getGraph(result))
  # edges 1-2 and 1-5 are in edges.txt
  expect_equal(unname(w[["1"]]["2"]), 2 / (m[1] + m[2]))
  expect_equal(unname(w[["1"]]["5"]), 2 / (m[1] + m[5]))
  # symmetric, and lower cost where migration is higher
  expect_equal(unname(w[["2"]]["1"]), unname(w[["1"]]["2"]))
  expect_equal(graph::numEdges(getGraph(result)), graph::numEdges(getGraph(mapsGraph)))

  unchanged <- readMapsRates(mapsGraph, mcmcpath, costs = FALSE)
  expect_true(all(unlist(graph::edgeWeights(getGraph(unchanged))) == 1))
})


## input validation
test_that("readMapsRates errors on bad inputs", {
  expect_error(readMapsRates(1, mcmcpath), "`x` must be a gGraph object")
  expect_error(readMapsRates(mapsGraph, ""), "`mcmcpath` is an empty string")
  expect_error(readMapsRates(mapsGraph, mcmcpath, suffix = ""), "`suffix` must be NULL")
  expect_error(readMapsRates(mapsGraph, mcmcpath, costs = NA), "must be TRUE or FALSE")

  dir <- tempfile("maps_")
  dir.create(dir)
  file.copy(file.path(mcmcpath, "qRates.txt"), dir)
  expect_error(readMapsRates(mapsGraph, dir), "does not contain mRates.txt")

  utils::write.table(log10m[, -1], file.path(dir, "mRates.txt"),
    row.names = FALSE, col.names = FALSE
  )
  expect_error(readMapsRates(mapsGraph, dir), "has 11 columns but `x` has 12 nodes")
})
