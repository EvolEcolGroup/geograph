mcmcpath <- system.file("extdata", "maps", package = "geoGraph")
ipmap <- scan(file.path(mcmcpath, "ipmap.txt"), quiet = TRUE)

modified_ipmap <- function(new.ipmap) {
  dir <- tempfile("maps_")
  dir.create(dir)
  file.copy(file.path(mcmcpath, c("demes.txt", "edges.txt")), dir)
  writeLines(as.character(new.ipmap), file.path(dir, "ipmap.txt"))
  dir
}

mapsGraphTest <- readMapsGrid(mcmcpath)
assign("mapsGraphTest", readMapsGrid(mcmcpath), envir = .GlobalEnv)

## structure
test_that("readMapsSamples builds a gData with one location per sampled deme", {
  result <- readMapsSamples(mapsGraphTest, mcmcpath)
  demes <- sort(unique(ipmap))

  expect_s4_class(result, "gData")
  expect_true(validObject(result))
  expect_equal(result@gGraph.name, "mapsGraphTest")
  expect_equal(result@nodes.id, as.character(demes))
  expect_equal(getData(result)$deme, demes)
  expect_equal(getData(result)$n_haploids, as.vector(table(ipmap)))
  expect_equal(sum(getData(result)$n_haploids), length(ipmap))
})

test_that("readMapsSamples places samples on the coordinates of their demes", {
  result <- readMapsSamples(mapsGraphTest, mcmcpath)
  node.coords <- getCoords(mapsGraphTest)[result@nodes.id, ]
  expect_equal(unname(result@coords), unname(node.coords))
})

## input validation
test_that("readMapsSamples errors on bad inputs", {
  expect_error(readMapsSamples(1, mcmcpath), "`x` must be a gGraph object")
  expect_error(readMapsSamples(mapsGraphTest, ""), "`mcmcpath` is an empty string")
  expect_error(
    readMapsSamples(readMapsGrid(mcmcpath), mcmcpath),
    "`x` must be a gGraph object stored "
  )
  expect_error(
    readMapsSamples(mapsGraphTest, modified_ipmap(c(ipmap, 13))),
    "outside 1..12"
  )
  expect_error(
    readMapsSamples(mapsGraphTest, modified_ipmap(c(ipmap, 1.5))),
    "integer deme indices"
  )
  no.ipmap <- modified_ipmap(ipmap)
  file.remove(file.path(no.ipmap, "ipmap.txt"))
  expect_error(readMapsSamples(mapsGraphTest, no.ipmap), "does not contain ipmap.txt")
})

rm("mapsGraphTest", envir = .GlobalEnv)
