## example MAPS grid shipped with the package (inst/extdata/maps)
mcmcpath <- system.file("extdata", "maps", package = "geoGraph")
demes <- utils::read.table(file.path(mcmcpath, "demes.txt"))
edges <- utils::read.table(file.path(mcmcpath, "edges.txt"))

## helper: copy of the example grid in a temporary directory, with demes.txt
## and/or edges.txt replaced
modifiedGrid <- function(new.demes = demes, new.edges = edges) {
  dir <- tempfile("maps_")
  dir.create(dir)
  utils::write.table(new.demes, file.path(dir, "demes.txt"), row.names = FALSE, col.names = FALSE)
  utils::write.table(new.edges, file.path(dir, "edges.txt"), row.names = FALSE, col.names = FALSE)
  dir
}


## structure
test_that("readMapsGrid builds a gGraph from the example MAPS output", {
  result <- readMapsGrid(mcmcpath)
  expect_s4_class(result, "gGraph")
  expect_true(validObject(result))
  expect_equal(getNodes(result), as.character(seq_len(nrow(demes))))
  expect_equal(graph::numEdges(getGraph(result)), nrow(edges))
})

test_that("readMapsGrid connects the demes listed in edges.txt, and only those", {
  result <- readMapsGrid(mcmcpath)
  # edges.txt starts with 1-2 and 1-5; 1-3 is not listed
  expect_true(areNeighbours("1", "2", getGraph(result)))
  expect_true(areNeighbours("5", "1", getGraph(result)))
  expect_false(areNeighbours("1", "3", getGraph(result)))
  expect_true(all(unlist(graph::edgeWeights(getGraph(result))) == 1))
})

test_that("readMapsGrid converts 0-360 longitudes to -180..180", {
  result <- readMapsGrid(mcmcpath)
  # the example grid crosses the antimeridian
  expect_true(any(demes[[1]] > 180))
  expected.lon <- ifelse(demes[[1]] > 180, demes[[1]] - 360, demes[[1]])
  expect_equal(unname(getCoords(result)[, "lon"]), expected.lon)
  expect_equal(unname(getCoords(result)[, "lat"]), demes[[2]])
})

test_that("readMapsGrid keeps duplicated or reversed edges once", {
  doubled <- rbind(edges, edges[1:2, 2:1])
  expect_message(
    result <- readMapsGrid(modifiedGrid(new.edges = doubled)),
    "2 duplicated edge"
  )
  expect_equal(graph::numEdges(getGraph(result)), nrow(edges))
})


## input validation
test_that("readMapsGrid errors on bad inputs", {
  n <- nrow(demes)
  
  expect_error(readMapsGrid(c("a", "b")), "`mcmcpath` must be a single character string")
  expect_error(readMapsGrid(1), "`mcmcpath` must be a single character string")
  expect_error(readMapsGrid(""), "directory not found: ")

  no.edges <- modifiedGrid()
  file.remove(file.path(no.edges, "edges.txt"))
  expect_error(readMapsGrid(no.edges), "does not contain edges.txt")
  
  expect_error(
    readMapsGrid(modifiedGrid(new.demes = cbind(demes, 1))),
    "demes.txt must have two numeric columns"
  )
  expect_error(
    readMapsGrid(modifiedGrid(new.edges = rbind(edges, c(1, n + 1)))),
    paste0("outside 1..", n)
  )
  expect_error(
    readMapsGrid(modifiedGrid(new.edges = rbind(edges, c(0, 1)))),
    paste0("outside 1..", n)
  )
  expect_error(
    readMapsGrid(modifiedGrid(new.edges = rbind(edges, c(2, 2)))),
    "from a deme to itself"
  )
  expect_error(
    readMapsGrid(modifiedGrid(new.edges = rbind(edges, c(1, 2.5)))),
    "non-integer"
  )
})
