# Set up a subset for testing
hgdp.sub <- hgdp[c(24, 1, 13, 27), ]

test_that("assignByPolygon assigns points correctly", {
  # create a matrix of locations including two continents and the sea
  myCoords <- data.frame(long = c(-24, 71.5, -46.5), lat = c(31, 30, -23.5))
  # assign to continents
  continents <- assignByPolygon(myCoords, layer = "world", attr = "continent")
  expect_identical(as.character(continents$continent), c(NA, "Asia", "South America"))
})


test_that("assignByPolygon works on a gData object", {
  res <- assignByPolygon(hgdp.sub, layer = "world", attr = "continent")
  expect_true(inherits(res, "gData"))

  # we expect continents to be "AMERICA", "EUROPE", "CENTRAL_SOUTH_ASIA",  "AFRICA"
  expect_identical(as.character(res@data$continent), c("South America", "Europe", "Asia", "Africa"))
})

test_that("assignByPolygon works on a list", {
  # Create a list
  hgdp.sub.list <- list(hgdp.sub@coords)

  # we expect continents to be "AMERICA", "EUROPE", "CENTRAL_SOUTH_ASIA",  "AFRICA"
  res <- assignByPolygon(hgdp.sub.list, layer = "world", attr = "continent")
  expect_identical(as.character(res$continent), c("South America", "Europe", "Asia", "Africa"))
})

test_that("assignByPolygon works with a single line input", {
  # works for one line
  south.america <- hgdp.sub@coords[1, ]
  res <- assignByPolygon(south.america, layer = "world", attr = "continent")
  expect_identical(as.character(res$continent), c("South America"))

  # returns error if missing element in coordinate pairs

  # If we enter a vector of three values
  hgdp.sub.odd <- hgdp.sub@coords[c(1, 2, 3), 1]

  expect_error(
    res <- assignByPolygon(hgdp.sub.odd, layer = "world", attr = "continent"),
    "Vector must have even number of longitude and latitude entries"
  )
})

test_that("assignByPolygon errors when layer is NULL", {
  myCoords <- data.frame(lon = c(2, 10), lat = c(48, 51))
  expect_error(
    assignByPolygon(myCoords, layer = NULL),
    "layer must not be NULL"
  )
})

test_that("assignByPolygon errors when layer is not an sf object", {
  myCoords <- data.frame(lon = c(2, 10), lat = c(48, 51))
  expect_error(
    assignByPolygon(myCoords, layer = data.frame(x = 1)),
    "Layer must be a sf object"
  )
})

test_that("assignByPolygon adds attribute to gGraph nodes.attr", {
  res <- assignByPolygon(worldgraph.10k, layer = "world", attr = "continent")
  expect_s4_class(res, "gGraph")
  expect_true("continent" %in% colnames(getNodesAttr(res)))
  expect_equal(nrow(getNodesAttr(res)), length(getNodes(worldgraph.10k)))
})

test_that("assignByPolygon restores sf_use_s2 state", {
  myCoords <- data.frame(lon = c(2, 10), lat = c(48, 51))
  sf::sf_use_s2(TRUE)
  assignByPolygon(myCoords, layer = "world", attr = "continent")
  expect_true(sf::sf_use_s2())

  sf::sf_use_s2(FALSE)
  assignByPolygon(myCoords, layer = "world", attr = "continent")
  expect_false(sf::sf_use_s2())

  sf::sf_use_s2(TRUE)
})

test_that("assignByPolygon works with attr = 'all'", {
  myCoords <- data.frame(lon = c(2, 10), lat = c(48, 51))
  res <- assignByPolygon(myCoords, layer = "world", attr = "all")
  expect_true(is.data.frame(res))
  expect_true(ncol(res) > 10)
})

test_that("assignByPolygon warns and returns NA when no points in any polygon", {
  ocean.coords <- data.frame(lon = c(-30, -40), lat = c(0, 10))
  expect_warning(
    res <- assignByPolygon(ocean.coords, layer = "world", attr = "continent"),
    "No points were assigned to any polygon"
  )
  expect_true(all(is.na(res$continent)))
})

test_that("assignByPolygon works on gData with NULL data slot", {
  ## create a gData with no data
  coords.dat <- data.frame(lon = c(2, 10), lat = c(48, 51))
  gdat <- new("gData", coords = coords.dat)
  gdat@gGraph.name <- "worldgraph.10k"
  gdat <- closestNode(gdat)
  res <- assignByPolygon(gdat, layer = "world", attr = "continent")
  expect_s4_class(res, "gData")
  expect_false(is.null(res@data))
})

test_that("assignByPolygon returns correct number of rows when no points in any polygon", {
  ocean.coords <- data.frame(lon = c(-30, -40, -35), lat = c(0, 10, 5))
  expect_warning(
    res <- assignByPolygon(ocean.coords, layer = "world", attr = "continent"),
    "No points were assigned to any polygon"
  )
  expect_equal(nrow(res), nrow(ocean.coords))
  expect_true(all(is.na(res$continent)))
})

test_that("assignByPolygon handles duplicate coordinates correctly", {
  dupes <- data.frame(lon = c(2, 2), lat = c(48, 48))
  res <- assignByPolygon(dupes, layer = "world", attr = "continent")
  expect_equal(nrow(res), 2L)
  expect_equal(res$continent[1], res$continent[2])
})
