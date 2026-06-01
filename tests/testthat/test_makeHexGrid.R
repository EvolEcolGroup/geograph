test_that("makeHexGrid errors on invalid `spacing`", {
  geo.box <- c(xmin = -10, xmax = 30, ymin = 35, ymax = 60)

  expect_error(
    makeHexGrid(geo.box, spacing = -1),
    "`spacing` must be a single positive numeric value"
  )

  expect_error(
    makeHexGrid(geo.box, spacing = 0),
    "`spacing` must be a single positive numeric value"
  )

  expect_error(
    makeHexGrid(geo.box, spacing = NA_real_),
    "`spacing` must be a single positive numeric value"
  )

  expect_error(
    makeHexGrid(geo.box, spacing = "1000"),
    "`spacing` must be a single positive numeric value"
  )

  expect_error(
    makeHexGrid(geo.box, spacing = c(500, 1000)),
    "`spacing` must be a single positive numeric value"
  )
})


test_that("makeHexGrid errors on invalid `geo.box`", {
  # unnamed numeric vector
  expect_error(
    makeHexGrid(c(-10, 30, 35, 60), spacing = 1000),
    "geo.box must be a bbox, sf object, or named numeric vector"
  )

  # wrong names
  expect_error(
    makeHexGrid(c(left = -10, right = 30, bottom = 35, top = 60), spacing = 1000),
    "geo.box must be a bbox, sf object, or named numeric vector"
  )

  # non-numeric non-spatial object
  expect_error(
    makeHexGrid(list(xmin = -10, xmax = 30, ymin = 35, ymax = 60), spacing = 1000),
    "geo.box must be a bbox, sf object, or named numeric vector"
  )
})


test_that("makeHexGrid returns a gGraph with named numeric vector input", {
  geo.box <- c(xmin = -10, xmax = 30, ymin = 35, ymax = 60)
  result <- makeHexGrid(geo.box, spacing = 1000)

  expect_s4_class(result, "gGraph")
})


test_that("makeHexGrid returns a gGraph from a bbox input", {
  bbox <- sf::st_bbox(c(xmin = -10, xmax = 30, ymin = 35, ymax = 60),
    crs = sf::st_crs(4326)
  )
  result <- makeHexGrid(bbox, spacing = 1000)

  expect_s4_class(result, "gGraph")
})


test_that("makeHexGrid returns a gGraph from an sf input", {
  poly <- sf::st_sfc(
    sf::st_polygon(list(matrix(
      c(-10, 35, 30, 35, 30, 60, -10, 60, -10, 35),
      ncol = 2, byrow = TRUE
    ))),
    crs = 4326
  )
  sf_obj <- sf::st_sf(geometry = poly)
  result <- makeHexGrid(sf_obj, spacing = 1000)

  expect_s4_class(result, "gGraph")
})


test_that("makeHexGrid produces more nodes with smaller spacing", {
  geo.box <- c(xmin = -10, xmax = 30, ymin = 35, ymax = 60)

  result_coarse <- makeHexGrid(geo.box, spacing = 2000)
  result_fine <- makeHexGrid(geo.box, spacing = 500)

  expect_gt(nrow(result_fine@coords), nrow(result_coarse@coords))
})


test_that("makeHexGrid@coords lon and lat values are within spacing margin of bbox", {
  geo.box <- c(xmin = -10, xmax = 30, ymin = 35, ymax = 60)
  spacing <- 1000
  result <- makeHexGrid(geo.box, spacing = spacing)

  # convert spacing from km to degrees (approximate: 1 degree ~ 111 km)
  margin <- (spacing / 111) * 1.5

  expect_true(all(result@coords[, "lon"] >= geo.box["xmin"] - margin))
  expect_true(all(result@coords[, "lon"] <= geo.box["xmax"] + margin))
  expect_true(all(result@coords[, "lat"] >= geo.box["ymin"] - margin))
  expect_true(all(result@coords[, "lat"] <= geo.box["ymax"] + margin))
})

test_that("makeHexGrid@nodes.attr is an empty data.frame with correct row names", {
  geo.box <- c(xmin = -10, xmax = 30, ymin = 35, ymax = 60)
  result <- makeHexGrid(geo.box, spacing = 1000)

  expect_s3_class(result@nodes.attr, "data.frame")
  expect_equal(ncol(result@nodes.attr), 0L)
  expect_equal(rownames(result@nodes.attr), as.character(seq_len(nrow(result@coords))))
})

test_that("makeHexGrid@meta has costs and colors both NULL", {
  geo.box <- c(xmin = -10, xmax = 30, ymin = 35, ymax = 60)
  result <- makeHexGrid(geo.box, spacing = 1000)

  expect_null(result@meta$costs)
  expect_null(result@meta$colors)
})

test_that("the central node has 6 neighbours", {
  geo.box <- c(xmin = -10, xmax = 30, ymin = 35, ymax = 60)
  spacing <- 1000
  result <- makeHexGrid(geo.box, spacing = spacing)

  # Find the central node (closest to the center of the bbox)
  center_lon <- (geo.box["xmin"] + geo.box["xmax"]) / 2
  center_lat <- (geo.box["ymin"] + geo.box["ymax"]) / 2

  distances <- sqrt((result@coords[, "lon"] - center_lon)^2 +
    (result@coords[, "lat"] - center_lat)^2)

  central_node_index <- which.min(distances)

  # Get the neighbors of the central node
  neighbors <- result@graph@edgeL[[central_node_index]]

  expect_equal(length(neighbors$edges), 6)
})

test_that("no node has no neighbours", {
  geo.box <- c(xmin = -10, xmax = 30, ymin = 35, ymax = 60)
  spacing <- 100
  result <- makeHexGrid(geo.box, spacing = spacing)

  for (i in seq_along(result@graph@edgeL)) {
    neighbors <- result@graph@edgeL[[i]]
    expect_gt(length(neighbors$edges), 0)
  }
})

test_that("no node has has more than 6 neighbours", {
  geo.box <- c(xmin = -10, xmax = 30, ymin = 35, ymax = 60)
  spacing <- 100
  result <- makeHexGrid(geo.box, spacing = spacing)

  for (i in seq_along(result@graph@edgeL)) {
    neighbors <- result@graph@edgeL[[i]]
    expect_lte(length(neighbors$edges), 6)
  }
})

test_that("makeHexGrid neighbour relationships are symmetric", {
  geo.box <- c(xmin = -10, xmax = 30, ymin = 35, ymax = 60)
  result <- makeHexGrid(geo.box, spacing = 1000)

  neighbours <- result@graph@edgeL
  for (i in seq_along(neighbours)) {
    for (neighbor in neighbours[[i]]$edges) {
      expect_true(i %in% neighbours[[neighbor]]$edges)
    }
  }
})

test_that("makeHexGrid neighbours are correct across the dateline", {
  ## region crossing the antimeridian
  geo.box <- c(xmin = 110, xmax = -150, ymin = -30, ymax = 30)
  result  <- makeHexGrid(geo.box, spacing = 500)
  
  # check if all nodes are connected
  for (i in seq_along(result@graph@edgeL)) {
    expect_gt(length(result@graph@edgeL[[i]]$edges), 0)
  }
  
  # no node should have more than 6 neighbours 
  for (i in seq_along(result@graph@edgeL)) {
    expect_lte(length(result@graph@edgeL[[i]]$edges), 6)
  }
  
  # check if dateline is briged
  coords <- result@coords
  bridges.dateline <- FALSE
  for (i in seq_along(result@graph@edgeL)) {
    for (j in result@graph@edgeL[[i]]$edges) {
      lon.i <- coords[i, "lon"]
      lon.j <- coords[as.integer(j), "lon"]
      if (lon.i > 150 && lon.j < -150) bridges.dateline <- TRUE
      if (lon.i < -150 && lon.j > 150) bridges.dateline <- TRUE
    }
  }
  expect_true(bridges.dateline)
})
