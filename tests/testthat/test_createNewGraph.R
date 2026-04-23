test_that("createNewGraph errors on invalid `spacing`", {
  geo_box <- c(xmin = -10, xmax = 30, ymin = 35, ymax = 60)

  expect_error(
    createNewGraph(geo_box, spacing = -1),
    "`spacing` must be a single positive numeric value"
  )

  expect_error(
    createNewGraph(geo_box, spacing = 0),
    "`spacing` must be a single positive numeric value"
  )

  expect_error(
    createNewGraph(geo_box, spacing = NA_real_),
    "`spacing` must be a single positive numeric value"
  )

  expect_error(
    createNewGraph(geo_box, spacing = "1000"),
    "`spacing` must be a single positive numeric value"
  )

  expect_error(
    createNewGraph(geo_box, spacing = c(500, 1000)),
    "`spacing` must be a single positive numeric value"
  )
})


test_that("createNewGraph errors on invalid `geo_box`", {
  # unnamed numeric vector
  expect_error(
    createNewGraph(c(-10, 30, 35, 60), spacing = 1000),
    "geo_box must be a bbox, sf object, or named numeric vector"
  )

  # wrong names
  expect_error(
    createNewGraph(c(left = -10, right = 30, bottom = 35, top = 60), spacing = 1000),
    "geo_box must be a bbox, sf object, or named numeric vector"
  )

  # non-numeric non-spatial object
  expect_error(
    createNewGraph(list(xmin = -10, xmax = 30, ymin = 35, ymax = 60), spacing = 1000),
    "geo_box must be a bbox, sf object, or named numeric vector"
  )
})


test_that("createNewGraph returns a gGraph with named numeric vector input", {
  geo_box <- c(xmin = -10, xmax = 30, ymin = 35, ymax = 60)
  result <- createNewGraph(geo_box, spacing = 1000)

  expect_s4_class(result, "gGraph")
})


test_that("createNewGraph returns a gGraph from a bbox input", {
  bbox <- sf::st_bbox(c(xmin = -10, xmax = 30, ymin = 35, ymax = 60),
    crs = sf::st_crs(4326)
  )
  result <- createNewGraph(bbox, spacing = 1000)

  expect_s4_class(result, "gGraph")
})


test_that("createNewGraph returns a gGraph from an sf input", {
  poly <- sf::st_sfc(
    sf::st_polygon(list(matrix(
      c(-10, 35, 30, 35, 30, 60, -10, 60, -10, 35),
      ncol = 2, byrow = TRUE
    ))),
    crs = 4326
  )
  sf_obj <- sf::st_sf(geometry = poly)
  result <- createNewGraph(sf_obj, spacing = 1000)

  expect_s4_class(result, "gGraph")
})


#test_that("createNewGraph@coords has correct structure", {
#  geo_box <- c(xmin = -10, xmax = 30, ymin = 35, ymax = 60)
#  result <- createNewGraph(geo_box, spacing = 1000)
#
#  coords <- result@coords
#  expect_true(nrow(coords) > 0)
#  expect_true(all(is.numeric(coords$lon)))
#  expect_true(all(is.numeric(coords$lat)))
#})


# test_that("createNewGraph@coords are within the supplied bounding box", {
#   geo_box <- c(xmin = -10, xmax = 30, ymin = 35, ymax = 60)
#   result <- createNewGraph(geo_box, spacing = 1000)
# 
#   coords <- result@coords
#   # allow a small margin since grid cell centres can sit on the boundary
#   expect_true(all(coords$lon >= geo_box["xmin"] - 1e-6))
#   expect_true(all(coords$lon <= geo_box["xmax"] + 1e-6))
#   expect_true(all(coords$lat >= geo_box["ymin"] - 1e-6))
#   expect_true(all(coords$lat <= geo_box["ymax"] + 1e-6))
# })


#test_that("createNewGraph@graphNEL node count matches coords rows", {
#  geo_box <- c(xmin = -10, xmax = 30, ymin = 35, ymax = 60)
#  result <- createNewGraph(geo_box, spacing = 1000)
#
#  n_coords <- nrow(result@coords)
#  n_nodes <- length(graph::nodes(result@graphNEL))
#  expect_equal(n_nodes, n_coords)
#})


#test_that("createNewGraph@neighbours length matches node count", {
#  geo_box <- c(xmin = -10, xmax = 30, ymin = 35, ymax = 60)
#  result <- createNewGraph(geo_box, spacing = 1000)
#
#  expect_equal(length(result@neighbours), nrow(result@coords))
#})


test_that("createNewGraph produces more nodes with smaller spacing", {
  geo_box <- c(xmin = -10, xmax = 30, ymin = 35, ymax = 60)

  result_coarse <- createNewGraph(geo_box, spacing = 2000)
  result_fine <- createNewGraph(geo_box, spacing = 500)

  expect_gt(nrow(result_fine@coords), nrow(result_coarse@coords))
})


#test_that("createNewGraph edges in graphNEL match neighbours slot", {
#  geo_box <- c(xmin = -10, xmax = 30, ymin = 35, ymax = 60)
#  result <- createNewGraph(geo_box, spacing = 1000)
#
#  node_ids <- graph::nodes(result@graphNEL)
#  edge_list <- graph::edges(result@graphNEL)
#  neighbours <- result@neighbours
#
#  for (i in seq_along(neighbours)) {
#    node <- node_ids[i]
#    expected_nb <- sort(node_ids[neighbours[[i]]])
#    actual_nb <- sort(edge_list[[node]])
#    expect_equal(actual_nb, expected_nb,
#      info = paste("Mismatch at node index", i)
#    )
#  }
#})
