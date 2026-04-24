# build a small synthetic raster covering the same region as the graph
make.test.raster <- function(geo.box, res = 2, vals = NULL) {
  r <- terra::rast(
    xmin = geo.box["xmin"], xmax = geo.box["xmax"],
    ymin = geo.box["ymin"], ymax = geo.box["ymax"],
    resolution = res,
    crs = "EPSG:4326"
  )
  terra::values(r) <- if (is.null(vals)) runif(terra::ncell(r)) else vals
  r
}

# shared objects built once so the slow createNewGraph call is not repeated
geo.box     <- c(xmin = -10, xmax = 30, ymin = 35, ymax = 60)
test.graph  <- createNewGraph(geo.box, spacing = 1000)
test.raster <- make.test.raster(geo.box)

test_that("assignRasterPoints errors if graph is not a gGraph", {
  expect_error(
    assignRasterPoints(graph = list(), raster = test.raster),
    "graph must be a gGraph object"
  )
  expect_error(
    assignRasterPoints(graph = "not_a_graph", raster = test.raster),
    "graph must be a gGraph object"
  )
})


test_that("assignRasterPoints returns a gGraph", {
  result <- assignRasterPoints(test.graph, test.raster)
  expect_s4_class(result, "gGraph")
})


test_that("assignRasterPoints creates a 'raster_points' column by default", {
  result <- assignRasterPoints(test.graph, test.raster)
  expect_true("raster_points" %in% colnames(result@nodes.attr))
})

test_that("assignRasterPoints respects a custom layer.name", {
  result <- assignRasterPoints(test.graph, test.raster, layer.name = "elevation")
  expect_true("elevation" %in% colnames(result@nodes.attr))
  expect_false("raster_points" %in% colnames(result@nodes.attr))
})

test_that("assignRasterPoints stores a list-column of the correct length", {
  result <- assignRasterPoints(test.graph, test.raster)
  col    <- result@nodes.attr[["raster_points"]]
  
  expect_type(col, "list")
  expect_equal(length(col), nrow(test.graph@coords))
})

test_that("assignRasterPoints list-column entries are data.frames", {
  result <- assignRasterPoints(test.graph, test.raster)
  col    <- result@nodes.attr[["raster_points"]]
  
  # every entry should be either a data.frame or NULL (for nodes with no raster
  # points assigned, left_join produces NA which becomes NULL after nesting)
  non.null <- Filter(Negate(is.null), col)
  expect_true(all(vapply(non.null, is.data.frame, logical(1))))
})

test_that("assignRasterPoints list-column entries have a 'value' column", {
  result   <- assignRasterPoints(test.graph, test.raster)
  col      <- result@nodes.attr[["raster_points"]]
  non.null <- Filter(Negate(is.null), col)
  
  expect_true(all(vapply(non.null, function(d) "value" %in% colnames(d), logical(1))))
})


test_that("assignRasterPoints assigns every raster cell to some node", {
  result      <- assignRasterPoints(test.graph, test.raster)
  col         <- result@nodes.attr[["raster_points"]]
  total.assigned <- sum(vapply(col, function(d) {
    if (is.null(d) || !is.data.frame(d)) 0L else nrow(d)
  }, integer(1)))
  
  expect_equal(total.assigned, terra::ncell(test.raster))
})


test_that("assignRasterPoints gives identical results on repeated calls", {
  result1 <- assignRasterPoints(test.graph, test.raster)
  result2 <- assignRasterPoints(test.graph, test.raster)
  
  expect_equal(result1@nodes.attr, result2@nodes.attr)
})


test_that("assignRasterPoints correctly captures known raster values", {
  # single-cell raster right in the middle of the bbox — must go to exactly one node
  r <- terra::rast(
    xmin = 9, xmax = 11,
    ymin = 46, ymax = 48,
    resolution = 2,
    crs = "EPSG:4326"
  )
  terra::values(r) <- 42
  
  result <- assignRasterPoints(test.graph, r)
  col    <- result@nodes.attr[["raster_points"]]
  
  # exactly one node should have received the value 42
  node.with.value <- Filter(function(d) {
    is.data.frame(d) && nrow(d) > 0 && any(d$value == 42)
  }, col)
  
  expect_equal(length(node.with.value), 1L)
})
