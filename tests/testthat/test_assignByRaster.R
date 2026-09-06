# helper: build a small synthetic raster covering the same region as the graph
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

# shared objects — built once to avoid repeating slow calls
geo.box <- c(xmin = -10, xmax = 30, ymin = 35, ymax = 60)
test.graph <- makeHexGrid(geo.box, spacing = 1000)
test.raster <- make.test.raster(geo.box)

test.graph.w <- assignByRaster(test.graph, test.raster, fun = "mean")


#####################
## assignByRaster
#####################

test_that("assignByRaster errors if graph is not a gGraph", {
  expect_error(
    assignByRaster(graph = list(), raster = test.raster),
    "graph must be a gGraph object"
  )
  expect_error(
    assignByRaster(graph = "not_a_graph", raster = test.raster),
    "graph must be a gGraph object"
  )
})

test_that("assignByRaster returns a gGraph", {
  result <- assignByRaster(test.graph, test.raster)
  expect_s4_class(result, "gGraph")
})

test_that("assignByRaster creates a 'raster_points' column by default", {
  result <- assignByRaster(test.graph, test.raster)
  expect_true("raster_points" %in% colnames(result@nodes.attr))
})

test_that("assignByRaster respects a custom layer.name", {
  result <- assignByRaster(test.graph, test.raster, layer.name = "elevation")
  expect_true("elevation" %in% colnames(result@nodes.attr))
  expect_false("raster_points" %in% colnames(result@nodes.attr))
})

test_that("assignByRaster stores a scalar numeric column of correct length", {
  result <- assignByRaster(test.graph, test.raster)
  col <- result@nodes.attr$raster_points
  expect_true(is.numeric(col))
  expect_equal(length(col), nrow(test.graph@coords))
})

test_that("assignByRaster gives identical results on repeated calls", {
  result1 <- assignByRaster(test.graph, test.raster)
  result2 <- assignByRaster(test.graph, test.raster)
  expect_equal(result1@nodes.attr, result2@nodes.attr)
})

test_that("assignByRaster works with all built-in character functions", {
  for (fn in c("mean", "max", "min", "median", "sd")) {
    result <- assignByRaster(test.graph, test.raster, fun = fn)
    col <- result@nodes.attr$raster_points
    expect_true(is.numeric(col),
      info = paste("fun =", fn, "should return numeric")
    )
    expect_equal(length(col), nrow(test.graph@coords),
      info = paste("fun =", fn, "wrong length")
    )
  }
})

test_that("assignByRaster accepts a custom function", {
  result <- assignByRaster(
    test.graph, test.raster,
    fun = function(x, na.rm) sum(x, na.rm = na.rm)
  )
  col <- result@nodes.attr$raster_points
  expect_true(is.numeric(col))
  expect_equal(length(col), nrow(test.graph@coords))
})

test_that("assignByRaster errors on invalid fun", {
  expect_error(
    assignByRaster(test.graph, test.raster, fun = 123),
    "`fun` must be a function or a supported character string"
  )
  expect_error(
    assignByRaster(test.graph, test.raster, fun = "variance"),
    "should be one of"
  )
})

test_that("assignByRaster mean is correct for known uniform values", {
  uniform.raster <- make.test.raster(geo.box, vals = rep(5, terra::ncell(test.raster)))
  result <- assignByRaster(test.graph, uniform.raster, fun = "mean")
  col <- result@nodes.attr$raster_points
  expect_true(all(col == 5 | is.na(col)))
})

test_that("assignByRaster max equals min for uniform raster", {
  uniform.raster <- make.test.raster(geo.box, vals = rep(3, terra::ncell(test.raster)))
  result.max <- assignByRaster(test.graph, uniform.raster, fun = "max")
  result.min <- assignByRaster(test.graph, uniform.raster, fun = "min")
  expect_equal(
    result.max@nodes.attr$raster_points,
    result.min@nodes.attr$raster_points
  )
})

test_that("assignByRaster respects na.rm = TRUE", {
  vals <- rep(c(1, NA_real_), length.out = terra::ncell(test.raster))
  na.raster <- make.test.raster(geo.box, vals = vals)
  result <- assignByRaster(test.graph, na.raster, fun = "mean", na.rm = TRUE)
  col <- result@nodes.attr$raster_points
  expect_true(all(is.finite(col[!is.na(col)])))
})

test_that("assignByRaster correctly captures known raster values", {
  r <- terra::rast(
    xmin = 9, xmax = 11, ymin = 46, ymax = 48,
    resolution = 2, crs = "EPSG:4326"
  )
  terra::values(r) <- 42
  result <- assignByRaster(test.graph, r, fun = "mean")
  col <- result@nodes.attr$raster_points
  # exactly one node should have received value 42
  expect_equal(sum(col == 42, na.rm = TRUE), 1L)
})

test_that("assignByRaster works with logical built-ins any and all", {
  logical.raster <- make.test.raster(
    geo.box,
    vals = sample(c(0, 1), terra::ncell(test.raster), replace = TRUE)
  )
  for (fn in c("any", "all")) {
    result <- assignByRaster(test.graph, logical.raster, fun = fn)
    col <- result@nodes.attr$raster_points
    expect_true(is.logical(col) || is.numeric(col),
      info = paste("fun =", fn, "should return logical or numeric")
    )
    expect_equal(length(col), nrow(test.graph@coords),
      info = paste("fun =", fn, "wrong length")
    )
  }
})

test_that("assignByRaster errors on multi-layer raster", {
  multi.layer.raster <- c(test.raster, test.raster) # 2-layer raster
  expect_error(
    assignByRaster(test.graph, multi.layer.raster),
    "raster must have exactly one layer"
  )
})
