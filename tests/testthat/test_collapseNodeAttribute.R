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
geo.box      <- c(xmin = -10, xmax = 30, ymin = 35, ymax = 60)
test.graph   <- createNewGraph(geo.box, spacing = 1000)
test.raster  <- make.test.raster(geo.box)
test.graph.w <- assignRasterPoints(test.graph, test.raster)


test_that("collapseNodeAttribute errors if graph is not a gGraph", {
  expect_error(
    collapseNodeAttribute(list(), attribute = "raster_points"),
    "`graph` must be a gGraph object"
  )
})

test_that("collapseNodeAttribute errors if attribute does not exist", {
  expect_error(
    collapseNodeAttribute(test.graph.w, attribute = "nonexistent"),
    "Node attribute 'nonexistent' not found"
  )
})

test_that("collapseNodeAttribute errors if attribute is not a list", {
  # add a scalar attribute and try to collapse it
  graph.scalar <- test.graph.w
  graph.scalar@nodes.attr[["scalar"]] <- rep(1, nrow(test.graph.w@coords))
  expect_error(
    collapseNodeAttribute(graph.scalar, attribute = "scalar"),
    "Selected node attribute must be a list"
  )
})

test_that("collapseNodeAttribute errors if fun is not a function or valid string", {
  expect_error(
    collapseNodeAttribute(test.graph.w, attribute = "raster_points", fun = 123),
    "`fun` must be a function or a supported character string"
  )
  expect_error(
    collapseNodeAttribute(test.graph.w, attribute = "raster_points", fun = "variance"),
    "should be one of"
  )
})


test_that("collapseNodeAttribute returns a gGraph when replace = TRUE", {
  result <- collapseNodeAttribute(test.graph.w, attribute = "raster_points")
  expect_s4_class(result, "gGraph")
})

test_that("collapseNodeAttribute replaces list column with a vector when replace = TRUE", {
  result <- collapseNodeAttribute(test.graph.w, attribute = "raster_points")
  col    <- result@nodes.attr[["raster_points"]]
  
  expect_false(is.list(col))
  expect_true(is.numeric(col) || is.logical(col))
})

test_that("collapseNodeAttribute collapsed vector has one value per node", {
  result <- collapseNodeAttribute(test.graph.w, attribute = "raster_points")
  col    <- result@nodes.attr[["raster_points"]]
  
  expect_equal(length(col), nrow(test.graph.w@coords))
})



test_that("collapseNodeAttribute returns a vector when replace = FALSE", {
  result <- collapseNodeAttribute(
    test.graph.w,
    attribute = "raster_points",
    replace   = FALSE
  )
  expect_true(is.numeric(result) || is.logical(result))
  expect_equal(length(result), nrow(test.graph.w@coords))
})

test_that("collapseNodeAttribute does not modify the graph when replace = FALSE", {
  result <- collapseNodeAttribute(
    test.graph.w,
    attribute = "raster_points",
    replace   = FALSE
  )
  # original list column should still be a list
  expect_true(is.list(test.graph.w@nodes.attr[["raster_points"]]))
})



test_that("collapseNodeAttribute works with all built-in character functions", {
  for (fn in c("mean", "max", "min", "median", "sd")) {
    result <- collapseNodeAttribute(
      test.graph.w,
      attribute = "raster_points",
      fun       = fn,
      replace   = FALSE
    )
    expect_true(is.numeric(result),
                info = paste("fun =", fn, "should return numeric"))
    expect_equal(length(result), nrow(test.graph.w@coords),
                 info = paste("fun =", fn, "wrong length"))
  }
})

test_that("collapseNodeAttribute works with logical built-ins any and all", {
  # assign a logical raster so any/all make sense
  logical.raster <- make.test.raster(
    geo.box,
    vals = sample(c(TRUE, FALSE), terra::ncell(test.raster), replace = TRUE)
  )
  graph.logical <- assignRasterPoints(test.graph, logical.raster)
  
  for (fn in c("any", "all")) {
    result <- collapseNodeAttribute(
      graph.logical,
      attribute = "raster_points",
      fun       = fn,
      replace   = FALSE
    )
    expect_true(is.logical(result),
                info = paste("fun =", fn, "should return logical"))
    expect_equal(length(result), nrow(test.graph@coords),
                 info = paste("fun =", fn, "wrong length"))
  }
})

test_that("collapseNodeAttribute accepts a custom function", {
  result <- collapseNodeAttribute(
    test.graph.w,
    attribute = "raster_points",
    fun       = function(x, na.rm) sum(x, na.rm = na.rm),
    replace   = FALSE
  )
  expect_true(is.numeric(result))
  expect_equal(length(result), nrow(test.graph.w@coords))
})



test_that("collapseNodeAttribute mean is correct for known uniform values", {
  uniform.raster <- make.test.raster(geo.box, vals = rep(5, terra::ncell(test.raster)))
  graph.uniform  <- assignRasterPoints(test.graph, uniform.raster)
  
  result <- collapseNodeAttribute(
    graph.uniform,
    attribute = "raster_points",
    fun       = "mean",
    replace   = FALSE
  )
  # every node that received at least one cell should have mean == 5
  expect_true(all(result == 5 | is.na(result)))
})

test_that("collapseNodeAttribute max equals min for uniform raster", {
  uniform.raster <- make.test.raster(geo.box, vals = rep(3, terra::ncell(test.raster)))
  graph.uniform  <- assignRasterPoints(test.graph, uniform.raster)
  
  result.max <- collapseNodeAttribute(graph.uniform, "raster_points", fun = "max", replace = FALSE)
  result.min <- collapseNodeAttribute(graph.uniform, "raster_points", fun = "min", replace = FALSE)
  
  expect_equal(result.max, result.min)
})


test_that("collapseNodeAttribute respects na.rm = TRUE", {
  # mix of real values and NAs — more realistic than all-NA
  vals <- rep(c(1, NA_real_), length.out = terra::ncell(test.raster))
  na.raster <- make.test.raster(geo.box, vals = vals)
  graph.na  <- assignRasterPoints(test.graph, na.raster)
  
  result <- collapseNodeAttribute(
    graph.na, "raster_points",
    fun    = "mean",
    na.rm  = TRUE,
    replace = FALSE
  )
  
  # na.rm = TRUE means non-NA values should produce a real number, not NaN
  non.na.results <- result[!is.na(result)]
  expect_true(all(is.finite(non.na.results)))
})

