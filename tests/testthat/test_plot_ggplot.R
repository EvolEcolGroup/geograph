## tests/testthat/test-plot_ggplot.R

test_that(".ggraphNodesDf returns node_id, lon, lat, and attributes", {
  df <- .ggraphNodesDf(worldgraph.40k)
  expect_true(all(c("node_id", "lon", "lat", "habitat") %in% colnames(df)))
  expect_equal(nrow(df), length(getNodes(worldgraph.40k)))
})

test_that(".ggraphEdgesDf returns endpoint coords matching the graph", {
  df <- .ggraphEdgesDf(worldgraph.40k)
  expect_true(all(c("from", "to", "x", "y", "xend", "yend") %in% colnames(df)))
  co <- getCoords(worldgraph.40k)
  expect_equal(df$x[1], unname(co[df$from[1], 1]))
  expect_equal(df$yend[1], unname(co[df$to[1], 2]))
})

test_that(".gdataDf defaults to node coords, switches with original = TRUE", {
  df_nodes <- .gdataDf(hgdp)
  df_orig  <- .gdataDf(hgdp, original = TRUE)
  expect_true(all(c("node_id", "lon", "lat") %in% colnames(df_nodes)))
  expect_equal(nrow(df_nodes), length(hgdp@nodes.id))
  ## node coords differ from original sample coords in general
  expect_false(isTRUE(all.equal(df_nodes$lon, df_orig$lon)))
})

test_that(".gpathDf has monotone order within each path", {
  addis <- closestNode(worldgraph.40k, list(lon = 38.74, lat = 9.03))
  paths <- dijkstraFrom(hgdp[1:3], addis)
  df <- .gpathDf(paths)
  expect_true(all(c("path_id", "order", "lon", "lat") %in% colnames(df)))
  ## order increases by 1 within each path
  ok <- by(df, df$path_id, function(s) all(diff(s$order) == 1))
  expect_true(all(unlist(ok)))
})

test_that("sf helpers return correct geometry types with CRS 4326", {
  pts <- .dfToSfPoints(data.frame(lon = c(0, 10), lat = c(50, 60)))
  expect_s3_class(pts, "sf")
  expect_equal(sf::st_crs(pts)$epsg, 4326L)
  expect_true(all(sf::st_geometry_type(pts) == "POINT"))
  
  lines <- .dfToSfLines(.ggraphEdgesDf(worldgraph.40k)[1:5, ])
  expect_true(all(sf::st_geometry_type(lines) == "LINESTRING"))
  expect_equal(nrow(lines), 5L)
})

test_that("geoms return ggplot layers; edges toggle changes layer count", {
  expect_length(geom_ggraph(data = worldgraph.40k, edges = TRUE), 2L)
  expect_length(geom_ggraph(data = worldgraph.40k, edges = FALSE), 1L)
})

test_that("geoms error on missing or wrong-class data", {
  expect_error(geom_ggraph(), "specified")
  expect_error(geom_ggraph(data = list()), "gGraph")
  expect_error(geom_gdata(data = list()), "gData")
  expect_error(geom_gpath(data = list()), "gPath")
})

test_that("autoplot methods return ggplot objects in both modes", {
  expect_s3_class(autoplot(worldgraph.40k), "ggplot")
  expect_s3_class(autoplot(worldgraph.40k, mode = "orthographic"), "ggplot")
  expect_s3_class(autoplot(hgdp), "ggplot")
})

