test_that("as.gGraph correctly imports a SpatRaster",{
  # expect error if we don't provide a SpatRaster
  expect_error(
    as.gGraph("not a SpatRaster"),
    "x must be a terra SpatRaster object"
  )
  # creat a spatraster
  set.seed(1)
  raster <- terra::rast(matrix(rbinom(size = 1, n=100,prob = 0.5),
                              nrow=10, ncol=10), crs="+proj=utm +zone=1 +datum=WGS84")
  names(raster) <- "land_mask"
  test_ggraph <- as.gGraph(raster)
  expect_true(inherits(test_ggraph, "gGraph"))
  
})