test_that("gPath2dist errors if m is not a gPath object", {
  expect_error(gPath2dist(list()), "m is not a gPath object")
  expect_error(gPath2dist("not_a_gpath"), "m is not a gPath object")
})

test_that("gPath2dist returns a dist object by default", {
  hgdp.sub <- hgdp[c(1, 2, 3, 4), ]
  hgdp.path <- dijkstraBetween(hgdp.sub)
  result <- gPath2dist(hgdp.path)

  expect_s3_class(result, "dist")
})

test_that("gPath2dist dist object has correct size", {
  # 4 locations -> 4*(4-1)/2 = 6 pairwise distances
  hgdp.sub <- hgdp[c(1, 2, 3, 4), ]
  hgdp.path <- dijkstraBetween(hgdp.sub)
  result <- gPath2dist(hgdp.path)

  expect_equal(attr(result, "Size"), 4L)
  expect_equal(length(result), 6L)
})

test_that("gPath2dist returns a named vector for a djikstraFrom output", {
  hgdp.sub <- hgdp[c(1, 2, 3, 4), ]
  hgdp.path <- dijkstraFrom(hgdp.sub, "24988")
  result <- gPath2dist(hgdp.path)

  expect_true(is.numeric(result))
  expect_false(inherits(result, "dist"))
  expect_equal(length(result), 4L)
  expect_false(is.null(names(result)))
})

test_that("gPath2dist builds a correctly sized dist when input locations share a node", {
  # rows 1 and 2 are the same location
  hgdp.sub  <- hgdp[c(1, 1, 2, 3), ]
  hgdp.path <- dijkstraBetween(hgdp.sub)
  
  # check that gPath2dist still produces a dist object of the correct size
  expect_no_warning(result <- gPath2dist(hgdp.path))
  
  expect_s3_class(result, "dist")
  expect_equal(attr(result, "Size"), 4L)
  expect_equal(length(result), 6L)
})

test_that("gPath2dist places a zero distance for co-located nodes and stays consistent", {
  hgdp.sub  <- hgdp[c(1, 1, 2, 3), ]
  hgdp.path <- dijkstraBetween(hgdp.sub)
  
  result.dist <- gPath2dist(hgdp.path)
  result.vec  <- as.vector(result.dist)
  
  # the two co-located locations (1 and 2) have dist zero 
  expect_equal(as.matrix(result.dist)[1, 2], 0)
  # have identical distances to every other location
  expect_equal(as.matrix(result.dist)[1, 3:4],
               as.matrix(result.dist)[2, 3:4])
  
  # dist and vector still describe the same distances
  expect_equal(as.vector(result.dist), as.vector(result.vec))
})

test_that("gPath2dist auto-detects dist for between and vector for from", {
  coords <- data.frame(
    lon = c(12.0, 12.0, -3.0, 24.0,  2.0, 19.0, -8.0, 28.0, 15.0,  5.0),
    lat = c(50.0, 50.0, 40.0, 60.0, 48.0, 42.0, 53.0, 45.0, 55.0, 38.0)
  )
  rownames(coords) <- paste0("s", 1:10)
  g <- new("gData", coords = coords, gGraph.name = "rawgraph.10k")
  
  mb <- dijkstraBetween(g[1:5])
  mf <- dijkstraFrom(g, "7377")
  
  res_between <- gPath2dist(mb)
  res_from    <- gPath2dist(mf)
  
  ## dijkstraBetween -> dist 
  expect_s3_class(res_between, "dist")
  expect_equal(attr(res_between, "Size"), 5L)
  expect_equal(length(res_between), 10L)
  
  ## dijkstraFrom -> named numeric vector
  expect_true(is.numeric(res_from))
  expect_false(inherits(res_from, "dist"))
  expect_equal(length(res_from), 10L)
  expect_false(is.null(names(res_from)))
})
