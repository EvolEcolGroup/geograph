test_that("gPath2dist errors if m is not a gPath object", {
  expect_error(gPath2dist(list()), "m is not a gPath object")
  expect_error(gPath2dist("not_a_gpath"), "m is not a gPath object")
})

test_that("gPath2dist returns a dist object by default", {
  hgdp.sub  <- hgdp[c(1, 2, 3, 4), ]
  hgdp.path <- dijkstraBetween(hgdp.sub)
  result    <- gPath2dist(hgdp.path)
  
  expect_s3_class(result, "dist")
})

test_that("gPath2dist dist object has correct size", {
  # 4 locations -> 4*(4-1)/2 = 6 pairwise distances
  hgdp.sub  <- hgdp[c(1, 2, 3, 4), ]
  hgdp.path <- dijkstraBetween(hgdp.sub)
  result    <- gPath2dist(hgdp.path)
  
  expect_equal(attr(result, "Size"), 4L)
  expect_equal(length(result), 6L)
})

test_that("gPath2dist returns a named vector with res.type = 'vector'", {
  hgdp.sub  <- hgdp[c(1, 2, 3, 4), ]
  hgdp.path <- dijkstraBetween(hgdp.sub)
  result    <- gPath2dist(hgdp.path, res.type = "vector")
  
  expect_true(is.numeric(result))
  expect_false(inherits(result, "dist"))
  expect_equal(length(result), 6L)
  expect_false(is.null(names(result)))
})

test_that("gPath2dist dist and vector return consistent distances", {
  hgdp.sub  <- hgdp[c(1, 2, 3, 4), ]
  hgdp.path <- dijkstraBetween(hgdp.sub)
  
  result.dist <- gPath2dist(hgdp.path, res.type = "dist")
  result.vec  <- gPath2dist(hgdp.path, res.type = "vector")
  
  expect_equal(as.vector(result.dist), as.vector(result.vec))
})

test_that("gPath2dist gives an error when dijkstraFrom output is used with res.type = 'dist'", {
  
  hgdp.sub <- hgdp[c(1, 2, 3, 4),]
  
  # Choose an origin node
  start <- "24988"
  
  hgdp.path <- dijkstraFrom(hgdp.sub, start)
  
  expect_error(
    gPath2dist(hgdp.path, res.type = "dist"),
    "Length of x does not match a number of pairwise comparisons")
})

test_that("gPath2dist works correctly with dijkstraFrom output and res.type = 'vector'", {
  hgdp.sub <- hgdp[c(1, 2, 3, 4),]
  
  # Choose an origin node
  start <- "24988"
  
  hgdp.path <- dijkstraFrom(hgdp.sub, start)
  
  result <- gPath2dist(hgdp.path, res.type = "vector")
  
  expect_true(is.numeric(result))
  expect_equal(length(result), 4L)
})
