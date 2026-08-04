test_that("closestNodeFnn matches closestNode on hgdp data", {
  idx <- buildNnIndex(worldgraph.40k)
  fnn_result <- closestNodeFnn(idx, hgdp@coords)
  old_result <- closestNode(worldgraph.40k, hgdp@coords)
  expect_identical(unname(fnn_result), unname(old_result))
})

test_that("buildNnIndex returns the expected structure", {
  idx <- buildNnIndex(worldgraph.40k)
  expect_type(idx, "list")
  expect_named(idx, c("nodes", "xyz"))
  expect_type(idx$nodes, "character")
  expect_true(is.matrix(idx$xyz))
  expect_equal(ncol(idx$xyz), 3L)
  expect_equal(length(idx$nodes), nrow(idx$xyz))
})

test_that("closestNodeFnn accepts matrix, data.frame, and list input", {
  idx <- buildNnIndex(worldgraph.40k)
  
  mat <- matrix(c(12, 50), ncol = 2)
  df  <- data.frame(lon = 12, lat = 50)
  lst <- list(lon = 12, lat = 50)
  
  r_mat <- closestNodeFnn(idx, mat)
  r_df  <- closestNodeFnn(idx, df)
  r_lst <- closestNodeFnn(idx, lst)
  
  expect_equal(unname(r_mat), unname(r_df))
  expect_equal(unname(r_df),  unname(r_lst))
})

test_that("closestNodeFnn handles a single query point", {
  idx <- buildNnIndex(worldgraph.40k)
  loc <- matrix(c(12, 50), ncol = 2)
  result <- closestNodeFnn(idx, loc)
  expect_length(result, 1L)
  expect_type(result, "character")
})

test_that("closestNodeFnn preserves query row names in output", {
  idx <- buildNnIndex(worldgraph.40k)
  loc <- data.frame(lon = c(12, 30), lat = c(50, 45),
                    row.names = c("berlin", "milan"))
  result <- closestNodeFnn(idx, loc)
  expect_equal(names(result), c("berlin", "milan"))
})


