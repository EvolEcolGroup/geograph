test_that(".zoomlog.up rejects malformed input", {
  expect_error(geoGraph:::.zoomlog.up("not numeric"),
               "Updating zoomlog using a wrong value")
  expect_error(geoGraph:::.zoomlog.up(c(1, 2, 3)),
               "Updating zoomlog using a wrong value")
  expect_error(geoGraph:::.zoomlog.up(c("a", "b", "c", "d")),
               "Updating zoomlog using a wrong value")
})

test_that(".zoomlog.up caps the zoom history at 100 rows", {
  env          <- geoGraph:::.geoGraphEnv
  original_log <- get("zoom.log", envir = env)
  on.exit(assign("zoom.log", original_log, envir = env), add = TRUE)
  
  for (i in seq_len(120)) {
    geoGraph:::.zoomlog.up(c(-i, i, -i, i))
  }
  expect_equal(nrow(get("zoom.log", envir = env)), 100L)
})

test_that("geo.bookmark lists, adds, and overwrites bookmarks", {
  env                <- geoGraph:::.geoGraphEnv
  original_bookmarks <- get("bookmarks", envir = env)
  on.exit(assign("bookmarks", original_bookmarks, envir = env), add = TRUE)
  
  ## check that we get the list
  expect_output(invisible(geo.bookmark(NULL)), "Available bookmarks")
  
  ## new bookmark is added
  geo.bookmark("test_mark")
  expect_true("test_mark" %in% rownames(get("bookmarks", envir = env)))
  
  ## re-adding the same name warns and overwrites
  expect_warning(geo.bookmark("test_mark"), "already existed")
})

test_that("geo.goto handles unknown and known bookmarks", {
  env                <- geoGraph:::.geoGraphEnv
  original_bookmarks <- get("bookmarks", envir = env)
  original_log       <- get("zoom.log",  envir = env)
  on.exit({
    assign("bookmarks", original_bookmarks, envir = env)
    assign("zoom.log",  original_log,       envir = env)
  }, add = TRUE)
  
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  plot(worldgraph.10k, reset = TRUE)
  
  ## unknown name 
  expect_output(invisible(geo.goto("nonexistent")), "Unknown bookmark")
  
  ## known name 
  geo.bookmark("goto_target")
  expect_no_error(geo.goto("goto_target"))
})