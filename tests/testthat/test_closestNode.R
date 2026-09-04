test_that("both approaches in closestNode matche on hgdp data", {
  fnn.result <- closestNode(worldgraph.40k, hgdp@coords, method = "knn")
  old.result <- closestNode(worldgraph.40k, hgdp@coords, method = "inArea")
  expect_identical(unname(fnn.result), unname(old.result))
})

test_that(".buildNnIndex returns a unit-sphere XYZ matrix", {
  coords <- getCoords(worldgraph.40k)
  xyz <- geoGraph:::.buildNnIndex(coords)

  ## structure: N × 3 numeric matrix
  expect_true(is.matrix(xyz))
  expect_equal(nrow(xyz), nrow(coords))
  expect_equal(ncol(xyz), 3L)
  expect_type(xyz, "double")

  ## every row is a unit vector on the sphere
  norms <- sqrt(rowSums(xyz^2))
  expect_true(all(abs(norms - 1) < 1e-10))
})

test_that("closestNode accepts matrix, data.frame, and list input", {
  mat <- matrix(c(12, 50), ncol = 2)
  df <- data.frame(lon = 12, lat = 50)
  lst <- list(lon = 12, lat = 50)

  r.mat <- closestNode(worldgraph.40k, mat)
  r.df <- closestNode(worldgraph.40k, df)
  r.lst <- closestNode(worldgraph.40k, lst)

  expect_equal(unname(r.mat), unname(r.df))
  expect_equal(unname(r.df), unname(r.lst))
})

test_that("closestNode handles a single query point", {
  loc <- matrix(c(12, 50), ncol = 2)
  result <- closestNode(worldgraph.40k, loc)
  expect_length(result, 1L)
  expect_type(result, "character")
})

test_that("closestNode preserves query row names in output", {
  loc <- data.frame(
    lon = c(12, 30), lat = c(50, 45),
    row.names = c("berlin", "milan")
  )
  result <- closestNode(worldgraph.40k, loc)
  expect_equal(names(result), c("berlin", "milan"))
})

test_that("closestNode respects attribute filters", {
  loc <- data.frame(lon = c(12, 30), lat = c(50, 45))

  ## every returned node should have habitat "land"
  r <- closestNode(worldgraph.40k, loc,
    attr.name = "habitat", attr.values = "land"
  )
  hab <- as.character(unlist(getNodesAttr(worldgraph.40k, attr.name = "habitat")))
  names(hab) <- getNodes(worldgraph.40k)
  expect_true(all(hab[r] == "land"))

  ## a non-existent value should error informatively
  expect_error(
    closestNode(worldgraph.40k, loc, attr.name = "habitat", attr.values = "moon"),
    "never found"
  )
})

test_that("closestNode on gData returns a gData with populated nodes.id", {
  loc <- list(x = c(3, -8, 11, 28), y = c(50, 57, 71, 67))
  gd <- new("gData", coords = loc)
  gd@gGraph.name <- "worldgraph.40k"

  result <- closestNode(gd, attr.name = "habitat", attr.values = "land")
  expect_s4_class(result, "gData")
  expect_length(result@nodes.id, 4L)
  expect_true(all(nchar(result@nodes.id) > 0))
})

test_that("closestNode handles exact poles without error", {
  loc <- data.frame(lon = c(0, 180), lat = c(90, -90))
  expect_no_error(closestNode(worldgraph.40k, loc))
})
