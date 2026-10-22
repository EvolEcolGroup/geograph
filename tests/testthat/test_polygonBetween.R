# build a test graph with two polygons
test.graph <- worldgraph.10k
west <- buffer(test.graph,
  nodes = "6414",
  d = 500,
  res.type = "nodes"
)

east <- buffer(test.graph,
  nodes = "9300",
  d = 1000,
  res.type = "nodes"
)

# assign polygon membership as a node attribute
polygon.attr <- rep(NA, length(getNodes(test.graph)))
names(polygon.attr) <- getNodes(test.graph)
polygon.attr[west] <- "west"
polygon.attr[east] <- "east"
test.graph@nodes.attr$name <- polygon.attr

test_that("polygonBetween errors if g is not a gGraph or gData", {
  expect_error(
    polygonBetween(list(), layer = "name", from = "west", to = "east"),
    "Input g must be a gGraph or gData object"
  )
})

test_that("polygonBetween errors if 'from' polygon has no nodes", {
  expect_error(
    polygonBetween(test.graph,
      layer = "name",
      from = "nonexistent", to = "east"
    ),
    "One or both polygons contain no nodes"
  )
})

test_that("polygonBetween errors if 'to' polygon has no nodes", {
  expect_error(
    polygonBetween(test.graph,
      layer = "name",
      from = "west", to = "nonexistent"
    ),
    "One or both polygons contain no nodes"
  )
})

test_that("polygonBetween returns a gPath with outline = FALSE", {
  result <- polygonBetween(test.graph,
    layer = "name",
    from = "west", to = "east",
    outline = FALSE
  )
  expect_true(inherits(result, "gPath"))
})

test_that("polygonBetween returns a gPath with outline = TRUE", {
  result <- polygonBetween(test.graph,
    layer = "name",
    from = "west", to = "east",
    outline = TRUE
  )
  expect_true(inherits(result, "gPath") || is.na(result))
})


test_that("polygonBetween outline = TRUE returns fewer or equal paths than outline = FALSE", {
  result.all <- polygonBetween(test.graph,
    layer = "name",
    from = "west", to = "east",
    outline = FALSE
  )
  result.outline <- polygonBetween(test.graph,
    layer = "name",
    from = "west", to = "east",
    outline = TRUE
  )
  if (!anyNA(result.outline)) {
    dist.all <- gPath2dist(result.all)
    dist.outline <- gPath2dist(result.outline)
    expect_lte(length(dist.outline), length(dist.all))
  }
})

test_that("polygonBetween distances are shorter for adjacent than distant polygons", {
  # make a new test graph with three polygons: west, east, and north
  test.graph <- worldgraph.10k
  west <- buffer(test.graph,
    nodes = "6414",
    d = 500,
    res.type = "nodes"
  )

  east <- buffer(test.graph,
    nodes = "9300",
    d = 500,
    res.type = "nodes"
  )

  far.east <- buffer(test.graph,
    nodes = "9939",
    d = 500,
    res.type = "nodes"
  )


  # assign polygon membership as a node attribute
  polygon.attr <- rep(NA, length(getNodes(test.graph)))
  names(polygon.attr) <- getNodes(test.graph)
  polygon.attr[west] <- "west"
  polygon.attr[east] <- "east"
  polygon.attr[far.east] <- "far.east"
  test.graph@nodes.attr$region <- polygon.attr

  result.adjacent <- polygonBetween(test.graph,
    layer = "region",
    from = "west", to = "east",
    outline = FALSE
  )
  result.distant <- polygonBetween(test.graph,
    layer = "region",
    from = "west", to = "far.east",
    outline = FALSE
  )


  dist.adjacent <- gPath2dist(result.adjacent)
  dist.distant <- gPath2dist(result.distant)

  expect_lt(
    min(dist.adjacent, na.rm = TRUE),
    min(dist.distant, na.rm = TRUE)
  )
})

test_that("polygonBetween finds outline nodes on a subset graph", {
  # 6 x 6 grid without its first column: node names no longer match positions
  sq <- makeSquareGrid(n.lon = 6, n.lat = 6, lon.range = c(0, 5), lat.range = c(0, 5))
  sub <- sq[as.character(7:36)]
  poly <- rep("none", 30)
  names(poly) <- getNodes(sub)
  poly[c("8", "9", "10", "14", "15", "16", "20", "21", "22")] <- "A" # 3 x 3 block
  poly["36"] <- "B"
  sub <- setNodesAttr(sub, attr.name = "poly", values = poly)

  res <- polygonBetween(sub, layer = "poly", from = "A", to = "B", outline = TRUE)
  from.nodes <- unique(sub(":.*", "", names(res)))
  # all nodes of the block but its centre (15) and node 9, whose western
  # neighbour was in the column removed above
  expect_setequal(from.nodes, c("8", "10", "14", "16", "20", "21", "22"))
})

test_that("polygonBetween computes all paths between the two polygons", {
  result <- polygonBetween(test.graph,
    layer = "name",
    from = "west", to = "east",
    outline = FALSE
  )
  n.west <- sum(getNodesAttr(test.graph)$name %in% "west")
  n.east <- sum(getNodesAttr(test.graph)$name %in% "east")
  expect_equal(length(result), n.west * n.east)
  expect_equal(dim(gPath2dist(result)), c(n.west, n.east))
})
