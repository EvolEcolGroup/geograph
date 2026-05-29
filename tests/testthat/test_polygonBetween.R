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
  dist.all <- gPath2dist(result.all, res = "vec")
  dist.outline <- gPath2dist(result.outline, res = "vec")
  if (!anyNA(result.outline)) {
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


  dist.adjacent <- gPath2dist(result.adjacent, res = "vec")
  dist.distant <- gPath2dist(result.distant, res = "vec")

  expect_lt(
    min(dist.adjacent, na.rm = TRUE),
    min(dist.distant, na.rm = TRUE)
  )
})
