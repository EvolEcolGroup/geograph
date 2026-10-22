testGraph <- makeSquareGrid(
  size      = 100,
  lon.range = c(-12, 2),
  lat.range = c(49, 61)
)
testGraph <- findLand(testGraph)

test_that("dijkstraBuffer errors if graph is not a gGraph", {
  origin <- getNodes(testGraph)[1]
  expect_error(
    dijkstraBuffer(list(), origin = origin, d = 10),
    "x must be a valid gGraph object"
  )
})

test_that("dijkstraBuffer errors if character origin is not in the graph", {
  expect_error(
    dijkstraBuffer(testGraph, origin = "not_a_node", d = 10),
    "origin is not a node in x."
  )
})

test_that("dijkstraBuffer returns a character vector with valid node IDs when res.type = nodes", {
  origin <- getNodes(testGraph)[1]
  all.nodes <- getNodes(testGraph)
  result <- dijkstraBuffer(testGraph,
    origin = origin,
    d = 10, res.type = "nodes"
  )

  expect_type(result, "character")
  expect_true(all(result %in% all.nodes))
})

test_that("dijkstraBuffer returns a gGraph when res.type = gGraph", {
  origin <- getNodes(testGraph)[1]
  result <- dijkstraBuffer(testGraph,
    origin = origin,
    d = 10, res.type = "gGraph"
  )

  expect_s4_class(result, "gGraph")
  expect_true("reachable" %in% colnames(result@nodes.attr))
  col <- result@nodes.attr$reachable
  expect_type(col, "logical")
  expect_equal(length(col), length(getNodes(testGraph)))
})

test_that("dijkstraBuffer returns more nodes with a larger max.distance", {
  origin <- getNodes(testGraph)[1]
  result.large <- dijkstraBuffer(testGraph,
    origin = origin,
    d = 100, res.type = "nodes"
  )
  result.small <- dijkstraBuffer(testGraph,
    origin = origin,
    d = 10, res.type = "nodes"
  )

  expect_gte(length(result.large), length(result.small))
})

test_that("dijkstraBuffer accepts an origin given as c(lon, lat)", {
  loc <- getCoords(testGraph)[1, ]
  res.vec <- dijkstraBuffer(testGraph, origin = unname(loc), d = 10)
  res.node <- dijkstraBuffer(testGraph, origin = getNodes(testGraph)[1], d = 10)
  expect_identical(res.vec, res.node)
})

test_that("dijkstraBuffer includes the origin node", {
  origin <- getNodes(testGraph)[1]
  res <- dijkstraBuffer(testGraph, origin = origin, d = 10)
  expect_true(origin %in% res)
  res.graph <- dijkstraBuffer(testGraph, origin = origin, d = 10, res.type = "gGraph")
  expect_true(getNodesAttr(res.graph, nodes = origin, attr.name = "reachable")$reachable)
})

test_that("dijkstraBuffer matches the costs computed by dijkstraFrom", {
  origin <- getNodes(testGraph)[1]
  costs <- gPath2dist(dijkstraFrom(testGraph, origin))
  names(costs) <- sub(".*:", "", names(costs))
  d <- stats::median(costs)
  expected <- c(origin, names(costs)[costs <= d])
  expect_setequal(dijkstraBuffer(testGraph, origin = origin, d = d), expected)
})

test_that("dijkstraBuffer works on graphs that are not fully connected", {
  # two separate chains: 1-2-3 and 4-5
  x <- new("gGraph", coords = cbind(lon = 1:5, lat = rep(0, 5)))
  x <- setEdges(x, add = data.frame(c("1", "2", "4"), c("2", "3", "5")))
  expect_equal(dijkstraBuffer(x, origin = "1", d = 1), c("1", "2"))
  expect_equal(dijkstraBuffer(x, origin = "1", d = 100), c("1", "2", "3"))
})
