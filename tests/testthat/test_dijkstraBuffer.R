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
