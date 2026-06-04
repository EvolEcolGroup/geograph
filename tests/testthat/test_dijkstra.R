test_that("dijkstra_between computes distances correctly", {
  # test the gData method
  # we take four locations from 4 different continents:
  # "AMERICA", "EUROPE", "CENTRAL_SOUTH_ASIA",  "AFRICA"
  hgdp_sub <- hgdp[c(24, 1, 13, 27), ]
  hgdp_between <- dijkstraBetween(hgdp_sub)
  # plot(worldgraph.40k, pch = "")
  # points(hgdp_sub, lwd = 3)
  # plot(hgdp_between)
  dist_matrix <- gPath2dist(hgdp_between)
  # expect distance between South America and any other location to be
  # to be larger than any other distance
  expect_true(min(dist_matrix[1:3]) > max(dist_matrix[4:6]))

  # now test the same for the gGraph algorithm
  # we extract the nodes from the gData object
  graph_between <- dijkstraBetween(worldgraph.40k,
    from = hgdp_sub@nodes.id,
    to = hgdp_sub@nodes.id
  )
  graph_dist_matrix <- gPath2dist(graph_between)
  expect_true(identical(dist_matrix, graph_dist_matrix))
})


testthat::test_that("DijkstraFrom works on a connected graph", {
  max_set <- keepMaxConnectedSet(worldgraph.10k)
  isConnected(max_set)

  # Choose a start point within the graph space
  coords_max_set <- getCoords(max_set)
  head(coords_max_set)
  # node 67
  origin <- "67"
  foo <- dijkstraFrom(max_set, origin)

  # Check output is a gPath
  testthat::expect_true(inherits(foo, "gPath"))

  # Check resulting gPath 'foo' has the same number of rows as coords_max_set
  # (-1 as no path is calculated from origin to origin)
  testthat::expect_equal(length(names(foo)), nrow(coords_max_set) - 1)
})

testthat::test_that("DijkstraFrom works on a gData object", {
  # Create a subset of hgdp data
  hgdp_sub <- hgdp[c(1, 2, 3, 4)]

  # Choose an origin node
  start <- "24988"

  myPath <- dijkstraFrom(hgdp_sub, start)

  # Check output is a gPath
  testthat::expect_true(inherits(myPath, "gPath"))

  # Check that myPath has the expected pairs of nodes
  testthat::expect_equal(names(myPath), c("24988:26898", "24988:11652", "24988:22532", "24988:23709"))
})

testthat::test_that("DijkstraBetween can handle two points on the same node in a gData object", {
  hgdp_sub <- hgdp[getData(hgdp)$Region == "MIDDLE_EAST" |
                     getData(hgdp)$Region == "CENTRAL_SOUTH_ASIA"]
  pop      <- getData(hgdp_sub)$Population
  nodes_id <- hgdp_sub@nodes.id
  
  m <- dijkstraBetween(hgdp_sub)
  d <- geoGraph::gPath2dist(m)
  
  ## one entry per population
  testthat::expect_equal(attr(d, "Size"), length(pop))
  
  ## every pair of populations sharing a graph node has distance 0
  d_mat <- as.matrix(d)
  for (i in seq_along(pop)) {
    for (j in seq_along(pop)) {
      if (i != j && nodes_id[i] == nodes_id[j]) {
        testthat::expect_equal(d_mat[i, j], 0)
      }
    }
  }
})

test_that("dijkstraBetween errors when from is empty", {
  expect_error(
    dijkstraBetween(worldgraph.10k, from = character(0), to = "1"),
    "`from` and `to` must be non-empty."
  )
})

test_that("dijkstraBetween errors when to is empty", {
  expect_error(
    dijkstraBetween(worldgraph.10k, from = "1", to = character(0)),
    "`from` and `to` must be non-empty."
  )
})

test_that("dijkstraFrom errors when start node is not in the graph", {
  max_set <- keepMaxConnectedSet(worldgraph.10k)
  # find a node that is in the graph but not in the max_set
  start <- setdiff(getNodes(worldgraph.10k), getNodes(max_set))[1]
  expect_error(
    dijkstraFrom(max_set, start),
    "Starting node is not in x."
  )
})

test_that("dijkstraFrom errors when gGraph is not fully connected", {
  # rawgraph.10k with dead edges dropped will have disconnected nodes
  disconnected <- dropDeadEdges(rawgraph.10k, thres = 0)
  expect_error(
    dijkstraFrom(disconnected, "1"),
    "Not all nodes are connected by the graph."
  )
})

test_that("dijkstraBetween errors when nodes are not connected", {
  disconnected <- dropDeadEdges(rawgraph.10k, thres = 0)
  nodes <- getNodes(disconnected)[1:2]
  expect_error(
    dijkstraBetween(disconnected, from = nodes[1], to = nodes[2]),
    "Not all nodes are connected by the graph."
  )
})

test_that("dijkstraBetween works with a single pair of nodes", {
  nodes <- getNodes(rawgraph.10k)[1:2]
  result <- dijkstraBetween(rawgraph.10k, from = nodes[1], to = nodes[2])
  expect_s3_class(result, "gPath")
  expect_equal(length(result), 1L)
})
