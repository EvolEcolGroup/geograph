test_that("setEdges can remove an existing edge", {
  # get two neighbouring nodes
  edge <- getEdges(worldgraph.10k, res.type = "matNames")[1, ]
  node.from <- edge[1]
  node.to <- edge[2]

  result <- setEdges(worldgraph.10k,
    remove = data.frame(from = node.from, to = node.to)
  )

  expect_false(areNeighbours(node.from, node.to, result))
})

test_that("setEdges can add an edge back after removing it", {
  edge <- getEdges(worldgraph.10k, res.type = "matNames")[1, ]
  node.from <- edge[1]
  node.to <- edge[2]

  removed <- setEdges(worldgraph.10k,
    remove = data.frame(from = node.from, to = node.to)
  )
  restored <- setEdges(removed,
    add = data.frame(from = node.from, to = node.to)
  )

  expect_true(areNeighbours(node.from, node.to, restored))
})

test_that("setEdges errors on unknown node names", {
  expect_error(
    setEdges(worldgraph.10k,
      add = data.frame(from = "nonexistent", to = "alsononexistent")
    ),
    "unknown specified nodes"
  )
})

test_that("setEdges returns a gGraph", {
  edge <- getEdges(worldgraph.10k, res.type = "matNames")[1, ]
  result <- setEdges(worldgraph.10k,
    remove = data.frame(from = edge[1], to = edge[2])
  )
  expect_s4_class(result, "gGraph")
})

test_that("setEdges errors when both add and remove are specified", {
  edge <- getEdges(worldgraph.10k, res.type = "matNames")[1, ]
  expect_error(
    setEdges(worldgraph.10k,
      add = data.frame(from = edge[1], to = edge[2]),
      remove = data.frame(from = edge[1], to = edge[2])
    ),
    "Only one of `add` or `remove` can be specified per call."
  )
})

test_that("setEdges errors when costs length does not match edges", {
  edge <- getEdges(worldgraph.10k, res.type = "matNames")[1, ]
  expect_error(
    setEdges(worldgraph.10k,
      add   = data.frame(from = edge[1], to = edge[2]),
      costs = c(1, 2)
    ),
    "`costs` must have length 1 or match the number of edges"
  )
})

test_that("setEdges recycles scalar cost to all edges", {
  edges <- getEdges(worldgraph.10k, res.type = "matNames")[1:2, ]

  # remove the edges first so we can add them back with known costs
  x <- setEdges(worldgraph.10k,
    remove = data.frame(from = edges[1, 1], to = edges[1, 2])
  )
  x <- setEdges(x,
    remove = data.frame(from = edges[2, 1], to = edges[2, 2])
  )

  # add them back with scalar cost = 5
  result <- setEdges(x,
    add   = data.frame(from = edges[, 1], to = edges[, 2]),
    costs = 5
  )

  # verify both edges exist
  expect_true(areNeighbours(edges[1, 1], edges[1, 2], getGraph(result)))
  expect_true(areNeighbours(edges[2, 1], edges[2, 2], getGraph(result)))

  # verify both edges have cost 5
  all.costs <- getCosts(result, res.type = "vector")

  edge.1.key <- paste(edges[1, 1], edges[1, 2], sep = ".")
  edge.2.key <- paste(edges[2, 1], edges[2, 2], sep = ".")

  expect_equal(unname(all.costs[edge.1.key]), 5)
  expect_equal(unname(all.costs[edge.2.key]), 5)
})

test_that("setEdges gives the same graph as graph::addEdge and graph::removeEdge", {
  set.seed(10)
  for (x in list(worldgraph.10k, rawgraph.10k, dropCosts(rawgraph.10k))) {
    E0 <- getEdges(x, res.type = "matNames")
    und <- E0[E0[, 1] < E0[, 2], ]
    ## remove, including both directions of an edge and a non-existing edge
    rm <- und[sample(nrow(und), 300), ]
    rm <- rbind(rm, rm[1, 2:1], c(getNodes(x)[1], getNodes(x)[500]))
    temp <- areNeighbours(rm[, 1], rm[, 2], getGraph(x))
    expectSameEdgeData(
      getGraph(setEdges(x, remove = rm)),
      graph::removeEdge(rm[temp, 1], rm[temp, 2], getGraph(x))
    )
    ## add, including existing edges, duplicated pairs in both directions,
    ## and a self-loop
    add <- cbind(sample(getNodes(x), 300), sample(getNodes(x), 300))
    add <- add[add[, 1] != add[, 2], ]
    add <- rbind(add, und[1:5, ], add[1, ], add[2, 2:1], rep(getNodes(x)[7], 2))
    costs <- runif(nrow(add))
    expectSameEdgeData(
      getGraph(setEdges(x, add = add, costs = costs)),
      suppressWarnings(graph::addEdge(add[, 1], add[, 2], getGraph(x), costs))
    )
    expectSameEdgeData(
      getGraph(setEdges(x, add = add)),
      suppressWarnings(graph::addEdge(add[, 1], add[, 2], getGraph(x), rep(1, nrow(add))))
    )
  }
})
