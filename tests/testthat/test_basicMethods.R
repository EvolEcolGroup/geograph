## [,gGraph
test_that("[,gGraph subsets coords, nodes.attr and graph consistently", {
  x <- worldgraph.10k[1:10]
  expect_equal(nrow(getCoords(x)), 10L)
  expect_equal(length(getNodes(x)), 10L)
  expect_equal(length(nodes(getGraph(x))), 10L)
})

test_that("[,gGraph works with logical index", {
  idx <- seq_len(nrow(getCoords(worldgraph.10k))) <= 10
  x   <- worldgraph.10k[idx]
  expect_equal(nrow(getCoords(x)), 10L)
})

test_that("[,gGraph works with character node names", {
  x <- worldgraph.10k[c("1", "2", "3")]
  expect_equal(getNodes(x), c("1", "2", "3"))
})

test_that("[,gGraph errors on unknown node names", {
  expect_error(
    worldgraph.10k["nonexistent_node"],
    "Some specified node labels were not found."
  )
})

test_that("[,gGraph returns full object when no index given", {
  x <- worldgraph.10k[]
  expect_equal(length(getNodes(x)), length(getNodes(worldgraph.10k)))
})

test_that("[,gGraph preserves nodes.attr when subsetting", {
  x <- worldgraph.10k[1:10]
  expect_equal(nrow(getNodesAttr(x)), 10L)
  expect_equal(colnames(getNodesAttr(x)), colnames(getNodesAttr(worldgraph.10k)))
})

## [,gData
test_that("[,gData subsets coords, nodes.id and data consistently", {
  x <- hgdp[1:5]
  expect_equal(nrow(getCoords(x)), 5L)
  expect_equal(length(getNodes(x)), 5L)
  expect_equal(nrow(getData(x)), 5L)
})

test_that("[,gData works with logical index", {
  idx <- hgdp@data$Latitude > 0
  x   <- hgdp[idx]
  expect_true(all(getData(x)$Latitude > 0))
})

test_that("[,gData errors on unknown node names", {
  expect_error(
    hgdp["nonexistent_node"],
    "Some specified node labels were not found."
  )
})

test_that("[,gData preserves gGraph.name after subsetting", {
  x <- hgdp[1:5]
  expect_equal(x@gGraph.name, hgdp@gGraph.name)
})

test_that("[,gData returns full object when no index given", {
  x <- hgdp[]
  expect_equal(length(getNodes(x)), length(getNodes(hgdp)))
})
