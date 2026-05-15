test_that("connectivityPlot result names match graph node names", {
  pdf(NULL)
  result <- connectivityPlot(worldgraph.10k, reset = TRUE)
  dev.off()
  
  expect_equal(sort(names(result)), sort(getNodes(worldgraph.10k)))
})

test_that("connectivityPlot returns invisible named color vector for gGraph", {
  pdf(NULL)
  result <- connectivityPlot(worldgraph.10k, reset = TRUE)
  dev.off()
  
  expect_true(is.character(result))
  expect_named(result)
  expect_equal(length(result), length(getNodes(worldgraph.10k)))
})

test_that("connectivityPlot returns invisible named color vector for gData", {
  pdf(NULL)
  result <- connectivityPlot(hgdp, reset = TRUE)
  dev.off()
  
  expect_true(is.character(result))
  expect_named(result)
  expect_equal(length(result), length(getNodes(hgdp)))
})

test_that("connectivityPlot produces different colors with different seeds", {
  pdf(NULL)
  result1 <- connectivityPlot(worldgraph.10k, seed = 1,  reset = TRUE)
  result2 <- connectivityPlot(worldgraph.10k, seed = 99, reset = TRUE)
  dev.off()
  
  expect_false(identical(result1, result2))
})

test_that("two nodes with the same color are actually connected", {
  pdf(NULL)
  result <- connectivityPlot(worldgraph.10k, seed = 42, reset = TRUE)
  dev.off()
  
  # pick all nodes sharing the most common non-gray color
  non.gray       <- result[result != "lightgray"]
  most.common    <- names(sort(table(non.gray), decreasing = TRUE))[1]
  same.col.nodes <- names(non.gray[non.gray == most.common])
  
  expect_true(areConnected(worldgraph.10k, nodes = same.col.nodes))
})

test_that("two nodes with different colors are not connected", {
  pdf(NULL)
  result <- connectivityPlot(worldgraph.10k, seed = 42, reset = TRUE)
  dev.off()
  
  # worldgraph.10k has multiple connected sets so this is always valid
  non.gray   <- result[result != "lightgray"]
  all.colors <- unique(non.gray)
  
  node.col1 <- names(non.gray[non.gray == all.colors[1]])[1]
  node.col2 <- names(non.gray[non.gray == all.colors[2]])[1]
  
  expect_false(areConnected(worldgraph.10k, nodes = c(node.col1, node.col2)))
})
