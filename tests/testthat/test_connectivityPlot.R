test_that("connectivityPlot result names match graph node names", {
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  result <- connectivityPlot(worldgraph.10k)

  expect_equal(sort(names(result)), sort(getNodes(worldgraph.10k)))
})

test_that("connectivityPlot returns invisible named color vector for gGraph", {
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  result <- connectivityPlot(worldgraph.10k)

  expect_true(is.character(result))
  expect_named(result)
  expect_equal(length(result), length(getNodes(worldgraph.10k)))
})

test_that("connectivityPlot returns invisible named color vector for gData", {
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  result <- connectivityPlot(hgdp)

  expect_true(is.character(result))
  expect_named(result)
  expect_equal(length(result), length(getNodes(hgdp)))
})

test_that("connectivityPlot produces different colors with different seeds", {
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  result1 <- connectivityPlot(worldgraph.10k, seed = 1)
  result2 <- connectivityPlot(worldgraph.10k, seed = 99)

  expect_false(identical(result1, result2))
})

test_that("two nodes of the gGraph with the same color are actually connected", {
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  result <- connectivityPlot(worldgraph.10k, seed = 42)

  # pick all nodes sharing the most common non-gray color
  non.gray       <- result[result != "lightgray"]
  most.common    <- names(sort(table(non.gray), decreasing = TRUE))[1]
  same.col.nodes <- names(non.gray[non.gray == most.common])
  
  expect_true(areConnected(worldgraph.10k, nodes = same.col.nodes))
})

test_that("two nodes of the gGraph with different colors are not connected", {
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  result <- connectivityPlot(worldgraph.10k, seed = 42)

  # worldgraph.10k has multiple connected sets so this is always valid
  non.gray   <- result[result != "lightgray"]
  all.colors <- unique(non.gray)
  
  node.col1 <- names(non.gray[non.gray == all.colors[1]])[1]
  node.col2 <- names(non.gray[non.gray == all.colors[2]])[1]
  
  expect_false(areConnected(worldgraph.10k, nodes = c(node.col1, node.col2)))
})

test_that("two nodes of the gData with the same color are actually connected", {
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  result <- connectivityPlot(hgdp, seed = 42)

  # pick all nodes sharing the most common non-gray color
  non.gray       <- result[result != "lightgray"]
  most.common    <- names(sort(table(non.gray), decreasing = TRUE))[1]
  same.col.nodes <- names(non.gray[non.gray == most.common])
  
  expect_true(areConnected(worldgraph.40k, nodes = same.col.nodes))
})

test_that("two nodes of the gData with different colors are not connected", {
  
  point1 <- c(-3.7038, 40.4168) 
  point2 <-  c(-74.0060, 40.7128) 
  
  cities.dat <- rbind.data.frame(point1, point2)
  colnames(cities.dat) <- c("lon", "lat")
  row.names(cities.dat) <- c("1", "2")
  cities <- new("gData", coords = cities.dat[, 1:2], gGraph.name = "worldgraph.10k")
  
  
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  result <- connectivityPlot(cities, seed = 42)

  # worldgraph.10k has multiple connected sets so this is always valid
  non.gray   <- result[result != "lightgray"]
  all.colors <- unique(non.gray)
  
  node.col1 <- names(non.gray[non.gray == all.colors[1]])[1]
  node.col2 <- names(non.gray[non.gray == all.colors[2]])[1]
  
  expect_false(areConnected(worldgraph.10k, nodes = c(node.col1, node.col2)))
})

test_that("connectivityPlot handles graph with all isolated nodes", {
  # create a gGraph with no edges
  myGraph <- dropDeadEdges(rawgraph.10k, thres = 0)

  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  result <- connectivityPlot(myGraph)

  expect_true(all(result == "lightgray"))
})

test_that("connectivityPlot assigns colors correctly to gData with sparse relevant sets", {

  # have a gData where relevant connected sets appear at indices beyond nbRelSets
  
  # on set 1 
  point1 <- c(-3.7038, 40.4168) 
  point2 <- c(31.2357, 30.0444) 
  
  # on set 3 
  point3 <- c(149.1300, -35.2809) 
  point4 <- c(115.8605, -32.7157) 
  
  # disconnected points
  point5 <- c(-157.8583, 21.3069) 
  point6 <- c(-172.1046, -13.7590)
  
  cities2.dat <- rbind.data.frame(point1, point2, point3, point4, point5, point6)
  colnames(cities2.dat) <- c("lon", "lat")
  row.names(cities2.dat) <- c("1", "2", "3", "4", "5", "6")
  cities2 <- new("gData", coords = cities2.dat[, 1:2], gGraph.name = "worldgraph.10k")
  
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  result <- connectivityPlot(cities2, seed = 123)

  # check that no nodes colored gray are connected to any other gray nodes
  gray.nodes <- names(result[result == "lightgray"])
  expect_false(areConnected(worldgraph.10k, nodes = gray.nodes))
})
