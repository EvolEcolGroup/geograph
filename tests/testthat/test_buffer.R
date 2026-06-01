test_that("buffer errors on invalid inputs", {
  x <- rawgraph.10k
  node <- getNodes(x)[1]
  expect_error(buffer(x, node, d = "far"), "d is not numeric")
  expect_error(buffer(x, "not_a_node", d = 100),
               "do not exist in the gGraph grid")
})

test_that("buffer warns when d is very large", {
  x <- rawgraph.10k
  node <- getNodes(x)[1]
  expect_warning(buffer(x, node, d = 1e5),
                 "greater than 10,000km")
})

test_that("buffer returns character vector of nodes including the origin", {
  x    <- rawgraph.10k[isInArea(worldgraph.10k,
                                reg = list(x = c(-10, 50), y = c(35, 70)), quiet = TRUE)]
  node <- closestNode(x, data.frame(lon = 12, lat = 50))
  res  <- buffer(x, node, d = 500)
  expect_type(res, "character")
  expect_true(node %in% res)        
  expect_true(all(res %in% getNodes(x)))
})

test_that("buffer with larger d returns more nodes", {
  x    <- rawgraph.10k[isInArea(worldgraph.10k,
                                reg = list(x = c(-10, 50), y = c(35, 70)), quiet = TRUE)]
  node <- closestNode(x, data.frame(lon = 12, lat = 50))
  small <- buffer(x, node, d = 300)
  large <- buffer(x, node, d = 800)
  expect_gt(length(large), length(small))
})

test_that("buffer res.type = 'gGraph' adds buffer attribute and colors", {
  x    <- rawgraph.10k[isInArea(worldgraph.10k,
                                reg = list(x = c(-10, 50), y = c(35, 70)), quiet = TRUE)]
  node <- closestNode(x, data.frame(lon = 12, lat = 50))
  res  <- buffer(x, node, d = 500, res.type = "gGraph")
  expect_s4_class(res, "gGraph")
  expect_true("buffer" %in% colnames(getNodesAttr(res)))
  expect_true(is.logical(getNodesAttr(res)$buffer))
  expect_equal(res@meta$buf.colors$color, c("orange", "lightgrey"))
  
  ## the buffered nodes flagged TRUE should match the node-based result
  nodes.res <- buffer(x, node, d = 500, res.type = "nodes")
  flagged   <- getNodes(res)[getNodesAttr(res)$buffer]
  expect_setequal(flagged, nodes.res)
})

test_that("buffer on gData returns nodes", {
  x   <- hgdp[27:30]
  res <- buffer(x, d = 400, res.type = "nodes")
  expect_type(res, "character")
  myGraph <- get(x@gGraph.name, envir = .GlobalEnv)
  expect_true(all(res %in% getNodes(myGraph)))
})

test_that("buffer on gData returns a gData", {
  x   <- hgdp[27:30]
  res <- buffer(x, d = 400, res.type = "gData")
  expect_s4_class(res, "gData")
  expect_equal(res@gGraph.name, x@gGraph.name)
  expect_true(nrow(getCoords(res)) > 0)
})

test_that("buffer on gData delegates to gGraph", {
  x   <- hgdp[27:30]
  res <- buffer(x, d = 400, res.type = "gGraph")
  expect_s4_class(res, "gGraph")
  expect_true("buffer" %in% colnames(getNodesAttr(res)))
})
