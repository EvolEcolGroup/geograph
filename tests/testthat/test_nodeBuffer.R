# shared objects — built once to keep tests fast
geo.box    <- c(xmin = -10, xmax = 30, ymin = 35, ymax = 60)
test.graph <- createNewGraph(geo.box, spacing = 1000)
test.graph@meta$cost <- rep(1, nrow(test.graph@coords))

test_that("nodeBuffer errors if graph is not a gGraph", {
  origin <- getNodes(test.graph)[1]
  expect_error(
    nodeBuffer(list(), origin = origin, max.distance = 2000),
    "`graph` must be a valid gGraph object"
  )
})

test_that("nodeBuffer errors if character origin is not in the graph", {
  expect_error(
    nodeBuffer(test.graph, origin = "not_a_node", max.distance = 2000),
    "`origin` is not a node in `graph`"
  )
})

test_that("nodeBuffer returns a character vector when map.distances = FALSE", {
  origin <- getNodes(test.graph)[1]
  result <- nodeBuffer(test.graph, origin = origin,
                       max.distance = 2000, map.distances = FALSE)
  
  expect_type(result, "character")
})

test_that("nodeBuffer returns only valid node IDs when map.distances = FALSE", {
  origin     <- getNodes(test.graph)[1]
  all.nodes  <- getNodes(test.graph)
  result     <- nodeBuffer(test.graph, origin = origin,
                           max.distance = 2000, map.distances = FALSE)
  
  expect_true(all(result %in% all.nodes))
})

test_that("nodeBuffer returns a gGraph when map.distances = TRUE", {
  origin <- getNodes(test.graph)[1]
  result <- nodeBuffer(test.graph, origin = origin,
                       max.distance = 2000, map.distances = TRUE)
  
  expect_s4_class(result, "gGraph")
})

test_that("nodeBuffer adds a diffusion_area attribute when map.distances = TRUE", {
  origin <- getNodes(test.graph)[1]
  result <- nodeBuffer(test.graph, origin = origin,
                       max.distance = 2000, map.distances = TRUE)
  
  expect_true("diffusion_area" %in% colnames(result@nodes.attr))
})

test_that("nodeBuffer diffusion_area is a logical vector of correct length", {
  origin <- getNodes(test.graph)[1]
  result <- nodeBuffer(test.graph, origin = origin,
                       max.distance = 2000, map.distances = TRUE)
  
  col <- result@nodes.attr$diffusion_area
  expect_type(col, "logical")
  expect_equal(length(col), length(getNodes(test.graph)))
})


test_that("nodeBuffer map.distances TRUE and FALSE agree on reachable nodes", {
  origin <- getNodes(test.graph)[1]
  
  vec    <- nodeBuffer(test.graph, origin = origin,
                       max.distance = 2000, map.distances = FALSE)
  mapped <- nodeBuffer(test.graph, origin = origin,
                       max.distance = 2000, map.distances = TRUE)
  
  col          <- mapped@nodes.attr$diffusion_area
  mapped.nodes  <- as.character(which(col))
  
  expect_equal(sort(vec), sort(mapped.nodes))
})


test_that("nodeBuffer returns more nodes with a larger max.distance", {
  origin  <- getNodes(test.graph)[1]
  result.small <- nodeBuffer(test.graph, origin = origin,
                             max.distance = 1000, map.distances = FALSE)
  result.large <- nodeBuffer(test.graph, origin = origin,
                             max.distance = 5000, map.distances = FALSE)
  
  expect_gte(length(result.large), length(result.small))
})


test_that("nodeBuffer accepts a numeric lon/lat vector as origin", {
  # use the coords of the first node as spatial input
  lon    <- test.graph@coords[1, "lon"]
  lat    <- test.graph@coords[1, "lat"]
  origin <- list(x = lon, y = lat)
  
  expect_no_error(
    nodeBuffer(test.graph, origin = origin,
               max.distance = 2000, map.distances = FALSE)
  )
})

test_that("nodeBuffer spatial and character origin give same result for same node", {
  node.id <- getNodes(test.graph)[1]
  lon    <- test.graph@coords[1, "lon"]
  lat    <- test.graph@coords[1, "lat"]
  
  result.char    <- nodeBuffer(test.graph, origin = node.id,
                               max.distance = 2000, map.distances = FALSE)
  result.spatial <- nodeBuffer(test.graph, origin = list(x = lon, y = lat),
                               max.distance = 2000, map.distances = FALSE)
  
  expect_equal(sort(result.char), sort(result.spatial))
})

test_that("nodeBuffer returns smaller buffer for smaller max.distance with spatial origin", {
  lon    <- test.graph@coords[1, "lon"]
  lat    <- test.graph@coords[1, "lat"]
  
  result.small <- nodeBuffer(test.graph, origin = list(x = lon, y = lat),
                             max.distance = 3, map.distances = FALSE)
  result.large <- nodeBuffer(test.graph, origin = list(x = lon, y = lat),
                             max.distance = 10, map.distances = FALSE)
  
  expect_gt(length(result.large), length(result.small))
})
