#######################
## setGraph
#######################

test_that("setGraph preserves all other gData slots when updating graph", {
  assign("myGraph", dropCosts(worldgraph.40k), envir = .GlobalEnv)
  on.exit(rm("myGraph", envir = .GlobalEnv), add = TRUE)

  result <- setGraph(hgdp, "myGraph")
  expect_equal(getNodes(result), getNodes(hgdp))
  expect_equal(getCoords(result), getCoords(hgdp))
  expect_equal(getData(result), getData(hgdp))
})

test_that("setGraph by name and by object give identical results", {
  assign("myGraph", dropCosts(worldgraph.40k), envir = .GlobalEnv)
  on.exit(rm("myGraph", envir = .GlobalEnv), add = TRUE)

  by.name <- setGraph(hgdp, "myGraph")
  by.object <- setGraph(hgdp, myGraph)
  expect_equal(by.name@gGraph.name, by.object@gGraph.name)
})

test_that("setGraph accepts string references to global objects", {
  assign("stringRefGraph", dropCosts(rawgraph.40k), envir = .GlobalEnv)
  on.exit(rm("stringRefGraph", envir = .GlobalEnv), add = TRUE)

  result <- setGraph(hgdp, "stringRefGraph")
  expect_equal(result@gGraph.name, "stringRefGraph")
})

test_that("setGraph rejects non-existent string references", {
  expect_error(
    setGraph(hgdp, "nonexistent_graph"),
    "not found in global environment"
  )
})

#######################
## setColors
#######################

test_that("setColors errors when col.rules has wrong number of columns", {
  bad.rules <- data.frame(
    habitat = c("sea", "land"),
    color   = c("blue", "green"),
    extra   = c(1, 2)
  )
  expect_error(
    setColors(worldgraph.10k, bad.rules),
    "exactly two columns"
  )
})

test_that("setColors errors when attr.name column not in nodes.attr", {
  col.rules <- data.frame(nonexistent = c("sea", "land"), color = c("blue", "green"))
  expect_error(
    setColors(worldgraph.10k, col.rules),
    "not found in x@nodes.attr"
  )
})

test_that("setColors errors when some node attribute values have no color rule", {
  # get rules and remove one habitat value
  col.rules <- getColors(worldgraph.10k, res.type = "rules")
  col.rules <- col.rules[col.rules$habitat != "sea", ]
  expect_error(
    setColors(worldgraph.10k, col.rules),
    "no color rule defined"
  )
})

test_that("setColors errors when color values are invalid R colors", {
  col.rules <- getColors(worldgraph.10k, res.type = "rules")
  col.rules$color[1] <- "notacolor"
  expect_error(
    setColors(worldgraph.10k, col.rules),
    "invalid R color values"
  )
})

test_that("setColors updates @meta$colors with valid rules", {
  rules <- getColors(worldgraph.10k, res.type = "rules")
  rules$color[rules$habitat == "sea"] <- "lightblue"
  result <- setColors(worldgraph.10k, rules)
  new.rules <- getColors(result, res.type = "rules")
  expect_equal(new.rules$color[new.rules$habitat == "sea"], "lightblue")
})

test_that("setColors errors when col.rules has no 'color' column", {
  bad <- data.frame(habitat = c("sea", "land"), other = c("blue", "green"))
  expect_error(
    setColors(worldgraph.10k, bad),
    "must have a column named 'color'"
  )
})

#######################
## getGraph,gData
#######################

test_that("getGraph errors when linked gGraph not in global environment", {
  bad.gdata <- hgdp
  bad.gdata@gGraph.name <- "nonexistent_graph"
  expect_error(
    getGraph(bad.gdata),
    "not found"
  )
})

## getNodesAttr,gData
test_that("getNodesAttr for gData returns same result as for underlying gGraph", {
  gdata.result <- getNodesAttr(hgdp)
  ggraph.result <- getNodesAttr(worldgraph.40k,
    nodes = getNodes(hgdp),
    attr.name = "habitat"
  )
  expect_equal(gdata.result, ggraph.result)
})

#######################
## getEdges
#######################

test_that("getEdges matId and matNames have same number of rows", {
  mat.id <- getEdges(worldgraph.10k, res.type = "matId")
  mat.names <- getEdges(worldgraph.10k, res.type = "matNames")
  expect_equal(nrow(mat.id), nrow(mat.names))
})

test_that("getEdges unique = TRUE returns exactly half the rows of unique = FALSE", {
  all <- getEdges(worldgraph.10k, res.type = "matNames", unique = FALSE)
  unique <- getEdges(worldgraph.10k, res.type = "matNames", unique = TRUE)
  expect_equal(nrow(unique) * 2, nrow(all))
})

#######################
## getCosts
#######################

test_that("getCosts asIs and vector contain same total costs", {
  as.is <- getCosts(worldgraph.10k, res.type = "asIs")
  vector <- getCosts(worldgraph.10k, res.type = "vector")
  expect_equal(sum(unlist(as.is)), sum(vector))
})

test_that("getCosts with unique = TRUE filters edge weights in both formats", {
  all.vec <- getCosts(worldgraph.10k, res.type = "vector", unique = FALSE)
  uniq.vec <- getCosts(worldgraph.10k, res.type = "vector", unique = TRUE)
  expect_lt(length(uniq.vec), length(all.vec))

  allAsIs <- getCosts(worldgraph.10k, res.type = "asIs", unique = FALSE)
  uniqAsIs <- getCosts(worldgraph.10k, res.type = "asIs", unique = TRUE)
  expect_lt(sum(lengths(uniqAsIs)), sum(lengths(allAsIs)))
})

#######################
## getColors
#######################

test_that("getColors returns one colour per node when res.type = 'colors'", {
  res <- getColors(worldgraph.10k, attr.name = "habitat")
  expect_equal(length(res), length(getNodes(worldgraph.10k)))
  expect_true(is.character(res))
})

test_that("getColors accepts a character vector of node names", {
  some.nodes <- head(getNodes(worldgraph.10k), 5)
  res <- getColors(worldgraph.10k, nodes = some.nodes, attr.name = "habitat")
  expect_equal(length(res), 5L)
})

#######################
## getNodeCosts
#######################

test_that("getNodeCosts returns one cost per node", {
  result <- getNodeCosts(worldgraph.10k, attr.name = "habitat")
  expect_equal(length(result), length(getNodes(worldgraph.10k)))
  expect_true(is.numeric(result))
})

test_that("getNodeCosts errors when meta has no costs component", {
  x <- worldgraph.10k
  x@meta$costs <- NULL
  expect_error(
    getNodeCosts(x, attr.name = "habitat"),
    "x@meta does not contain a 'costs' component"
  )
})

#######################
## getCoords
#######################

test_that("getCoords for gData with original = FALSE returns matched node coords", {
  res <- getCoords(hgdp, original = FALSE)
  expect_equal(nrow(res), length(getNodes(hgdp)))
  expect_equal(ncol(res), 2L)
})
