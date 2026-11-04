library("geoGraph")

test_that("arbitrary function to set costs", {
  data("worldgraph.40k")
  exp.cost <- function(x1, x2, cost.coeff) {
    exp(-abs(x1 - x2) * cost.coeff)
  }
  worldgraph.40k@nodes.attr$meanProd <- runif(graph::numNodes(getGraph(worldgraph.40k)))
  my.coeff <- 0.5
  test.graph <-
    setCosts(
      worldgraph.40k,
      node.values = worldgraph.40k@nodes.attr$meanProd,
      method = "function",
      FUN = exp.cost,
      cost.coeff = my.coeff
    )
  # now check that we have the right costs
  sample.edge <- names(test.graph@graph@edgeData@data)[1]
  sample.nodes <- as.integer(strsplit(sample.edge, "|", fixed = TRUE)[[1]])
  sample.mean.prod <- worldgraph.40k@nodes.attr$meanProd[sample.nodes]
  expect_equal(
    test.graph@graph@edgeData@data[[1]]$weight,
    exp.cost(sample.mean.prod[1], sample.mean.prod[2], cost.coeff = my.coeff)
  )
})

test_that("setCosts errors when some habitat values have no cost rule", {
  graph.no.cost <- dropCosts(worldgraph.10k)
  graph.no.cost@meta$costs <- data.frame(
    habitat = c("sea", "land"), # missing coast, mountain etc.
    cost    = c(10, 1)
  )
  expect_error(
    setCosts(graph.no.cost, attr.name = "habitat"),
    "The following node attribute values have no cost rule defined"
  )
})

test_that("setCosts with cost.rules updates meta and sets costs in one call", {
  baseline <- setCosts(rawgraph.10k, attr.name = "habitat")
  cost.rules <- getCosts(rawgraph.10k, res.type = "rules")
  cost.rules$cost[cost.rules$habitat == "sea"] <- 50

  result <- setCosts(rawgraph.10k, attr.name = "habitat", cost.rules = cost.rules)

  # meta should be updated
  new.rules <- getCosts(result, res.type = "rules")
  expect_equal(as.numeric(new.rules$cost[new.rules$habitat == "sea"]), 50)
  # edge costs should differ from baseline after changing sea rule
  baseline.w <- vapply(baseline@graph@edgeData@data, function(e) e$weight, numeric(1))
  result.w <- vapply(result@graph@edgeData@data, function(e) e$weight, numeric(1))
  expect_false(isTRUE(all.equal(result.w, baseline.w)))
})

test_that("setCosts with cost.rules must have exactly two columns", {
  cost.rules <- data.frame(
    habitat = c("sea", "land"),
    cost = c(100, 1),
    extra = c(1, 2)
  )
  expect_error(
    setCosts(worldgraph.10k, attr.name = "habitat", cost.rules = cost.rules),
    "exactly two columns"
  )
})

test_that("setCosts with cost.rules second column must be numeric", {
  cost.rules <- data.frame(
    habitat = c("sea", "land"),
    cost = c("high", "low")
  )
  expect_error(
    setCosts(worldgraph.10k, attr.name = "habitat", cost.rules = cost.rules),
    "cost.rules cost column must be numeric"
  )
})

test_that("setCosts with cost.rules must not have NAs in cost column", {
  cost.rules <- data.frame(
    habitat = c("sea", "land"),
    cost = c(100, NA)
  )
  expect_error(
    setCosts(worldgraph.10k, attr.name = "habitat", cost.rules = cost.rules),
    "cost.rules cost column must not contain NA"
  )
})

test_that("setCosts with cost.rules must have unique values in first column", {
  cost.rules <- data.frame(
    habitat = c("sea", "sea", "land"),
    cost = c(100, 1, 1)
  )
  expect_error(
    setCosts(rawgraph.10k, attr.name = "habitat", cost.rules = cost.rules),
    "must contain unique values"
  )
})

test_that("setCosts accepts cost.rules with columns in any order", {
  cost.rules.reversed <- data.frame(
    cost    = c(100, 1),
    habitat = c("sea", "land")
  )
  cost.rules.normal <- data.frame(
    habitat = c("sea", "land"),
    cost    = c(100, 1)
  )

  result.reversed <- setCosts(rawgraph.10k,
    attr.name = "habitat",
    cost.rules = cost.rules.reversed
  )
  result.normal <- setCosts(rawgraph.10k,
    attr.name = "habitat",
    cost.rules = cost.rules.normal
  )

  expect_equal(
    getCosts(result.reversed, res.type = "vector"),
    getCosts(result.normal, res.type = "vector")
  )
})


test_that("setCosts errors when x is not a gGraph and when method = 'function' but FUN is NULL", {
  expect_error(setCosts(list()), "x is not a valid gGraph object")
  expect_error(setCosts("not_a_graph"), "x is not a valid gGraph object")
  expect_error(
    setCosts(worldgraph.10k, attr.name = "habitat", method = "function"),
    "FUN needs to be defined"
  )
})


test_that("setCosts errors when cost.rules is not a data.frame", {
  expect_error(
    setCosts(worldgraph.10k,
      attr.name = "habitat",
      cost.rules = list(habitat = "sea", cost = 10)
    ),
    "cost.rules must be a data.frame"
  )
})

test_that("setCosts errors when cost.rules lacks the attr.name column", {
  cost.rules <- data.frame(foo = c("sea", "land"), cost = c(10, 1))
  expect_error(
    setCosts(worldgraph.10k, attr.name = "habitat", cost.rules = cost.rules),
    "cost.rules must include the column named by attr.name"
  )
})

test_that("setCosts errors when attr.name is absent from x@meta$costs", {
  g <- worldgraph.10k
  g@meta$costs <- data.frame(foo = c("sea", "land"), cost = c(10, 1))
  expect_error(
    setCosts(g, attr.name = "habitat"),
    "attr.name is not documented"
  )
})

test_that("setCosts errors when x has no costs component on the attribute path", {
  g <- worldgraph.10k
  g@meta$costs <- NULL
  expect_error(
    setCosts(g, attr.name = "habitat"),
    "x@meta does not contain a 'costs' component"
  )
})

test_that("setCosts errors when node.values is not numeric", {
  expect_error(
    setCosts(worldgraph.10k, node.values = c("a", "b", "c")),
    "Provided 'node.values' not numeric"
  )
})

test_that("setCosts computes edge costs as the product of node costs", {
  g <- setCosts(worldgraph.10k, node.values = 2, method = "product")
  w <- vapply(g@graph@edgeData@data, function(e) e$weight, numeric(1))
  expect_gt(length(w), 0)
  expect_true(all(w == 4))
})

test_that("setCosts does not chain rules when costs equal attribute values", {
  # nodes coded 1 and 2; code 1 costs 2 and code 2 costs 10.
  # A rule-by-rule replacement would turn 1 -> 2 -> 10.
  x <- makeSquareGrid(n.lon = 2, n.lat = 2, lon.range = c(0, 1), lat.range = c(0, 1))
  x <- setNodesAttr(x, attr.name = "code", values = c(1, 2, 2, 1))
  x <- setCosts(x,
    attr.name = "code", method = "product",
    cost.rules = data.frame(code = c(1, 2), cost = c(2, 10))
  )
  # every edge joins a code-1 node to a code-2 node: 2 * 10
  expect_true(all(getCosts(x, res.type = "vector") == 20))
})

test_that("setCosts gives the same weights as the previous implementation", {
  set.seed(5)
  for (x in list(worldgraph.10k, rawgraph.10k)) {
    N <- length(getNodes(x))
    nv <- runif(N, 1, 10)
    for (method in c("mean", "product")) {
      expect_same_graph(
        getGraph(setCosts(x, node.values = nv, method = method)),
        setCostsGraphRef(x, nv, method)
      )
    }
    ## asymmetric and symmetric functions, extra arguments
    FUN <- function(a, b, k = 2) a + k * b
    expect_same_graph(
      getGraph(setCosts(x, node.values = nv, method = "function", FUN = FUN)),
      setCostsGraphRef(x, nv, "function", FUN)
    )
    expect_same_graph(
      getGraph(setCosts(x, node.values = nv, method = "function", FUN = FUN, k = 5)),
      setCostsGraphRef(x, nv, "function", FUN, k = 5)
    )
    expect_same_graph(
      getGraph(setCosts(x, node.values = nv, method = "function", FUN = pmax)),
      setCostsGraphRef(x, nv, "function", pmax)
    )
  }

  ## integer and named node values, recycled node values
  x <- worldgraph.10k
  N <- length(getNodes(x))
  nv <- sample(1:5, N, replace = TRUE)
  names(nv) <- getNodes(x)
  for (method in c("mean", "product")) {
    expect_same_graph(getGraph(setCosts(x, node.values = nv, method = method)), setCostsGraphRef(x, nv, method))
  }
  expect_same_graph(
    getGraph(setCosts(x, node.values = c(1, 3), method = "product")),
    setCostsGraphRef(x, rep(c(1, 3), length = N), "product")
  )

  ## costs from a node attribute and cost rules
  habitat <- as.character(getNodesAttr(x, attr.name = "habitat")$habitat)
  rules <- data.frame(habitat = unique(habitat), cost = seq_along(unique(habitat)) * 1.5)
  expect_same_graph(
    getGraph(setCosts(x, attr.name = "habitat", cost.rules = rules, method = "function", FUN = function(a, b) a - b)),
    setCostsGraphRef(x, rules$cost[match(habitat, rules$habitat)], "function", function(a, b) a - b)
  )
  ## integer weights on a graph where every node has an edge stay integer
  y <- keepMaxConnectedSet(worldgraph.10k)
  nv <- sample(1:5, length(getNodes(y)), replace = TRUE)
  res <- getGraph(setCosts(y, node.values = nv, method = "product"))
  expect_same_graph(res, setCostsGraphRef(y, nv, "product"))
  expect_type(graph::edgeWeights(res)[[1]], "integer")
  expect_error(
    setCosts(y, node.values = nv, method = "function", FUN = function(a, b) a > b),
    "must be numeric"
  )
})

test_that("setCosts errors if FUN does not return one value per neighbour", {
  expect_error(
    setCosts(worldgraph.10k, node.values = 1, method = "function", FUN = function(a, b) max(a, b)),
    "one value for each neighbour"
  )
})

test_that("dropDeadEdges gives the same graph as the previous implementation", {
  set.seed(6)
  x <- setCosts(rawgraph.10k, node.values = runif(length(getNodes(rawgraph.10k))))
  th <- median(getCosts(x, res.type = "vector"))
  expect_same_graph(getGraph(dropDeadEdges(x, th)), dropDeadEdgesGraphRef(x, th))
  ## asymmetric weights are swapped, as before
  y <- setCosts(rawgraph.10k, node.values = runif(length(getNodes(rawgraph.10k))),
    method = "function", FUN = function(a, b) a + 2 * b
  )
  expect_same_graph(getGraph(dropDeadEdges(y, Inf)), dropDeadEdgesGraphRef(y, Inf))
})

test_that("combineCosts gives the same graph as the previous implementation", {
  set.seed(7)
  N <- length(getNodes(rawgraph.10k))
  x1 <- setCosts(rawgraph.10k, node.values = runif(N))
  x2 <- setCosts(rawgraph.10k, node.values = runif(N), method = "function", FUN = function(a, b) a + 2 * b)
  for (method in c("sum", "product")) {
    expect_same_graph(getGraph(combineCosts(x1, x2, method = method)), combineCostsGraphRef(x1, x2, method))
  }
  FUN <- function(a, b, k = 1) {
    stopifnot(identical(names(a), names(b)), !is.null(names(a)))
    k * a - b
  }
  expect_same_graph(
    getGraph(combineCosts(x1, x2, method = "function", FUN = FUN, k = 3)),
    combineCostsGraphRef(x1, x2, "function", FUN, k = 3)
  )
})

test_that("setDistCosts gives the same weights as the previous implementation", {
  for (x in list(rawgraph.10k, worldgraph.10k)) {
    expect_same_graph(getGraph(setDistCosts(x)), setDistCostsGraphRef(x))
  }
})
