library("geoGraph")

test_that("arbitrary function to set costs", {
  data("worldgraph.40k")
  exp.cost <- function(x1, x2, cost.coeff) {
    exp(-abs(x1 - x2) * cost.coeff)
  }
  worldgraph.40k@nodes.attr$meanProd <- runif(graph::numNodes(getGraph(worldgraph.40k)))
  my_coeff <- 0.5
  test_graph <-
    setCosts(
      worldgraph.40k,
      node.values = worldgraph.40k@nodes.attr$meanProd,
      method = "function",
      FUN = exp.cost,
      cost.coeff = my_coeff
    )
  # now check that we have the right costs
  sample_edge <- names(test_graph@graph@edgeData@data)[1]
  sample_nodes <- as.integer(strsplit(sample_edge, "|", fixed = TRUE)[[1]])
  sample_meanProd <- worldgraph.40k@nodes.attr$meanProd[sample_nodes]
  expect_equal(
    test_graph@graph@edgeData@data[[1]]$weight,
    exp.cost(sample_meanProd[1], sample_meanProd[2], cost.coeff = my_coeff)
  )
})

test_that("setCosts errors when some habitat values have no cost rule", {
  graph.no.cost <- dropCosts(worldgraph.10k)
  graph.no.cost@meta$costs <- data.frame(
    habitat = c("sea", "land"),  # missing coast, mountain etc.
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
  baseline_w <- vapply(baseline@graph@edgeData@data, function(e) e$weight, numeric(1))
  result_w <- vapply(result@graph@edgeData@data, function(e) e$weight, numeric(1))
  expect_false(isTRUE(all.equal(result_w, baseline_w)))
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
  
  result.reversed <- setCosts(rawgraph.10k, attr.name = "habitat",
                              cost.rules = cost.rules.reversed)
  result.normal   <- setCosts(rawgraph.10k, attr.name = "habitat",
                              cost.rules = cost.rules.normal)

  expect_equal(
    getCosts(result.reversed, res.type = "vector"),
    getCosts(result.normal,   res.type = "vector")
  )
})
