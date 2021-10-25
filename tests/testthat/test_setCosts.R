library("geoGraph")
context("test cost setting")

test_that("arbitrary function to set costs", {
  data("worldgraph.40k")
  exp.cost <- function(x1, x2, cost.coeff) {
    exp(-abs(x1 - x2) * cost.coeff)
  }
  my_coeff <- 0.5
  test_graph <-
    setCosts(
      worldgraph.40k,
      node.costs = worldgraph.40k@nodes.attr$meanProd,
      method = "function",
      FUN = exp.cost,
      cost.coeff = my_coeff
    )
  #now check that we have the right costs
  sample_edge <- names(test_graph@graph@edgeData@data)[1]
  sample_nodes <- as.integer(strsplit(sample_edge, "|", fixed = TRUE)[[1]])
  sample_meanProd <- worldgraph.40k@nodes.attr$meanProd[sample_nodes]
  expect_equal(
    test_graph@graph@edgeData@data[[1]]$weight,
    exp.cost(sample_meanProd[1], sample_meanProd[2], cost.coeff = my_coeff)
  )
})