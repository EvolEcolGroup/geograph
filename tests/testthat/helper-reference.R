## Test-only helpers for the performance rewrites (see data-raw/speed_plan.md).

## Compare two graphNEL objects on what matters to geoGraph. Do not use
## identical() on the graphNEL objects themselves: the internal order of the
## @edgeData@data entries can differ while the graph is the same.
expect_same_graph <- function(a, b) {
  expect_identical(graph::nodes(a), graph::nodes(b))
  expect_identical(graph::edges(a), graph::edges(b))
  expect_identical(graph::edgeWeights(a), graph::edgeWeights(b))
  expect_identical(graph::edgemode(a), graph::edgemode(b))
  expect_identical(length(a@edgeData@data), length(b@edgeData@data))
}
