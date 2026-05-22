# Combine the costs of two gGraph objects

The function `combineCosts` combines the edge costs of two
[gGraph](https://evolecolgroup.github.io/geograph/dev/reference/gGraph-class.md)
objects. The first object is used as a template to generate the objects
with the combined costs. Two two
[gGraph](https://evolecolgroup.github.io/geograph/dev/reference/gGraph-class.md)
objects must have the same edges.

## Usage

``` r
combineCosts(x1, x2, method = c("sum", "product", "function"), FUN = NULL, ...)
```

## Arguments

- x1:

  The first gGraph (which will be used as a template to build the
  combined gGraph)

- x2:

  The second gGraph from which costs will be combined

- method:

  a character string indicating which method should be used to combined
  edge cost from the two gGraph. Currently available options are 'sum',
  'prod' and 'function', where the combined costs are computed as the
  sum, the product or a custom function (defined in `FUN`) of the costs
  of its nodes.

- FUN:

  a function used to compute the cost between two nodes (needed if
  `method="function"`).

- ...:

  additional parameters to be passed to `FUN`.

## Value

A
[gGraph](https://evolecolgroup.github.io/geograph/dev/reference/gGraph-class.md)
object with the newly defined costs, based on the combination of the two
gGraph objects, used as weightings of edges.

## Details

Note that costs are inversely proportional to connectivity between
edges: the larger the cost associated to an edge, the lower the
connectivity between the two concerned nodes.  

Also note that 'costs' defined in `geoGraph` are equivalent to 'weights'
as defined in `graph` and `RBGL` packages.

## See also

[setCosts](https://evolecolgroup.github.io/geograph/dev/reference/setCosts.md)
to set costs for a single gGraph object

Other cost_functions:
[`dropCosts()`](https://evolecolgroup.github.io/geograph/dev/reference/dropCosts.md),
[`getCosts()`](https://evolecolgroup.github.io/geograph/dev/reference/getCosts.md),
[`hasCosts()`](https://evolecolgroup.github.io/geograph/dev/reference/hasCosts.md),
[`setCosts()`](https://evolecolgroup.github.io/geograph/dev/reference/setCosts.md),
[`setDistCosts()`](https://evolecolgroup.github.io/geograph/dev/reference/setDistCosts.md)

## Examples

``` r
 
data("worldgraph.40k")
# new graph with custom cost function
exp.cost <- function(x1, x2, cost.coeff) {
  exp(-abs(x1 - x2) * cost.coeff)
}
# create a set of node costs
worldgraph.40k@nodes.attr$meanProd <- runif(n = 40962)
new_costs_graph <-
  setCosts(
    worldgraph.40k,
    node.values = worldgraph.40k@nodes.attr$meanProd,
    method = "function",
    FUN = exp.cost,
    cost.coeff = 0.5
  )
# combine costs from the original graph with the new costs
combine_costs_graph <- combineCosts(worldgraph.40k, new_costs_graph, method = "sum")
```
