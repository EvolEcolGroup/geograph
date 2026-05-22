# Set friction in a gGraph object

The function `setCosts` defines costs for the edges of a
[`gGraph`](https://evolecolgroup.github.io/geograph/dev/reference/gGraph-class.md)
object according to a node attribute and cost rules defined in
`@meta$costs`. Each node has a value for the chosen attribute which is
associated to a cost. The cost of an edge is computed as a function of
the costs of its two nodes.

## Usage

``` r
setCosts(
  x,
  attr.name = NULL,
  node.values = NULL,
  cost.rules = NULL,
  method = c("mean", "product", "function"),
  FUN = NULL,
  ...
)
```

## Arguments

- x:

  a
  [`gGraph`](https://evolecolgroup.github.io/geograph/dev/reference/gGraph-class.md)
  object with at least one node attribute and a `@meta$costs` component
  (see `worldgraph.10k` for an example).

- attr.name:

  the name of the node attribute used to compute costs.

- node.values:

  a numeric vector of costs for the nodes. If provided, overrides
  `attr.name`.

- cost.rules:

  a two-column `data.frame` to update `x@meta$costs` before computing
  edge costs. If `NULL`, existing `x@meta$costs` is used.

- method:

  how edge costs are computed from node costs: `"mean"`, `"product"`, or
  `"function"` (requires `FUN`).

- FUN:

  a function to compute edge cost from two node costs. Required when
  `method = "function"`.

- ...:

  additional arguments passed to `FUN`.

## Value

A
[`gGraph`](https://evolecolgroup.github.io/geograph/dev/reference/gGraph-class.md)
object with the newly defined edge costs.

## Details

Costs are inversely proportional to connectivity: the larger the cost of
an edge, the lower the connectivity between its two nodes. Costs in
`geoGraph` are equivalent to weights in the `graph` and `RBGL` packages.

## See also

[`dropDeadEdges`](https://evolecolgroup.github.io/geograph/dev/reference/dropDeadEdges.md),
[`getCosts`](https://evolecolgroup.github.io/geograph/dev/reference/getCosts.md),
[`hasCosts`](https://evolecolgroup.github.io/geograph/dev/reference/hasCosts.md)

Other cost_functions:
[`combineCosts()`](https://evolecolgroup.github.io/geograph/dev/reference/combineCosts.md),
[`dropCosts()`](https://evolecolgroup.github.io/geograph/dev/reference/dropCosts.md),
[`getCosts()`](https://evolecolgroup.github.io/geograph/dev/reference/getCosts.md),
[`hasCosts()`](https://evolecolgroup.github.io/geograph/dev/reference/hasCosts.md),
[`setDistCosts()`](https://evolecolgroup.github.io/geograph/dev/reference/setDistCosts.md)

## Examples

``` r
## get and modify cost rules then set costs in one call
cost.rules <- getCosts(worldgraph.10k, res.type = "rules")
cost.rules
#>            habitat cost
#> 1              sea  100
#> 2             land    1
#> 3         mountain   10
#> 4       landbridge    5
#> 5 oceanic crossing   20
#> 6  deselected land  100

## make sea travel cheaper
cost.rules$cost[cost.rules$habitat == "sea"] <- 50

## update rules and set costs in one call
x <- setCosts(worldgraph.10k, attr.name = "habitat", cost.rules = cost.rules)
```
