# Get costs associated to edges of a gGraph object

The function `getCosts` returns the costs associated to the edges of a
[`gGraph`](https://evolecolgroup.github.io/geograph/reference/gGraph-class.md)
object in different formats. `getNodeCosts` returns the costs associated
to nodes based on a node attribute and the cost rules stored in
`@meta$costs`.

## Usage

``` r
getCosts(x, ...)

# S4 method for class 'gGraph'
getCosts(x, res.type = c("asIs", "vector", "rules"), unique = FALSE, ...)

getNodeCosts(x, ...)

# S4 method for class 'gGraph'
getNodeCosts(x, attr.name, ...)
```

## Arguments

- x:

  a valid
  [`gGraph`](https://evolecolgroup.github.io/geograph/reference/gGraph-class.md)
  object.

- ...:

  other arguments passed to other methods (currently unused).

- res.type:

  a character string indicating the output format:

  - `"asIs"`: a named list of weights for each node's edges (default).

  - `"vector"`: a named numeric vector of all edge weights.

  - `"rules"`: the cost rules `data.frame` stored in `x@meta$costs`,
    with one row per node attribute value and a column named `"cost"`.

- unique:

  logical. If `TRUE`, only unique edge weights are returned. Defaults to
  `FALSE`. Only used when `res.type` is `"asIs"` or `"vector"`.

- attr.name:

  character string. The name of the node attribute used to look up costs
  from `x@meta$costs` (for `getNodeCosts` only).

## Value

For `getCosts`:

- `"asIs"`: a named list of edge weights, one element per node.

- `"vector"`: a named numeric vector of edge weights.

- `"rules"`: a `data.frame` of cost rules from `x@meta$costs`.

For `getNodeCosts`: a numeric vector of costs, one per node.

## Details

In `geoGraph`, costs are equivalent to weights in the `graph` package:
the larger the cost of an edge, the lower the connectivity between its
two nodes.

## Functions

- `getCosts(gGraph)`: Method for gGraph object

- `getNodeCosts()`: Function to get the costs values for nodes

- `getNodeCosts(gGraph)`: Method to get node costs for gGraph object

## See also

[`setCosts`](https://evolecolgroup.github.io/geograph/reference/setCosts.md)
to set edge costs.
[`dropCosts`](https://evolecolgroup.github.io/geograph/reference/dropCosts.md)
to remove all costs.
[`hasCosts`](https://evolecolgroup.github.io/geograph/reference/hasCosts.md)
to check if a graph has costs defined.

Other accessor_methods:
[`getColors()`](https://evolecolgroup.github.io/geograph/reference/getColors.md),
[`getCoords()`](https://evolecolgroup.github.io/geograph/reference/getCoords.md),
[`getData()`](https://evolecolgroup.github.io/geograph/reference/getData.md),
[`getEdges()`](https://evolecolgroup.github.io/geograph/reference/getEdges.md),
[`getGraph()`](https://evolecolgroup.github.io/geograph/reference/getGraph.md),
[`getNodes()`](https://evolecolgroup.github.io/geograph/reference/getNodes.md),
[`getNodesAttr()`](https://evolecolgroup.github.io/geograph/reference/getNodesAttr.md),
[`setColors()`](https://evolecolgroup.github.io/geograph/reference/setColors.md),
[`setGraph()`](https://evolecolgroup.github.io/geograph/reference/setGraph.md)

Other cost_functions:
[`combineCosts()`](https://evolecolgroup.github.io/geograph/reference/combineCosts.md),
[`dropCosts()`](https://evolecolgroup.github.io/geograph/reference/dropCosts.md),
[`hasCosts()`](https://evolecolgroup.github.io/geograph/reference/hasCosts.md),
[`setCosts()`](https://evolecolgroup.github.io/geograph/reference/setCosts.md),
[`setDistCosts()`](https://evolecolgroup.github.io/geograph/reference/setDistCosts.md)

## Examples

``` r
## get edge costs as a vector
head(getCosts(worldgraph.10k, res.type = "vector", unique = TRUE))
#> 67.9955   67.68 67.9953   68.69 68.9955 69.9957 
#>       1       1       1       1       1       1 

## get cost rules
getCosts(worldgraph.10k, res.type = "rules")
#>            habitat cost
#> 1              sea  100
#> 2             land    1
#> 3         mountain   10
#> 4       landbridge    5
#> 5 oceanic crossing   20
#> 6  deselected land  100

## get node costs based on habitat attribute
head(getNodeCosts(worldgraph.10k, attr.name = "habitat"))
#> [1] 100 100 100 100 100 100
```
