# Get costs associated to edges of a gGraph object

The function `getCosts` returns the costs associated to the edges of a
[gGraph](https://evolecolgroup.github.io/geograph/dev/reference/gGraph-class.md)
object using different possible outputs. These outputs are designed to
match possible outputs of
[`getEdges`](https://evolecolgroup.github.io/geograph/dev/reference/getEdges.md)
function.

## Usage

``` r
getCosts(x, ...)

# S4 method for class 'gGraph'
getCosts(x, res.type = c("asIs", "vector"), unique = FALSE, ...)

getNodeCosts(x, ...)

# S4 method for class 'gGraph'
getNodeCosts(x, attr.name, ...)
```

## Arguments

- x:

  a valid
  [gGraph](https://evolecolgroup.github.io/geograph/dev/reference/gGraph-class.md).

- ...:

  other arguments passed to other methods (currently unused).

- res.type:

  a character string indicating which kind of output should be used. See
  value.

- unique:

  a logical indicating whether the costs should be returned for unique
  edges (TRUE), or if duplicate edges should be considered as well
  (TRUE, default).

- attr.name:

  the name of the node attribute used to define node costs.

## Value

The output depends on the value of the argument `res.type`:  

- `asIs`: output is a named list of weights, each slot containing
  weights associated to the edges stemming from one given node. This
  format is that of the `weights` accessor for
  [`graph::graphNEL`](https://rdrr.io/pkg/graph/man/graphNEL-class.html)
  objects.  

- `vector`: a vector of weights; this output matches matrix outputs of
  [`getEdges`](https://evolecolgroup.github.io/geograph/dev/reference/getEdges.md).  

## Details

`getNodeCosts` returns the costs associated to nodes based on one node
attribute.

The notion of 'costs' in the context of
[gGraph](https://evolecolgroup.github.io/geograph/dev/reference/gGraph-class.md)
objects is identical to the concept of 'weights' in the library `graph`
(and thus
[`graph::graphNEL`](https://rdrr.io/pkg/graph/man/graphNEL-class.html))
objects. The larger it is for an edge, the less connectivity there is
between the couple of concerned nodes.

## Functions

- `getCosts(gGraph)`: Method for gGraph object

- `getNodeCosts()`: Function to get the costs values for nodes

- `getNodeCosts(gGraph)`: Method to get node costs for gGraph object

## See also

[`setCosts`](https://evolecolgroup.github.io/geograph/dev/reference/setCosts.md)
to set edge costs.
[`dropCosts`](https://evolecolgroup.github.io/geograph/dev/reference/dropCosts.md)
to remove all costs.
[`hasCosts`](https://evolecolgroup.github.io/geograph/dev/reference/hasCosts.md)
to check if a graph has costs defined.

Other accessor_methods:
[`getColors()`](https://evolecolgroup.github.io/geograph/dev/reference/getColors.md),
[`getCoords()`](https://evolecolgroup.github.io/geograph/dev/reference/getCoords.md),
[`getData()`](https://evolecolgroup.github.io/geograph/dev/reference/getData.md),
[`getEdges()`](https://evolecolgroup.github.io/geograph/dev/reference/getEdges.md),
[`getGraph()`](https://evolecolgroup.github.io/geograph/dev/reference/getGraph.md),
[`getNodes()`](https://evolecolgroup.github.io/geograph/dev/reference/getNodes.md),
[`getNodesAttr()`](https://evolecolgroup.github.io/geograph/dev/reference/getNodesAttr.md)

## Examples

``` r

head(getEdges(worldgraph.10k, res.type = "matNames", unique = TRUE))
#>      Vi   Vj    
#> [1,] "67" "9955"
#> [2,] "67" "68"  
#> [3,] "67" "9953"
#> [4,] "68" "69"  
#> [5,] "68" "9955"
#> [6,] "69" "9957"
head(getCosts(worldgraph.10k, res.type = "vector", unique = TRUE))
#> 67.9955   67.68 67.9953   68.69 68.9955 69.9957 
#>       1       1       1       1       1       1 
```
