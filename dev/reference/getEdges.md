# Get edges from a gGraph object

The function `getEdges` returns the edges of a
[gGraph](https://evolecolgroup.github.io/geograph/dev/reference/gGraph-class.md)
object using different possible outputs.

## Usage

``` r
getEdges(x, ...)

# S4 method for class 'gGraph'
getEdges(x, res.type = c("asIs", "matNames", "matId"), unique = FALSE, ...)
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

  a logical indicating whether all returned edges should be unique
  (TRUE) or if duplicated edges should be allowed (TRUE, default).

## Value

The output depends on the value of the argument `res.type`:  

- `asIs`: output is a named list of nodes, each slot containing nodes
  forming an edge with one given node. This format is that of the
  `edges` accessor for
  [`graph::graphNEL`](https://rdrr.io/pkg/graph/man/graphNEL-class.html)
  objects.  

- `matNames`: a matrix with two columns giving couples of node names
  forming edges.  

- `matId`: a matrix with two columns giving couples of node indices
  forming edges.  

## Functions

- `getEdges(gGraph)`: Method for gGraph objects

## See also

[`setEdges`](https://evolecolgroup.github.io/geograph/dev/reference/setEdges.md)
to add or remove edges.
[`geo.add.edges`](https://evolecolgroup.github.io/geograph/dev/reference/geo.add.edges.md)
and
[`geo.remove.edges`](https://evolecolgroup.github.io/geograph/dev/reference/geo.add.edges.md)
for interactive versions.

Other accessor_methods:
[`getColors()`](https://evolecolgroup.github.io/geograph/dev/reference/getColors.md),
[`getCoords()`](https://evolecolgroup.github.io/geograph/dev/reference/getCoords.md),
[`getCosts()`](https://evolecolgroup.github.io/geograph/dev/reference/getCosts.md),
[`getData()`](https://evolecolgroup.github.io/geograph/dev/reference/getData.md),
[`getGraph()`](https://evolecolgroup.github.io/geograph/dev/reference/getGraph.md),
[`getNodes()`](https://evolecolgroup.github.io/geograph/dev/reference/getNodes.md),
[`getNodesAttr()`](https://evolecolgroup.github.io/geograph/dev/reference/getNodesAttr.md),
[`setColors()`](https://evolecolgroup.github.io/geograph/dev/reference/setColors.md),
[`setGraph()`](https://evolecolgroup.github.io/geograph/dev/reference/setGraph.md)

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
head(getEdges(worldgraph.10k, res.type = "matId", unique = TRUE))
#>      Vi   Vj
#> [1,] 67 9955
#> [2,] 67  387
#> [3,] 67   68
#> [4,] 67 9953
#> [5,] 67  388
#> [6,] 68  388
```
