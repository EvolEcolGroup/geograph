# Get the graph component of a gGraph or gData object

The function `getGraph` returns the
[`graph::graphNEL`](https://rdrr.io/pkg/graph/man/graphNEL-class.html)
object stored in a
[`gGraph`](https://evolecolgroup.github.io/geograph/dev/reference/gGraph-class.md)
or
[`gData`](https://evolecolgroup.github.io/geograph/dev/reference/gData-class.md)
object.

## Usage

``` r
getGraph(x, ...)

# S4 method for class 'gGraph'
getGraph(x, ...)

# S4 method for class 'gData'
getGraph(x, ...)
```

## Arguments

- x:

  a valid
  [`gGraph`](https://evolecolgroup.github.io/geograph/dev/reference/gGraph-class.md)
  or
  [`gData`](https://evolecolgroup.github.io/geograph/dev/reference/gData-class.md)
  object.

- ...:

  additional arguments passed to other methods (currently unused).

## Value

A [`graph::graphNEL`](https://rdrr.io/pkg/graph/man/graphNEL-class.html)
object.

## Functions

- `getGraph(gGraph)`: Method for gGraph objects

- `getGraph(gData)`: Method for gData objects

## See also

[`getNodes`](https://evolecolgroup.github.io/geograph/dev/reference/getNodes.md),
[`getEdges`](https://evolecolgroup.github.io/geograph/dev/reference/getEdges.md),
[`getCoords`](https://evolecolgroup.github.io/geograph/dev/reference/getCoords.md)

Other accessor_methods:
[`getColors()`](https://evolecolgroup.github.io/geograph/dev/reference/getColors.md),
[`getCoords()`](https://evolecolgroup.github.io/geograph/dev/reference/getCoords.md),
[`getCosts()`](https://evolecolgroup.github.io/geograph/dev/reference/getCosts.md),
[`getData()`](https://evolecolgroup.github.io/geograph/dev/reference/getData.md),
[`getEdges()`](https://evolecolgroup.github.io/geograph/dev/reference/getEdges.md),
[`getNodes()`](https://evolecolgroup.github.io/geograph/dev/reference/getNodes.md),
[`getNodesAttr()`](https://evolecolgroup.github.io/geograph/dev/reference/getNodesAttr.md),
[`setColors()`](https://evolecolgroup.github.io/geograph/dev/reference/setColors.md),
[`setGraph()`](https://evolecolgroup.github.io/geograph/dev/reference/setGraph.md)

## Examples

``` r
getGraph(worldgraph.10k)
#> A graphNEL graph with undirected edges
#> Number of Nodes = 10242 
#> Number of Edges = 6954 
```
