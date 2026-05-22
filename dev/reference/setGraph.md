# Set the linked gGraph for a gData object

The function `setGraph` sets the name of the
[`gGraph`](https://evolecolgroup.github.io/geograph/dev/reference/gGraph-class.md)
object linked to a
[`gData`](https://evolecolgroup.github.io/geograph/dev/reference/gData-class.md)
object. It validates that the named object exists in the global
environment and is a valid
[`gGraph`](https://evolecolgroup.github.io/geograph/dev/reference/gGraph-class.md).

## Usage

``` r
setGraph(x, graph)

# S4 method for class 'gData'
setGraph(x, graph)
```

## Arguments

- x:

  a valid
  [`gData`](https://evolecolgroup.github.io/geograph/dev/reference/gData-class.md)
  object.

- graph:

  either a character string giving the name of a
  [`gGraph`](https://evolecolgroup.github.io/geograph/dev/reference/gGraph-class.md)
  object in the global environment, or a
  [`gGraph`](https://evolecolgroup.github.io/geograph/dev/reference/gGraph-class.md)
  object itself.

## Value

A
[`gData`](https://evolecolgroup.github.io/geograph/dev/reference/gData-class.md)
object with the updated linked
[`gGraph`](https://evolecolgroup.github.io/geograph/dev/reference/gGraph-class.md).

## Functions

- `setGraph(gData)`: Method for gData objects

## See also

[`getGraph`](https://evolecolgroup.github.io/geograph/dev/reference/getGraph.md)
to retrieve the linked graph.
[`gData`](https://evolecolgroup.github.io/geograph/dev/reference/gData-class.md)
for the class definition.

Other accessor_methods:
[`getColors()`](https://evolecolgroup.github.io/geograph/dev/reference/getColors.md),
[`getCoords()`](https://evolecolgroup.github.io/geograph/dev/reference/getCoords.md),
[`getCosts()`](https://evolecolgroup.github.io/geograph/dev/reference/getCosts.md),
[`getData()`](https://evolecolgroup.github.io/geograph/dev/reference/getData.md),
[`getEdges()`](https://evolecolgroup.github.io/geograph/dev/reference/getEdges.md),
[`getGraph()`](https://evolecolgroup.github.io/geograph/dev/reference/getGraph.md),
[`getNodes()`](https://evolecolgroup.github.io/geograph/dev/reference/getNodes.md),
[`getNodesAttr()`](https://evolecolgroup.github.io/geograph/dev/reference/getNodesAttr.md),
[`setColors()`](https://evolecolgroup.github.io/geograph/dev/reference/setColors.md)

## Examples

``` r
myGraph <- dropCosts(rawgraph.40k)
hgdp2 <- setGraph(hgdp, "myGraph")
#> Error in setGraph(hgdp, "myGraph"): gGraph object myGraph not found in global environment.
getGraph(hgdp2)
#> Error in h(simpleError(msg, call)): error in evaluating the argument 'x' in selecting a method for function 'getGraph': object 'hgdp2' not found
```
