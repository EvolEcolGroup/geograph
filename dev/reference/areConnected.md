# Test if a set of nodes form a connected set

This function tests if a set of nodes form a connected set on a
[`gGraph`](https://evolecolgroup.github.io/geograph/dev/reference/gGraph-class.md)
object.

## Usage

``` r
areConnected(x, nodes)
```

## Arguments

- x:

  a
  [gGraph](https://evolecolgroup.github.io/geograph/dev/reference/gGraph-class.md)
  object

- nodes:

  a vector of node names

## Value

a single logical value, being TRUE if nodes form a connected set.

## Details

@details This function is very similar to
[`isConnected()`](https://evolecolgroup.github.io/geograph/dev/reference/isConnected.md),
but it allows the user to specify a subset of nodes to test for
connectivity, whereas
[`isConnected()`](https://evolecolgroup.github.io/geograph/dev/reference/isConnected.md)
tests if all nodes in the object form a connected set. Note that
[`isConnected()`](https://evolecolgroup.github.io/geograph/dev/reference/isConnected.md)
is a method for both
[gGraph](https://evolecolgroup.github.io/geograph/dev/reference/gGraph-class.md)
and
[gData](https://evolecolgroup.github.io/geograph/dev/reference/gData-class.md)
objects, whereas `areConnected()` is only a method for
[gGraph](https://evolecolgroup.github.io/geograph/dev/reference/gGraph-class.md)
objects.

This is an implementation of the
[`graph::isConnected()`](https://rdrr.io/pkg/graph/man/graph-class.html)
function for data classes in `geoGraph`.

## See also

Other connectivity_functions:
[`areNeighbours()`](https://evolecolgroup.github.io/geograph/dev/reference/areNeighbours.md),
[`isConnected,gData-method`](https://evolecolgroup.github.io/geograph/dev/reference/isConnected.md),
[`isReachable()`](https://evolecolgroup.github.io/geograph/dev/reference/isReachable.md)

## Examples

``` r
# create a small square graph
test_graph <- makeGrid(25, lon.range = c(1,5), lat.range = c(1,5))
# test that the function correctly identifies connected sets
# 1, 9, and 10 are connected
areConnected(test_graph, nodes = c("1", "9", "10"))
#> [1] TRUE
# even though they are not neighbours
areNeighbours(V1= "1", V2 = "9", graph = test_graph)
#>  1->9 
#> FALSE 
areNeighbours(V1= "1", V2 = "10", graph = test_graph)
#> 1->10 
#> FALSE 
```
