# Test if a set of nodes form a connected set

This function tests if a set of nodes form a connected set on a
[`gData`](https://evolecolgroup.github.io/geograph/dev/reference/gData-class.md)
or
[`gGraph`](https://evolecolgroup.github.io/geograph/dev/reference/gGraph-class.md)
object.

## Usage

``` r
# S4 method for class 'gData'
isConnected(object, ...)

# S4 method for class 'gGraph'
isConnected(object, ...)
```

## Arguments

- object:

  a
  [gData](https://evolecolgroup.github.io/geograph/dev/reference/gData-class.md)
  or
  [gGraph](https://evolecolgroup.github.io/geograph/dev/reference/gGraph-class.md)
  object

- ...:

  other arguments passed to other methods.

## Value

a single logical value, being TRUE if nodes form a connected set.

## Details

This function is a method for both
[gData](https://evolecolgroup.github.io/geograph/dev/reference/gData-class.md)
and
[gGraph](https://evolecolgroup.github.io/geograph/dev/reference/gGraph-class.md)
objects. For a
[gData](https://evolecolgroup.github.io/geograph/dev/reference/gData-class.md)
object, it tests if all the nodes in the object form a connected set on
the associated
[gGraph](https://evolecolgroup.github.io/geograph/dev/reference/gGraph-class.md)
object. For a
[gGraph](https://evolecolgroup.github.io/geograph/dev/reference/gGraph-class.md)
object, it tests if all nodes in the graph form a connected set. Use
[`areConnected()`](https://evolecolgroup.github.io/geograph/dev/reference/areConnected.md)
if you want to test if a specific subset of nodes form a connected set.

## See also

Other connectivity_functions:
[`areConnected()`](https://evolecolgroup.github.io/geograph/dev/reference/areConnected.md),
[`areNeighbours()`](https://evolecolgroup.github.io/geograph/dev/reference/areNeighbours.md),
[`isReachable()`](https://evolecolgroup.github.io/geograph/dev/reference/isReachable.md)

## Examples

``` r
# in the world graph, there are nodes that are not connected
isConnected(worldgraph.10k)
#> [1] FALSE
# all the nodes in the hgdp gData object are connected
isConnected(hgdp)
#> [1] TRUE
```
