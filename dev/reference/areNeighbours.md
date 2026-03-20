# Tests connectivity between pairs of nodes

Tests connectivity between pairs of nodes. This function tests if two
nodes are directly connected with an edge (i.e. if they are neighbours).

## Usage

``` r
areNeighbours(V1, V2, graph)
```

## Arguments

- V1:

  A vector of node names.

- V2:

  A vector of node names of the same length as `V1`.

- graph:

  An object of class
  [gGraph](https://evolecolgroup.github.io/geograph/dev/reference/gGraph-class.md)
  or
  [`graph::graphNEL`](https://rdrr.io/pkg/graph/man/graphNEL-class.html)
  (usually in the `@graph` slot of a
  [gGraph](https://evolecolgroup.github.io/geograph/dev/reference/gGraph-class.md)
  object.

## Value

a vector of logical, having one value for each pair of nodes.

## See also

Other connectivity_functions:
[`areConnected()`](https://evolecolgroup.github.io/geograph/dev/reference/areConnected.md),
[`isConnected,gData-method`](https://evolecolgroup.github.io/geograph/dev/reference/isConnected.md)

## Examples

``` r
# create a small square graph
test_graph <- makeGrid(25, lon.range = c(1,5), lat.range = c(1,5))
# get the coordinates of the first 10 nodes
getCoords(test_graph)[1:10, ]
#>    lon lat
#> 1    1   5
#> 2    1   4
#> 3    1   3
#> 4    1   2
#> 5    1   1
#> 6    2   5
#> 7    2   4
#> 8    2   3
#> 9    2   2
#> 10   2   1
# test that the function correctly identifies neighbours
# 1 and 2 are neighbours, but 1 and 9 are not
areNeighbours(V1 = c("1","1"), V2 = c("2","9"), graph = test_graph)
#>  1->2  1->9 
#>  TRUE FALSE 
```
