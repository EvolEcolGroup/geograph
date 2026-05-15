# Remove all costs from a gGraph object

The function `dropCosts` removes all edge weights (costs) from a
[`gGraph`](https://evolecolgroup.github.io/geograph/dev/reference/gGraph-class.md)
object, returning an unweighted graph.

## Usage

``` r
dropCosts(x, ...)

# S4 method for class 'gGraph'
dropCosts(x)
```

## Arguments

- x:

  a valid
  [`gGraph`](https://evolecolgroup.github.io/geograph/dev/reference/gGraph-class.md)
  object.

- ...:

  additional arguments passed to other methods (currently unused).

## Value

A
[`gGraph`](https://evolecolgroup.github.io/geograph/dev/reference/gGraph-class.md)
object with all edge costs removed.

## Functions

- `dropCosts(gGraph)`: Method for gGraph objects

## See also

[`getCosts`](https://evolecolgroup.github.io/geograph/dev/reference/getCosts.md)
to retrieve edge costs,
[`setCosts`](https://evolecolgroup.github.io/geograph/dev/reference/setCosts.md)
to set edge costs.
[`hasCosts`](https://evolecolgroup.github.io/geograph/dev/reference/hasCosts.md)
to check if a graph has costs defined.

## Examples

``` r
hasCosts(rawgraph.10k)  
#> [1] TRUE
x <- dropCosts(worldgraph.10k)
hasCosts(x)               
#> [1] FALSE
```
