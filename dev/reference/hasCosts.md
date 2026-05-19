# Check if a gGraph has costs

This function tests whether a
[`gGraph`](https://evolecolgroup.github.io/geograph/dev/reference/gGraph-class.md)
has heterogeneous costs associated to its edges.

## Usage

``` r
hasCosts(x)
```

## Arguments

- x:

  a valid
  [`gGraph`](https://evolecolgroup.github.io/geograph/dev/reference/gGraph-class.md)
  object.

## Value

`TRUE` if the graph has heterogeneous edge costs, `FALSE` if costs are
absent or uniform.

## Details

This low-level function is designed to be called by other procedures of
[geoGraph](https://evolecolgroup.github.io/geograph/dev/reference/geoGraph-package.md).
However, it can sometimes be useful by itself. Note that a
[`gGraph`](https://evolecolgroup.github.io/geograph/dev/reference/gGraph-class.md)
with all costs set uniform will also result in FALSE. Unlike other
functions in
[geoGraph](https://evolecolgroup.github.io/geograph/dev/reference/geoGraph-package.md),
this function does not test for the validity of the provided arguments
(for speed purposes).

## See also

Other cost_functions:
[`combineCosts()`](https://evolecolgroup.github.io/geograph/dev/reference/combineCosts.md),
[`setCosts()`](https://evolecolgroup.github.io/geograph/dev/reference/setCosts.md),
[`setDistCosts()`](https://evolecolgroup.github.io/geograph/dev/reference/setDistCosts.md)

## Examples

``` r

hasCosts(rawgraph.10k)   # TRUE
#> [1] TRUE
hasCosts(worldgraph.10k) # FALSE as not all edges have costs
#> [1] FALSE
```
