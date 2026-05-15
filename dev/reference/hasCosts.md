# Check if a gGraph has costs

This function tests whether a
[gGraph](https://evolecolgroup.github.io/geograph/dev/reference/gGraph-class.md)
has costs associated to its edges.

## Usage

``` r
hasCosts(x)
```

## Arguments

- x:

  a valid
  [gGraph](https://evolecolgroup.github.io/geograph/dev/reference/gGraph-class.md).

## Value

a logical value is returned.

## Details

This low-level function is designed to be called by other procedures of
[geoGraph](https://evolecolgroup.github.io/geograph/dev/reference/geoGraph-package.md).
However, it can sometimes be useful by itself. Note that unlike other
functions in `geoGraph`, this function does not test for the validity of
the provided arguments (for speed purposes).

## Examples

``` r

hasCosts(rawgraph.10k)
#> [1] TRUE
```
