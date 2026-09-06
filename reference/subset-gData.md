# Subset a gData object

Select a subset of locations from a
[`gData`](https://evolecolgroup.github.io/geograph/reference/gData-class.md)
object by index, logical vector, or node name. The `@coords`,
`@nodes.id`, and `@data` slots are all subsetted consistently.

## Arguments

- x:

  a valid
  [`gData`](https://evolecolgroup.github.io/geograph/reference/gData-class.md)
  object.

- i:

  indices for subsetting locations — a logical vector, integer indices,
  or character node names. If missing, all locations are kept.

- j:

  indices for subsetting data columns. If missing, all columns are kept.

- ...:

  additional arguments passed to the `[` method of `@data`.

- drop:

  logical passed to the `[` method of `@data`. Defaults to `FALSE`.

## Value

A
[`gData`](https://evolecolgroup.github.io/geograph/reference/gData-class.md)
object with `@coords`, `@nodes.id`, and `@data` all subsetted
consistently.

## See also

[`getCoords`](https://evolecolgroup.github.io/geograph/reference/getCoords.md),
[`getNodes`](https://evolecolgroup.github.io/geograph/reference/getNodes.md),
[`getData`](https://evolecolgroup.github.io/geograph/reference/getData.md)

Other basic_methods:
[`subset-gGraph`](https://evolecolgroup.github.io/geograph/reference/subset-gGraph.md)

## Examples

``` r
## subset to northern hemisphere populations
north <- hgdp[hgdp@data$Latitude > 40]
plot(worldgraph.40k, reset = TRUE)
#> Spherical geometry (s2) switched off
#> Spherical geometry (s2) switched on
points(north)
```
