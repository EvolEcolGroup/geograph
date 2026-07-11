# Subset a gGraph object

Select a subset of nodes from a
[`gGraph`](https://evolecolgroup.github.io/geograph/dev/reference/gGraph-class.md)
object by index, logical vector, or node name. The `@coords`,
`@nodes.attr`, and `@graph` slots are all subsetted consistently.

## Arguments

- x:

  a valid
  [`gGraph`](https://evolecolgroup.github.io/geograph/dev/reference/gGraph-class.md)
  object.

- i:

  indices for subsetting nodes — a logical vector, integer indices, or
  character node names. If missing, all nodes are kept.

- j:

  indices for subsetting node attributes (columns of `@nodes.attr`). If
  missing, all attributes are kept.

- ...:

  additional arguments (currently unused).

- drop:

  logical, currently unused.

## Value

A
[`gGraph`](https://evolecolgroup.github.io/geograph/dev/reference/gGraph-class.md)
object containing only the selected nodes.

## See also

[`getNodes`](https://evolecolgroup.github.io/geograph/dev/reference/getNodes.md)
to retrieve node names.
[`isInArea`](https://evolecolgroup.github.io/geograph/dev/reference/isInArea.md)
to select nodes within a geographic area.

Other basic_methods:
[`subset-gData`](https://evolecolgroup.github.io/geograph/dev/reference/subset-gData.md)

## Examples

``` r
## subset to nodes in a geographic area
plot(worldgraph.10k, reset = TRUE)
#> Spherical geometry (s2) switched off

#> Spherical geometry (s2) switched on
geo.zoomin(list(x = c(-6, 38), y = c(35, 73)))
#> Spherical geometry (s2) switched off

#> Spherical geometry (s2) switched on
x <- worldgraph.10k[isInArea(worldgraph.10k, quiet = TRUE)]

## subset by node name
x <- worldgraph.10k[c("1", "2", "3")]
```
