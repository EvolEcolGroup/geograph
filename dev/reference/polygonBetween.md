# Least-cost paths between two polygons

This function computes least-cost path distances between all nodes
belonging to two polygons in a
[`gData`](https://evolecolgroup.github.io/geograph/dev/reference/gData-class.md)
or
[`gGraph`](https://evolecolgroup.github.io/geograph/dev/reference/gGraph-class.md)
using `dijkstraBetween.` The polygons are specified via a node attribute
layer. Optionally, only outline nodes (nodes with at least one neighbor
outside the polygon) are used to reduce computation time.

## Usage

``` r
polygonBetween(g, layer, from, to, outline = TRUE)
```

## Arguments

- g:

  [`gData`](https://evolecolgroup.github.io/geograph/dev/reference/gData-class.md)
  or
  [`gGraph`](https://evolecolgroup.github.io/geograph/dev/reference/gGraph-class.md)
  object with edge costs already defined.

- layer:

  Character. Name of the node attribute layer containing polygon
  membership.

- from:

  Character. Name of the first polygon.

- to:

  Character. Name of the second polygon.

- outline:

  Logical. If `TRUE`, only outline nodes of each polygon (nodes with at
  least one neighbor outside the polygon) are used. Defaults to `TRUE`.

## Value

A numeric vector containing the least-cost distances between nodes of
the two polygons.

## Details

The function extracts all nodes belonging to the two specified polygons
and computes least-cost paths between them using `dijkstraBetween.` If
`outline = TRUE`, the computation is restricted to outline nodes of each
polygon, which can substantially reduce runtime for large polygons.

## Examples

``` r
world.countries <- rnaturalearth::ne_countries(
  scale = "medium",
  returnclass = "sf"
)
newGraph <- extractFromLayer(rawgraph.10k,
  layer = world.countries,
  attr = c("continent", "name")
)
#> although coordinates are longitude/latitude, st_intersects assumes that they
#> are planar
test <- polygonBetween(newGraph, layer = "name", "Spain", "Germany", outline = TRUE)
plot(newGraph, col = NA, reset = TRUE)
plot(test, col = "red")

```
