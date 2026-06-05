# Build a regular grid gGraph

The function `makeGrid` builds a
[gGraph](https://evolecolgroup.github.io/geograph/reference/gGraph-class.md)
using a regular grid for a given area. If no area is specified,
currently plotted area is used. Note that such grid is only valid for
small scales, for cases in which curvature of the surface of the earth
can be neglected.

## Usage

``` r
makeGrid(
  size = NULL,
  n.lon = NULL,
  n.lat = NULL,
  lon.range = NULL,
  lat.range = NULL
)
```

## Arguments

- size:

  an integer giving the approximate number of nodes of the grid. The
  function will attempt to make a square grid of (approximately) this
  size.

- n.lon:

  the number of longitude coordinates of the grid (i.e., width of the
  grid, in number of cells)

- n.lat:

  the number of latitude coordinates of the grid (i.e., height of the
  grid, in number of cells)

- lon.range, lat.range:

  vectors of length two giving the range covered by the grid, in
  longitude and latitude, respectively.

## Value

A
[gGraph](https://evolecolgroup.github.io/geograph/reference/gGraph-class.md)
object.

## Examples

``` r

## zoom in to a smaller area
plot(worldgraph.10k)

geo.zoomin(c(-10, 0, 50, 54))



## make a new gGraph
newGraph <- makeGrid(1e3)
newGraph <- findLand(newGraph)
newGraph@meta$colors <- data.frame(
  habitat = c("sea", "land"),
  color = c("blue", "green")
)


## plot the new gGraph
plot(newGraph, reset = TRUE, edge = TRUE)

```
