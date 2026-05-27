# Make a new gGraph object from a custom square grid

Deprecated. Use
[`makeSquareGrid`](https://evolecolgroup.github.io/geograph/dev/reference/makeSquareGrid.md)
instead.

## Usage

``` r
makeGrid(size = NULL, n.lon = NULL, n.lat = NULL,
  lon.range = NULL, lat.range = NULL)
```

## Arguments

- size:

  an integer giving the approximate number of nodes.

- n.lon:

  the number of longitude coordinates.

- n.lat:

  the number of latitude coordinates.

- lon.range, lat.range:

  vectors of length two giving the range.

## Value

A
[`gGraph`](https://evolecolgroup.github.io/geograph/dev/reference/gGraph-class.md)
object. See
[`makeSquareGrid`](https://evolecolgroup.github.io/geograph/dev/reference/makeSquareGrid.md)
for details.

## See also

[`geoGraph-deprecated`](https://evolecolgroup.github.io/geograph/dev/reference/geoGraph-deprecated.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# by defining the range covered by the grid
squareGraph <- makeGrid(
  size      = 10000,
  lon.range = c(-12, 2),
  lat.range = c(49, 61)
)
squareGraph <- findLand(squareGraph)
squareGraph <- setColors(
  squareGraph,
  col.rules = data.frame(
    habitat = c("sea", "land"),
    color = c("blue", "green")
  )
)
plot(squareGraph, reset = TRUE)

# If no area is specified, currently plotted area is used
geo.zoomin(c(8, 13, 54, 58))
newGraph <- makeGrid(1e3)
newGraph <- findLand(newGraph)
newGraph <- setColors(
  newGraph,
  col.rules = data.frame(
    habitat = c("sea", "land"),
    color = c("blue", "green")
  )
)

## plot the new gGraph
plot(newGraph, reset = TRUE, edge = TRUE)
} # }
```
