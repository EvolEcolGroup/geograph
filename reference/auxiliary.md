# Auxiliary methods for geoGraph

These methods are low-level functions called by other procedures of
`geoGraph`. Some can, however, be useful in themselves. Note that unlike
other functions in `geoGraph`, these functions do not generally test for
the validity of the provided arguments (for speed purposes).  

## Usage

``` r
hasCosts(x)

geo.segments(
  x0,
  y0,
  x1,
  y1,
  col = graphics::par("fg"),
  lty = graphics::par("lty"),
  lwd = graphics::par("lwd"),
  ...
)
```

## Arguments

- x:

  a valid
  [gGraph](https://evolecolgroup.github.io/geograph/reference/gGraph-class.md).

- x0, y0:

  coordinates of points *from* which to draw.

- x1, y1:

  coordinates of points *to* which to draw.

- col:

  a character string or an integer indicating the color of the segments.

- lty:

  a character string or an integer indicating the type of line.

- lwd:

  an integer indicating the line width.

- ...:

  further graphical parameters (from 'par') passed to the `segments`
  function.

## Value

For `hasCost`, a logical value is returned. `geo.segments` returns NULL.

## Details

- `hasCosts`: tests whether a
  [gGraph](https://evolecolgroup.github.io/geograph/reference/gGraph-class.md)
  has costs associated to its edges.  

- `geo.segments`: a substitute to `segments` which correctly draws
  segments between locations distant by more than 90 degrees of
  longitude.  

- `rebuild`: in development.

## Examples

``` r

hasCosts(worldgraph.10k)
#> [1] FALSE
```
