# Plot segments correctly when crossing the antimeridian

A substitute to `segments` which correctly draws segments between
locations distant by more than 90 degrees of longitude (i.e. from one
hemisphere to the other). It is used instead of segments, but it is
slower.

## Usage

``` r
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

NULL.

## Details

This low-level function is designed to be called by other procedures of
[geoGraph](https://evolecolgroup.github.io/geograph/dev/reference/geoGraph-package.md).
However, it can sometimes be useful by itself. Note that unlike other
functions in `geoGraph`, this functions does not test for the validity
of the provided arguments (for speed purposes).
