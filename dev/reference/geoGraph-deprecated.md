# Deprecated functions in package geoGraph

Functions listed below are deprecated and will be removed in a future
version. Where possible, alternatives are mentioned. Help pages for
deprecated functions are available at `help("<function>-deprecated")`.

## Usage

``` r
makeGrid(
  size = NULL,
  n.lon = NULL,
  n.lat = NULL,
  lon.range = NULL,
  lat.range = NULL
)

extractFromLayer(x, ...)
```

## Value

NULL.

## `makeGrid`

For `makeGrid`, use
[`makeSquareGrid`](https://evolecolgroup.github.io/geograph/dev/reference/makeSquareGrid.md).

## `extractFromLayer`

For `extractFromLayer`, use
[`assignByPolygon`](https://evolecolgroup.github.io/geograph/dev/reference/assignByPolygon.md).

## Examples

``` r
if (FALSE) { # \dontrun{
# these functions are deprecated — use the replacements instead:
# makeGrid()       -> makeSquareGrid()
# extractFromLayer() -> assignByPolygon()
} # }
```
