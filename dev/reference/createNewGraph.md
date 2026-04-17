# Make a new gGraph object from a custom discrete global grid

This function constructs a new
[`gGraph`](https://evolecolgroup.github.io/geograph/dev/reference/gGraph-class.md)
object based on a discrete global grid system (DGGS) with a user-defined
spatial resolution. The graph is restricted to a geographic bounding box
defined by the user.

## Usage

``` r
createNewGraph(geo_box, spacing, ...)
```

## Arguments

- geo_box:

  A geographic bounding box. With either a named numeric vector with
  `xmin`, `xmax`, `ymin`, `ymax` or an object of class `bbox` or `sf`.
  Coordinates must be in longitude/latitude (EPSG:4326).

- spacing:

  A positive numeric value giving the desired spacing (in km) of the
  discrete global grid. The closest available DGGS resolution is used.

- ...:

  Additional arguments passed to other methods (currently not used).

## Value

A
[`gGraph`](https://evolecolgroup.github.io/geograph/dev/reference/gGraph-class.md)
object representing the DGGS restricted to the specified geographic
region. The `@nodes.attr` slot is empty.

## Details

The function constructs a discrete global grid using the `dggridR`
package, then identifies neighboring grid cells using the `spdep`
package, and finally builds a
[`gGraph`](https://evolecolgroup.github.io/geograph/dev/reference/gGraph-class.md)
object with the resulting graph structure and node coordinates.

## Examples

``` r
# Define a geographic bounding box (e.g., for a region in Europe)
geo_box <- c(xmin = -10, xmax = 30, ymin = 35, ymax = 60)
# Create a gGraph with a spacing of 1000 km
ggraph <- createNewGraph(geo_box = geo_box, spacing = 1000)
#> Resolution: 4, Area (km^2): 629710.644103813, Spacing (km): 783.739159045648, CLS (km): 895.60184164835
plot(ggraph, edge = TRUE)
```
