# Assign raster values to graph nodes

The function `assignByRaster` takes a
[`gGraph`](https://evolecolgroup.github.io/geograph/dev/reference/gGraph-class.md)
object and a `SpatRaster`, assigns each raster cell to the nearest graph
node, and collapses the values to a single scalar per node using a
summary function.

## Usage

``` r
assignByRaster(
  graph,
  raster,
  layer.name = "raster_points",
  fun = "mean",
  na.rm = TRUE,
  ...
)
```

## Arguments

- graph:

  a
  [`gGraph`](https://evolecolgroup.github.io/geograph/dev/reference/gGraph-class.md)
  object.

- raster:

  a `SpatRaster` object (from the `terra` package).

- layer.name:

  a character string giving the name of the new node attribute. Defaults
  to `"raster_points"`.

- fun:

  a function or character string specifying how to summarize raster
  values within each node. Built-in options are `"mean"`, `"max"`,
  `"min"`, `"median"`, `"sd"`, `"any"`, and `"all"`. Alternatively, pass
  any function that takes a numeric vector and returns a scalar.
  Defaults to `"mean"`.

- na.rm:

  logical. Whether to remove `NA` values before summarizing. Defaults to
  `TRUE`.

- ...:

  additional arguments passed to `fun`.

## Value

A
[`gGraph`](https://evolecolgroup.github.io/geograph/dev/reference/gGraph-class.md)
object with a new scalar node attribute named `layer.name`.

## Details

This function is memory-intensive for high-resolution rasters or large
graphs. Once values are assigned, further node attributes can be derived
using
[`setNodesAttr`](https://evolecolgroup.github.io/geograph/dev/reference/setNodesAttr.md).

## See also

[`setNodesAttr`](https://evolecolgroup.github.io/geograph/dev/reference/setNodesAttr.md)
to set node attributes manually.
[`assignByPolygon`](https://evolecolgroup.github.io/geograph/dev/reference/assignByPolygon.md)
to assign attributes from GIS shapefiles.

## Examples

``` r

## Make a new gGraph without any nodes attribute
geo.box <- c(xmin = -10, xmax = 30, ymin = 35, ymax = 60)
ggraph <- makeHexGrid(geo.box, spacing = 1000)
#> Resolution: 4, Area (km^2): 629710.644103813, Spacing (km): 783.739159045648, CLS (km): 895.60184164835

## Create a synthetic raster of random elevation values over the same region.
set.seed(42)
r <- terra::rast(
  xmin = geo.box["xmin"], xmax = geo.box["xmax"],
  ymin = geo.box["ymin"], ymax = geo.box["ymax"],
  resolution = 5, crs = "EPSG:4326"
)
terra::values(r) <- runif(terra::ncell(r))

## assign mean raster value per node
ggraph <- assignByRaster(ggraph, r, layer.name = "elevation", fun = "mean")
#> although coordinates are longitude/latitude, st_nearest_feature assumes that
#> they are planar

## assign standard deviation per node (useful for ruggedness)
ggraph <- assignByRaster(ggraph, r, layer.name = "ruggedness", fun = "sd")
#> although coordinates are longitude/latitude, st_nearest_feature assumes that
#> they are planar
```
