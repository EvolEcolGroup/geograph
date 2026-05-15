# Assign raster points to graph nodes

This function takes a
[`gGraph`](https://evolecolgroup.github.io/geograph/dev/reference/gGraph-class.md)
object and a raster `SpatRaster`, and assigns each raster cell to the
nearest graph node. The points are stored in the graph as a node
attribute called `raster_points`, which contains all raster values for
that node. Users can then summarize these points to create new
attributes.

## Usage

``` r
assignRasterPoints(graph, raster, layer.name = "raster_points")
```

## Arguments

- graph:

  A
  [`gGraph`](https://evolecolgroup.github.io/geograph/dev/reference/gGraph-class.md)
  object.

- raster:

  A `SpatRaster` object (from `terra`)

- layer.name:

  Character, optional. If provided, stores the raster points in
  `graph@nodes.attr[[layer.name]]` instead of `raster_points.`

## Value

A
[`gGraph`](https://evolecolgroup.github.io/geograph/dev/reference/gGraph-class.md)
object with a new node attribute containing the raster points assigned
to each node.

## Details

This function is memory-intensive if the raster is high-resolution
and/or the graph has many nodes. Users should be mindful of input sizes.
Once points are assigned, the user can compute summaries (mean, median,
SD, thresholding) to create custom attributes like "mountain" or "land".

## Examples

``` r
if (requireNamespace("terra", quietly = TRUE)) {
# create a small graph over Europe
geo.box <- c(xmin = -10, xmax = 30, ymin = 35, ymax = 60)
ggraph <- createNewGraph(geo.box, spacing = 1000)

# create a matching synthetic raster
r <- terra::rast(
  xmin = geo.box["xmin"], xmax = geo.box["xmax"],
  ymin = geo.box["ymin"], ymax = geo.box["ymax"],
  resolution = 5,
  crs = "EPSG:4326"
)
terra::values(r) <- runif(terra::ncell(r))

# assign raster points to the nearest node
ggraph <- assignRasterPoints(ggraph, r)

# use a custom layer name
ggraph <- assignRasterPoints(ggraph, r, layer.name = "elevation")
}
#> Resolution: 4, Area (km^2): 629710.644103813, Spacing (km): 783.739159045648, CLS (km): 895.60184164835
#> Spherical geometry (s2) switched off
#> although coordinates are longitude/latitude, st_nearest_feature assumes that
#> they are planar
#> although coordinates are longitude/latitude, st_nearest_feature assumes that
#> they are planar
```
