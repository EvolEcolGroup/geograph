# Collapse a list-based node attribute into a scalar node attribute

This function collapses a list-based node attribute (e.g. raster values
assigned to each node) into a single scalar value per node. Optionally
replaces the original attribute to reduce memory usage.

## Usage

``` r
collapseNodeAttribute(
  graph,
  attribute,
  fun = c("mean", "max", "min", "median", "sd", "any", "all"),
  na.rm = TRUE,
  replace = TRUE,
  ...
)
```

## Arguments

- graph:

  A
  [`gGraph`](https://evolecolgroup.github.io/geograph/dev/reference/gGraph-class.md)
  object.

- attribute:

  Character string giving the name of the node attribute to collapse.

- fun:

  A function or a character string specifying how to collapse values.
  Built-in options include `min`, `max`, `mean`, `median`, `sd`, `any`,
  and `all`.

- na.rm:

  Logical; whether to remove `NA` values before collapsing. Defaults to
  `TRUE`.

- replace:

  Logical; if `TRUE` (default), replaces the original list-based node
  attribute with the collapsed vector. If `FALSE`, the collapsed vector
  is returned but the graph is left unchanged.

- ...:

  Additional arguments passed to `fun`.

## Value

If `replace = TRUE`, a modified
[`gGraph`](https://evolecolgroup.github.io/geograph/dev/reference/gGraph-class.md)
object with the collapsed attribute replacing the original one.

If `replace = FALSE`, a vector of collapsed values (one per node).

## Examples

``` r
if (requireNamespace("terra", quietly = TRUE)) {
# create a small graph over Europe
geo.box <- c(xmin = -10, xmax = 30, ymin = 35, ymax = 60)
ggraph <- createNewGraph(geo.box, spacing = 1000)
# create a matching synthetic raster and assign it to the graph
r <- terra::rast(
  xmin = geo.box["xmin"], xmax = geo.box["xmax"],
  ymin = geo.box["ymin"], ymax = geo.box["ymax"],
  resolution = 5,
  crs = "EPSG:4326"
)
terra::values(r) <- runif(terra::ncell(r))
ggraph <- assignRasterPoints(ggraph, r, layer.name = "elevation")
# collapse the elevation attribute to the mean value per node (replaces the list)
ggraph <- collapseNodeAttribute(ggraph, attribute = "elevation", fun = "mean")
}
#> Resolution: 4, Area (km^2): 629710.644103813, Spacing (km): 783.739159045648, CLS (km): 895.60184164835
#> although coordinates are longitude/latitude, st_nearest_feature assumes that
#> they are planar
```
