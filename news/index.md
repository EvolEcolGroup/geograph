# Changelog

## geoGraph v2.1.0

- `dijkstraBetween(x, from, to)` on a `gGraph` now returns every path
  from each node of `from` to each node of `to`. With `to` missing or
  identical to `from`, all pairs of `from` are computed, as before.
  [`polygonBetween()`](https://evolecolgroup.github.io/geograph/reference/polygonBetween.md)
  benefits from this.

- `gPath` objects record their `type`;
  [`gPath2dist()`](https://evolecolgroup.github.io/geograph/reference/gPath2dist.md)
  uses it and returns a `from` x `to` matrix for the output of
  [`dijkstraBetween()`](https://evolecolgroup.github.io/geograph/reference/dijkstraBetween.md)
  with distinct `from` and `to` (and so for
  [`polygonBetween()`](https://evolecolgroup.github.io/geograph/reference/polygonBetween.md)).
  [`dijkstraBetween()`](https://evolecolgroup.github.io/geograph/reference/dijkstraBetween.md)
  on a `gData` with two locations now returns a `dist`, as for more
  locations.

- [`dijkstraBuffer()`](https://evolecolgroup.github.io/geograph/reference/dijkstraBuffer.md)
  includes the origin node and works on graphs that are not fully
  connected.

- [`isInArea()`](https://evolecolgroup.github.io/geograph/reference/isInArea.md)
  errors on an invalid `reg` (it returned `NA`), and accepts
  `reg = c(xmin, xmax, ymin, ymax)`.

- [`findLand()`](https://evolecolgroup.github.io/geograph/reference/findLand.md),
  [`assignByPolygon()`](https://evolecolgroup.github.io/geograph/reference/assignByPolygon.md)
  and
  [`buffer()`](https://evolecolgroup.github.io/geograph/reference/buffer.md)
  replace existing node attributes of the same name instead of adding
  duplicated columns;
  [`findLand()`](https://evolecolgroup.github.io/geograph/reference/findLand.md)
  always returns a factor with levels `land` and `sea`.

- [`assignByRaster()`](https://evolecolgroup.github.io/geograph/reference/assignByRaster.md)
  removes `NA`s itself (when `na.rm = TRUE`) before applying `fun`, so
  any summary function works, including `length` and functions without
  an `na.rm` argument. `na.rm` is no longer passed to `fun`.

- [`polygonBetween()`](https://evolecolgroup.github.io/geograph/reference/polygonBetween.md)
  only accepts `gGraph` objects (for gData use
  [`dijkstraBetween()`](https://evolecolgroup.github.io/geograph/reference/dijkstraBetween.md)
  on the linked `gGraph` instead).

- [`makeHexGrid()`](https://evolecolgroup.github.io/geograph/reference/makeHexGrid.md)
  is much faster on large grids , neighbors are found with a
  nearest-neighbor search instead of `spdep`.

- [`setDistCosts()`](https://evolecolgroup.github.io/geograph/reference/setDistCosts.md)
  is about 8x faster; its documentation now states that costs are in
  miles.

- [`dijkstraBuffer()`](https://evolecolgroup.github.io/geograph/reference/dijkstraBuffer.md)
  is much faster: it no longer builds every path.

- `new("gData", nodes.id = ...)` no longer ignores `nodes.id`;
  `new("gGraph", meta = ...)` no longer ignores `meta`; `new("gGraph")`
  accepts coordinates without column names.

- `gGraph` objects without node attributes (e.g. from
  [`makeSquareGrid()`](https://evolecolgroup.github.io/geograph/reference/makeSquareGrid.md)
  or
  [`makeHexGrid()`](https://evolecolgroup.github.io/geograph/reference/makeHexGrid.md),
  or saved with older versions) can be subset, and work with
  [`getNodesAttr()`](https://evolecolgroup.github.io/geograph/reference/getNodesAttr.md),
  [`setNodesAttr()`](https://evolecolgroup.github.io/geograph/reference/setNodesAttr.md),
  [`buffer()`](https://evolecolgroup.github.io/geograph/reference/buffer.md),
  [`dijkstraBuffer()`](https://evolecolgroup.github.io/geograph/reference/dijkstraBuffer.md),
  [`assignByRaster()`](https://evolecolgroup.github.io/geograph/reference/assignByRaster.md)
  and
  [`geom_ggraph()`](https://evolecolgroup.github.io/geograph/reference/geom_ggraph.md).

- [`setCosts()`](https://evolecolgroup.github.io/geograph/reference/setCosts.md),
  [`getNodeCosts()`](https://evolecolgroup.github.io/geograph/reference/getCosts.md)
  and
  [`getColors()`](https://evolecolgroup.github.io/geograph/reference/getColors.md)
  no longer chain rules when a cost or color equals another attribute
  value.

- [`assignByPolygon()`](https://evolecolgroup.github.io/geograph/reference/assignByPolygon.md)
  and
  [`findLand()`](https://evolecolgroup.github.io/geograph/reference/findLand.md)
  handle layers in a projected CRS, and
  [`assignByPolygon()`](https://evolecolgroup.github.io/geograph/reference/assignByPolygon.md)
  works on `gData` objects with several locations on the same node.

- [`closestNode()`](https://evolecolgroup.github.io/geograph/reference/closestNode.md)
  accepts a single location as `c(lon, lat)`.

- [`polygonBetween()`](https://evolecolgroup.github.io/geograph/reference/polygonBetween.md)
  detects polygon outlines correctly on subset graphs.

- [`areConnected()`](https://evolecolgroup.github.io/geograph/reference/areConnected.md)
  no longer reports an isolated node as connected in graphs without
  edges.

- [`combineCosts()`](https://evolecolgroup.github.io/geograph/reference/combineCosts.md)
  compares the edges of the two graphs reliably.

- [`dropDeadEdges()`](https://evolecolgroup.github.io/geograph/reference/dropDeadEdges.md)
  also drops edges when all costs are equal.

- [`dijkstraFrom()`](https://evolecolgroup.github.io/geograph/reference/dijkstraFrom.md)
  on a `gData` gives an informative error when `start` is not connected
  to the locations.

- Plotting:
  [`plotEdges()`](https://evolecolgroup.github.io/geograph/reference/plot-gGraph.md)
  works with a single visible edge;
  [`points()`](https://rdrr.io/r/graphics/points.html) keeps per-node
  colors on the right nodes when zoomed in; `plot(x, shape = NULL)`
  draws the nodes of the new area;
  [`geo.segments()`](https://evolecolgroup.github.io/geograph/reference/geo.segments.md)
  keeps per-segment colors with their segments.

- `[` and
  [`geom_gdata()`](https://evolecolgroup.github.io/geograph/reference/geom_gdata.md)
  work on `gData` objects whose data is a vector.

- Validity checks give informative messages.

## geoGraph v2.0.1

- Added ggplot2 support for gGraph, gData, and gPath objects via
  autoplot() methods and geom_ggraph(), geom_gdata(), and geom_gpath()
  layers.

- `closestNode` now uses a faster KNN implementation for nearest
  neighbor searches by default. The legacy zone-expansion method is
  still available via method = “inArea”.

- Functions that temporarily toggle `sf` settings now restore the prior
  state on exit.

- Updated `gPath2dist` to now automatically return a vector or dist
  object based on the input type

- Updated `makeHexGrid` to handle date line crossing `gGraph` objects

- Updated `isInArea` to print a reproducible bounding box message

- Added `assignByRaster` to assign raster cell values to graph nodes
  with customizable aggregation.

- Added `dijkstraBuffer` to identify nodes within a specified
  shortest-path distance from an origin.

- Added `makeHexGrid` for explicit hexagonal grid construction.

- Deprecated `extractFromLayer` in favor of `assignByPolygon` and
  `makeGrid` in favor of `makeSquareGrid`.

- Added documentation for the `gPath` class and basic subsetting methods
  for `gGraph` and `gData` objects

- Added `setGraph` and `setColors` as well as updated functionality in
  `getColors`, `getCosts`, and `setCosts`

- Removed `getHistory` method for gGraph objects and upgraded roxygen2
  documentation from version 7.3.3 to 8.0.0

- Added comprehensive test coverage for cost functions and updated
  documentation

- Updated
  [`plot.gPath()`](https://evolecolgroup.github.io/geograph/reference/plot.gPath.md)
  and documentation for all dijkstra methods

- Updated
  [`connectivityPlot()`](https://evolecolgroup.github.io/geograph/reference/connectivityPlot.md)
  ensuring the color assignment matches the connected set

- Fix incorrect handling of duplicate nodes in
  [`dijkstraBetween()`](https://evolecolgroup.github.io/geograph/reference/dijkstraBetween.md),
  ensuring zero-distance paths are properly represented.

- remove all use of deprecated packages (`sp` and `maptools`), and move
  to `sf` objects

## geoGraph v1.1

- Update documentation to roxygen.
- Make vignette fully live.

## geoGraph v1.0

- First public release with full functionality.
