# geoGraph (development version)

## Behaviour changes

* `dijkstraBetween(x, from, to)` on a `gGraph` now returns every path from each
  node of `from` to each node of `to`. With `to` missing or identical to `from`, all pairs of
  `from` are computed, as before. 

* `gPath` objects record their `type`; `gPath2dist()` uses it and returns a
  `from` x `to` matrix for the output of `dijkstraBetween()` with distinct
  `from` and `to` (and so for `polygonBetween()`). `dijkstraBetween()` on a
  `gData` with two locations now returns a `dist`, as for more locations.

* `dijkstraBuffer()` includes the origin node, works on graphs that are not
  fully connected, and is much faster.

* `isInArea()` errors on an invalid `reg` (it returned `NA`), and accepts
  `reg = c(xmin, xmax, ymin, ymax)`.

* `findLand()` and `assignByPolygon()` replace existing node attributes of the
  same name instead of adding duplicated columns; `findLand()` always returns
  a factor with levels `land` and `sea`.

* `dijkstraFrom()` requires a single `start` node.

## Bug fixes

* `new("gData", nodes.id = ...)` no longer ignores `nodes.id`;
  `new("gGraph", meta = ...)` no longer ignores `meta`; `new("gGraph")` accepts
  coordinates without column names.

* `gGraph` objects without node attributes (e.g. from `makeSquareGrid()` or
  `makeHexGrid()`) can be subset, and work with `getNodesAttr()`,
  `setNodesAttr()`, `buffer()`, `geom_ggraph()`.

* `setCosts()`, `getNodeCosts()` and `getColors()` no longer chain rules when
  a cost or color equals another attribute value.

* `assignByPolygon()` and `findLand()` handle layers in a projected CRS, and
  `assignByPolygon()` works on `gData` objects with several locations on the
  same node.

* `closestNode()` accepts a single location as `c(lon, lat)`.

* `polygonBetween()` detects polygon outlines correctly on subset graphs. It
  only accepts `gGraph` objects.

* `areConnected()` no longer reports an isolated node as connected in graphs
  without edges.

* `[` and `geom_gdata()` work on `gData` objects whose data is a vector.

* `assignByRaster()` removes missing values directly on `na.rm` argument.

* `dijkstraFrom()` on a `gData` gives an informative error when `start` is not
  connected to the locations.

* `setDistCosts()` is much faster; its documentation now states that costs
  are in miles.

# geoGraph v2.0.1

* Added ggplot2 support for gGraph, gData, and gPath objects via autoplot() methods and geom_ggraph(), geom_gdata(), and geom_gpath() layers.

* `closestNode` now uses a faster KNN implementation for nearest neighbor searches by default. The legacy zone-expansion method is still available via method = "inArea".

* Functions that temporarily toggle `sf` settings now restore the prior state on exit.

* Updated `gPath2dist` to now automatically return a vector or dist object based on the input type

* Updated `makeHexGrid` to handle date line crossing `gGraph` objects

* Updated `isInArea` to print a reproducible bounding box message 

* Added `assignByRaster` to assign raster cell values to graph nodes with customizable aggregation.

* Added `dijkstraBuffer` to identify nodes within a specified shortest-path distance from an origin.

* Added `makeHexGrid` for explicit hexagonal grid construction.

* Deprecated `extractFromLayer` in favor of `assignByPolygon` and `makeGrid` in favor of `makeSquareGrid`.

* Added documentation for the `gPath` class and basic subsetting methods for `gGraph` and `gData` objects

* Added `setGraph` and `setColors` as well as updated functionality in `getColors`, `getCosts`, and `setCosts` 

* Removed `getHistory` method for gGraph objects and upgraded roxygen2 documentation from version 7.3.3 to 8.0.0

* Added comprehensive test coverage for cost functions and updated documentation 

* Updated `plot.gPath()` and documentation for all dijkstra methods

* Updated `connectivityPlot()` ensuring the color assignment matches the connected set

* Fix incorrect handling of duplicate nodes in `dijkstraBetween()`, 
  ensuring zero-distance paths are properly represented.

* remove all use of deprecated packages (`sp` and `maptools`), and move to
  `sf` objects

# geoGraph v1.1

* Update documentation to roxygen.
* Make vignette fully live.


# geoGraph v1.0

* First public release with full functionality.
