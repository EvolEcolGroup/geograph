# Changelog

## geoGraph (development version)

## geoGraph v2.0

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
