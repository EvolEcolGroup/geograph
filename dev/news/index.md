# Changelog

## geoGraph (development version)

- Added documentation for the `gPath` class and basic subsetting methods
  for `gGraph` and `gData` objects

- Added `setGraph` and `setColors` as well as updated functionality in
  `getColors`, `getCosts`, and `setCosts`

- Removed `getHistory` method for gGraph objects and upgraded roxygen2
  documentation from version 7.3.3 to 8.0.0

- Added comprehensive test coverage for cost functions and updated
  documentation

- Updated
  [`plot.gPath()`](https://evolecolgroup.github.io/geograph/dev/reference/plot.gPath.md)
  and documentation for all dijkstra methods

- Updated
  [`connectivityPlot()`](https://evolecolgroup.github.io/geograph/dev/reference/connectivityPlot.md)
  ensuring the color assignment matches the connected set

- Fix incorrect handling of duplicate nodes in
  [`dijkstraBetween()`](https://evolecolgroup.github.io/geograph/dev/reference/dijkstraBetween.md),
  ensuring zero-distance paths are properly represented.

- remove all use of deprecated packages (`sp` and `maptools`), and move
  to `sf` objects

## geoGraph v1.1

- Update documentation to roxygen.
- Make vignette fully live.

## geoGraph v1.0

- First public release with full functionality.
