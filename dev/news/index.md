# Changelog

## geoGraph (development version)

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
