# geoGraph (development version)

* `dijkstraFrom()`, `dijkstraBetween()` and `polygonBetween()` no longer use
  `RBGL::sp.between()`, which spent most of its time looking up node names
  and growing vectors for every path (and looped over the whole graph for an
  unreachable node). Paths from the same start are now traced back together:
  `dijkstraBetween(hgdp)` takes 1.7 s instead of 3.7 s, and the 10.5k paths
  from one node to the rest of the largest connected set of `worldgraph.40k`
  2 s instead of 7.6 s. The paths, lengths and their order are unchanged.
  `dijkstraFrom()` on a `gData` also computes the connected sets of the graph
  once instead of twice.

* `geom_ggraph(edges = TRUE)` builds its edge segments faster (one matrix per
  edge, without the checks of `sf::st_linestring()`, and edges selected by
  node position): `ggplot_build()` of the edges of `rawgraph.40k` takes 2.5 s
  instead of 3.9 s. The layer data are unchanged.

* `plotEdges()` and `plot(x, edges = TRUE)` select the visible edges and their
  costs from one edge table instead of matching node names and splitting
  edge names: `plotEdges(rawgraph.40k)` takes 0.65 s instead of 1.7 s. The
  segments, their order and widths are unchanged. `plot()` computes the
  visible nodes and their colors once when a shape is drawn.

* `hasCosts()` reads the stored weights directly when every edge has one
  (0.02 s instead of 0.2 s on `worldgraph.40k`).

* `areNeighbours()` looks up pairs of nodes with numeric keys (0.02 s instead
  of 0.4 s for 5,000 pairs), and `setEdges()` adds and removes edges by node
  position instead of name: 0.08 s instead of 2 s to remove 5,000 edges of
  `worldgraph.40k`, 0.17 s instead of 1.7 s to add 5,000. Results are
  unchanged.

* `assignByRaster()` finds the nearest node of each cell with a k-d tree
  (`FNN`) instead of `sf::st_nearest_feature()`, using the same planar
  distances on longitude and latitude, and groups values per node without
  data frames: 0.1 s instead of 1.1 s for 175k cells, 2.3 s instead of 20 s
  for a 2.3M-cell world raster. Results are unchanged, except for cells
  whose centre is exactly equidistant from several nodes (e.g. a raster
  aligned halfway between the nodes of a square grid), which can go to a
  different one of these nodes: the choice is arbitrary in both versions.
  `tidyr` and `tibble` are no longer needed.

* `getCosts(res.type = "asIs", unique = TRUE)` no longer looks up each node by
  name: 0.2 s instead of 3.1 s on `worldgraph.40k`, same result.

* `makeHexGrid()` and `makeSquareGrid()` build their graph from a flat edge
  list, and `new("gGraph")` no longer renames the nodes of a graph whose node
  names are already right: `makeSquareGrid(size = 90000)` takes 1.2 s instead
  of 4 s, `makeHexGrid()` over Europe at `spacing = 15` 19 s instead of 23 s.
  The graphs are unchanged (with stored weights of 1).

* `makeSquareGrid()` works for grids of 100,000 nodes or more (it failed with
  "node names in 'ft' must be in 'V'", as `as.character(1e5)` is `"1e+05"`).

* `setCosts()`, `dropDeadEdges()`, `combineCosts()` and `setDistCosts()` compute
  and store edge weights in one pass over the edges instead of node by node,
  and no longer write every weight twice: 2-2.5 times faster (e.g.
  `setCosts()` on `rawgraph.40k` 0.7 s instead of 1.8 s). The weights are
  unchanged, including the quirk of `setCosts(method = "function")` with an
  asymmetric `FUN`, where the edge from `i` to `j` stores `FUN(cost_j, cost_i)`.
  `FUN` is still called once per node. `setCosts()` and `combineCosts()` now
  give an informative error when `FUN` does not return one value per edge.

* Subsetting a `gGraph` with `[` (and so `isInArea()`-based subsets,
  `dropDeadNodes()` and `keepMaxConnectedSet()`) no longer uses
  `graph::subGraph()`, whose cost grows with the number of kept times dropped
  nodes: keeping every other node of `worldgraph.40k` takes 0.3 s instead of
  3.3 s (7 s instead of 0.13 s without costs). The result is unchanged.

* `keepMaxConnectedSet()` subsets the graph to the largest connected set
  directly instead of rebuilding it node by node: 0.4 s instead of 8 s on
  `worldgraph.40k`. `dropDeadNodes()` finds nodes with edges on node positions
  (5 s to 0.3 s, mostly from the faster subsetting). Results are unchanged.

# geoGraph v2.1.0

* `dijkstraBetween(x, from, to)` on a `gGraph` now returns every path from each
  node of `from` to each node of `to`. With `to` missing or identical to `from`, all pairs of
  `from` are computed, as before. `polygonBetween()` benefits from this.

* `gPath` objects record their `type`; `gPath2dist()` uses it and returns a
  `from` x `to` matrix for the output of `dijkstraBetween()` with distinct
  `from` and `to` (and so for `polygonBetween()`). `dijkstraBetween()` on a
  `gData` with two locations now returns a `dist`, as for more locations.

* `dijkstraBuffer()` includes the origin node and works on graphs that are not
  fully connected.

* `isInArea()` errors on an invalid `reg` (it returned `NA`), and accepts
  `reg = c(xmin, xmax, ymin, ymax)`.

* `findLand()`, `assignByPolygon()` and `buffer()` replace existing node
  attributes of the same name instead of adding duplicated columns;
  `findLand()` always returns a factor with levels `land` and `sea`.

* `assignByRaster()` removes `NA`s itself (when `na.rm = TRUE`) before applying
  `fun`, so any summary function works, including `length` and functions
  without an `na.rm` argument. `na.rm` is no longer passed to `fun`.

* `polygonBetween()` only accepts `gGraph` objects (for gData 
  use `dijkstraBetween()` on the linked `gGraph` instead).

* `makeHexGrid()` is much faster on large grids , neighbors are found with 
  a nearest-neighbor search instead of `spdep`.

* `setDistCosts()` is about 8x faster; its documentation now states that costs
  are in miles.

* `dijkstraBuffer()` is much faster: it no longer builds every path.

* `new("gData", nodes.id = ...)` no longer ignores `nodes.id`;
  `new("gGraph", meta = ...)` no longer ignores `meta`; `new("gGraph")` accepts
  coordinates without column names.

* `gGraph` objects without node attributes (e.g. from `makeSquareGrid()` or
  `makeHexGrid()`, or saved with older versions) can be subset, and work with
  `getNodesAttr()`, `setNodesAttr()`, `buffer()`, `dijkstraBuffer()`,
  `assignByRaster()` and `geom_ggraph()`.

* `setCosts()`, `getNodeCosts()` and `getColors()` no longer chain rules when
  a cost or color equals another attribute value.

* `assignByPolygon()` and `findLand()` handle layers in a projected CRS, and
  `assignByPolygon()` works on `gData` objects with several locations on the
  same node.

* `closestNode()` accepts a single location as `c(lon, lat)`.

* `polygonBetween()` detects polygon outlines correctly on subset graphs.

* `areConnected()` no longer reports an isolated node as connected in graphs
  without edges.

* `combineCosts()` compares the edges of the two graphs reliably.

* `dropDeadEdges()` also drops edges when all costs are equal.

* `dijkstraFrom()` on a `gData` gives an informative error when `start` is not
  connected to the locations.

* Plotting: `plotEdges()` works with a single visible edge; `points()` keeps
  per-node colors on the right nodes when zoomed in; `plot(x, shape = NULL)`
  draws the nodes of the new area; `geo.segments()` keeps per-segment colors
  with their segments.

* `[` and `geom_gdata()` work on `gData` objects whose data is a vector.

* Validity checks give informative messages.

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
