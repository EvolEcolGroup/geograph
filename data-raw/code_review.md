# Code review of geoGraph (branch `codereview`)

Review of the whole package (all 42 files in `R/`, tests, data documentation)
starting from `dev` at `0f1da0f` (version 2.0.1.9001), October 2026.

Every fix is a **separate commit with its own tests**. For each one, the new
tests were checked to fail on the code before the fix (except where noted:
pure refactors and performance changes, where tests guard against
regressions). To review an item, run `git show <commit>`.

How to use this file: tick each box once you have reviewed and accepted the
commit. Items marked **⚠ behaviour change** alter results or errors that
existing user code may rely on, so they deserve a closer look.

## Checks run on the branch

- `devtools::test()`: 275 tests, 0 failures, 0 errors, 0 warnings
  (59 of them new, in 22 test files, 3 of which are new files).
- `R CMD check --no-manual` (with examples, tests and rebuilding the
  vignettes): 0 errors, 0 warnings, 1 note ("unable to verify current time",
  from the sandbox having no network access, not from the package).
- `lintr::lint_package()`: no new lints from the changes. The six extra
  `object_usage_linter` hits ("no visible global function definition for
  `.nodesAttr`/`.connectedSets`/`.locationsToSf`/`setNodesAttr`") are false
  positives: lintr resolves names against the *installed* geoGraph, which is
  older than the source (the pre-existing hits for `makeSquareGrid` and
  `assignByPolygon` have the same cause).

---

## 1. Wrong results without any error

These are the most important: they silently return incorrect values.

- [ ] **1.1 Dijkstra paths between `from` and `to` dropped most pairs**
  ⚠ behaviour change (`b9919d3`)
  - *Problem*: `dijkstraBetween(gGraph, from, to)` recycled `from` and `to`
    to a common length and kept only the pairs `i < j` of the recycled
    vectors. `from = "1", to = c("2", "3")` returned only `1:3`;
    `from = c("1", "2"), to = c("3", "4")` returned only `1:4`.
    `polygonBetween()` relies on it, so it returned an arbitrary subset of the
    polygon-to-polygon paths.
  - *Fix*: if `to` is missing or identical to `from`, all pairs `i < j` (as
    before, and what the `gData` method uses); otherwise every
    `from[i] -> to[j]` path. Pair indices come from `utils::combn()` (no
    longer grown in a quadratic loop), and paths are always re-ordered by name.
  - `gPath` objects now carry a `type` attribute (`"from"`, `"pairwise"`,
    `"cross"`). `gPath2dist()` uses it instead of guessing from the path
    names, which also fixes `dijkstraBetween()` on a `gData` with **two**
    locations (it returned a vector instead of a `dist`). `"cross"` paths
    give a `length(from) x length(to)` matrix. Objects without the attribute
    fall back to the old heuristic.
  - *Tests*: `test_dijkstra.R`, `test_gPath2dist.R`.

- [ ] **1.2 Cost and colour rules applied in a chain** ⚠ (`a888403`)
  - *Problem*: `setCosts()`, `getNodeCosts()` and `getColors()` replaced
    values rule by rule, so a cost that equals another attribute value was
    replaced again: with rules `1 -> 2`, `2 -> 10`, a node coded 1 got cost 10
    (verified: `getNodeCosts` returned `10 10 3` instead of `2 10 3`).
  - *Fix*: a single `match()`. In `getNodeCosts()`/`getColors()`, values
    without a rule now give `NA` instead of leaking the attribute value
    through as a cost or colour (`setCosts()` already stops on them).
  - *Tests*: `test_setCosts.R`, `test_accessors.R`.

- [ ] **1.3 Projected layers in `assignByPolygon()` and `findLand()`** (`ec34f18`)
  - *Problem*: query points were built with `st_set_crs(st_crs(layer))`,
    i.e. lon/lat values were read as if they were in the layer's CRS. With a
    UTM / web-mercator shapefile, degrees became metres and points fell in
    the wrong polygons (verified: two points in polygons A and B both came
    back as A).
  - *Fix*: points are created in EPSG:4326 and transformed to the layer CRS
    (`.locationsToSf()`); layers without a CRS are still assumed lon/lat.
  - *Tests*: `test_assignByPolygon.R`, `test_findLand.R`.

- [ ] **1.4 Polygon outlines wrong on subset graphs** (`8f3cc3c`)
  - *Problem*: `polygonBetween(outline = TRUE)` compared `@edgeL` neighbours
    (integer positions) with node names; this only works while names equal
    positions. On a subset graph (e.g. `rawgraph.10k[isInArea(...)]`) almost
    every node was considered an outline node.
  - *Fix*: neighbours by name with `graph::edges()`. The `@return` doc
    (which promised a numeric vector) now describes the returned `gPath`.
  - *Tests*: `test_polygonBetween.R`.

- [ ] **1.5 `findLand()` / `assignByPolygon()` duplicated existing attributes**
  ⚠ (`7f0e2fb`, `1d40fb7`; test tweak in `1c638f8`)
  - *Problem*: re-running on a graph that already had the attribute (e.g.
    `findLand(worldgraph.10k)`, which has `habitat`) appended a second column
    with the same name; `getNodesAttr()` kept returning the old one, so the
    call had no visible effect. `assignByPolygon()` on a single-node graph
    also overwrote all other attributes (test was `nrow > 1`).
  - *Fix*: columns are set by name (replacing existing ones).
    `findLand()` (matrix) always returns levels `c("land", "sea")`, as
    documented (only `"land"` when all points were on land).
  - *Tests*: `test_findLand.R`, `test_assignByPolygon.R`.

- [ ] **1.6 `dijkstraBuffer()` excluded the origin** ⚠ (`114e018`)
  - *Problem*: built on `dijkstraFrom()`, which skips the start node: the
    origin was never in its own buffer (`reachable = FALSE` for it). It also
    stopped on any graph that is not fully connected, and built every path
    only to sum their lengths.
  - *Fix*: uses `RBGL::dijkstra.sp()` costs directly; origin (cost 0)
    included, unreachable nodes (cost `Inf`) left out; ~9x faster on a
    1,250-node graph (more on larger ones).
  - *Tests*: `test_dijkstraBuffer.R` (including agreement with
    `dijkstraFrom()` costs).

- [ ] **1.7 `gData` constructor ignored `nodes.id`** (`971647c`)
  - *Problem*: `new("gData", nodes.id = ...)` re-read the empty prototype
    slot instead of the argument, so the ids were silently dropped. The
    warning for unknown nodes referenced a non-existent slot
    (`x@gGraphName`) and the missing-graph warning printed an empty name
    (`input$gGraphName`).
  - *Fix*: typos fixed; `nodes.id` must have one value per location.
  - *Tests*: `test_classes.R`.

- [ ] **1.8 `gGraph` constructor ignored `meta`** (`22dd23e`)
  - *Problem*: `new("gGraph", ..., meta = list(...))` dropped `meta`.
    `makeHexGrid()` passed its graph as `graphNEL =`, which worked only
    through `$` partial matching, plus two ignored arguments.
  - *Fix*: `meta` handled (must be a list); `makeHexGrid()` passes `graph =`.
  - *Tests*: `test_classes.R`, `test_makeHexGrid.R`.

- [ ] **1.9 `points()` drew per-node colours on the wrong nodes** (`7b8ff77`)
  - *Problem*: colours were computed twice; the second pass recycled the
    colours of the visible nodes over all nodes, so when zoomed in nodes got
    other nodes' colours (two visible nodes were swapped in the test).
  - *Fix*: the colour logic, copied four times in `plot()`/`points()`, is now
    one helper `.nodeColors()`.
  - *Tests*: `test_plot_gGraph.R` (traces the colours actually sent to the
    device).

- [ ] **1.10 `plot(x, shape = NULL)` drew the nodes of the previous area**
  (`9615d64`)
  - *Problem*: visible nodes were chosen with `reg = "current"`, i.e. from
    the plot still on the device. After zooming on Europe,
    `plot(worldgraph.10k, reset = TRUE, shape = NULL)` drew 320 of 10,242
    nodes on a world-sized plot.
  - *Fix*: use the new zoom limits.
  - *Tests*: `test_plot_gGraph.R`.

- [ ] **1.11 `geo.segments()` mixed up per-segment colours** (`41beb46`)
  - *Problem*: antimeridian-crossing segments are drawn separately, but the
    full `col`/`lwd`/`lty` vectors were passed to both calls (a segment meant
    to be blue was drawn red). Also removes a stray `XMIN` statement left by
    a comment split over two lines.
  - *Tests*: `test_geo.segments.R`.

- [ ] **1.12 `areConnected()` TRUE for an isolated node** (`faf821c`)
  - *Problem*: the "keep sets with more than one node" code (copied in
    `areConnected`, `isReachable` and both `connectivityPlot` methods) used
    `1:(which.min(temp) - 1)`, which is `1:0` when all sets are singletons,
    selecting the first one: in an edgeless graph node "1" was "connected",
    the others not.
  - *Fix*: one helper `.connectedSets()`. `isReachable()` loses a loop that
    recomputed the same test once per set; `connectivityPlot()` (gData) a
    redundant assignment.
  - *Tests*: `test_connectivity.R`.

- [ ] **1.13 `combineCosts()` edge comparison** (`79ef76a`)
  - *Problem*: `all(unlist(edgeL1) == unlist(edgeL2))` recycled edge lists of
    different lengths (with a warning) and ignored node names: graphs with
    the same structure but different nodes were silently combined.
  - *Fix*: `identical()` on node names and per-node edges.
  - *Tests*: `test_combineCosts.R`.

- [ ] **1.14 `dropDeadEdges()` ignored uniform costs** (`ba570d1`)
  - *Problem*: it returned early when `hasCosts()` was FALSE, which is also
    the case when all costs are equal: edges all costing 5 survived
    `thres = 1`.
  - *Fix*: only skip graphs with no stored costs.
  - *Tests*: `test_dropDead.R` (new file).

## 2. Errors on valid input

- [ ] **2.1 `gGraph` objects without node attributes** (`17798cb`)
  - *Problem*: graphs from `new("gGraph", coords = ...)` or
    `makeSquareGrid()` had a 0 x 0 `@nodes.attr`: `setNodesAttr()` failed
    ("replacement has n rows, data has 0") and so did `getNodesAttr()`
    ("undefined columns selected"), hence `buffer(res.type = "gGraph")` and
    `geom_ggraph()`. `makeHexGrid()` graphs (n x 0 table) could not even be
    subset with `[` (`df[i, TRUE]` errors on a data.frame with no columns).
  - *Fix*: the constructor creates one row per node; `getNodesAttr()`,
    `setNodesAttr()` and `[` handle zero-column and legacy 0 x 0 tables
    (`.nodesAttr()` helper).
  - *Tests*: `test_setNodesAttr.R`.

- [ ] **2.2 Coordinates without column names** (`93e9c1a`)
  - `new("gGraph", coords = matrix(...))` failed with "missing value where
    TRUE/FALSE needed"; now treated like unrecognised names (message, given
    order used). *Tests*: `test_classes.R`.

- [ ] **2.3 `assignByPolygon()` on `gData` with locations sharing a node**
  (`ef6ebc0`)
  - Failed with "duplicate 'row.names' are not allowed" (node ids used as
    row names). *Tests*: `test_assignByPolygon.R`.

- [ ] **2.4 `closestNode()` with `c(lon, lat)`** (`42d53fb`, rewrapped in
  `0e97f91`)
  - A plain numeric vector gave "coords does not have two columns"; this
    broke `dijkstraBuffer(origin = c(lon, lat))`, which documents that form.
    *Tests*: `test_closestNode.R`, `test_dijkstraBuffer.R`.

- [ ] **2.5 `polygonBetween()` on `gData`** (`b188177`)
  - Documented but always failed ("unused arguments": the `gData` method of
    `dijkstraBetween` takes no `from`/`to`). Polygons are now read from the
    linked `gGraph`, and paths computed between the `gData` locations
    falling in each polygon. *Please check these are the semantics you
    want.* *Tests*: `test_polygonBetween.R`.

- [ ] **2.6 `dijkstraFrom()` start node validation** ⚠ (`d15524a`)
  - On a `gData`, a start node in another connected set failed with
    "subscript out of bounds" after RBGL NA warnings; it now gives an
    informative error. Several start nodes produced mismatched start/finish
    pairs, so a single node is now required (as documented).
    *Tests*: `test_dijkstra.R`.

- [ ] **2.7 `isInArea()` with an invalid `reg`** ⚠ (`f1e1f03`)
  - Any unrecognised `reg` returned a single `NA`, which propagates silently
    (`x[isInArea(x, reg)]` builds a gGraph of `NA` rows). This included
    `c(xmin, xmax, ymin, ymax)`, accepted by `geo.zoomin()`; that form is now
    supported and anything else is an error. **The existing test expecting
    `NA` was changed** to expect an error. The area message no longer embeds
    the source indentation. *Tests*: `test_isInArea.R`.

- [ ] **2.8 `plotEdges()` with one visible edge** (`6da1597`)
  - Missing `drop = FALSE`: "argument is of length zero".
    *Tests*: `test_plot_gGraph.R`.

- [ ] **2.9 `gData` with vector `@data`** (`ba341ea`, `f804320`)
  - `[` failed with "argument is of length zero" (`nrow()` of a vector), so
    the branch written for vectors was unreachable; `geom_gdata()`/
    `autoplot()` failed with "missing value where TRUE/FALSE needed". A
    vector is now exposed as a `data` column for ggplot.
    *Tests*: `test_basicMethods.R`, `test_plot_ggplot.R`.

- [ ] **2.10 `assignByRaster()` with a custom function without `na.rm`**
  (`2921184`)
  - `fun(vals, na.rm = na.rm, ...)` failed with "unused argument" for e.g.
    `function(v) length(v)`. `na.rm` is now passed only to functions that
    accept it (primitives, or an `na.rm`/`...` formal); otherwise NAs are
    removed beforehand. Note: `terra`'s `as.data.frame()` already drops `NA`
    cells, so `na.rm` currently has little effect for single-layer rasters.
    *Tests*: `test_assignByRaster.R`.

- [ ] **2.11 `gPath` `xy` attribute** (`5f6d2a9`)
  - Became a vector when all paths involve one node (missing `drop = FALSE`).
    *Tests*: `test_dijkstra.R`.

## 3. Performance

- [ ] **3.1 `setDistCosts()` vectorised** (`b5e7a22`)
  - One `fields::rdist.earth()` call per edge replaced by one
    `rdist.earth.vec()` call: same values (max. difference 4e-11),
    `setDistCosts(rawgraph.10k)` 0.93 s -> 0.11 s. Docs now state that costs
    are **in miles** (see 5.1). Tests pass before and after (value checks).
- `dijkstraBuffer()` speed-up: see 1.6.

## 4. Code quality and documentation

- [ ] **4.1 Validity methods return messages** (`b1e5dfd`)
  - They printed with `cat()` and returned `FALSE`, so `validObject()` said
    "invalid class “gGraph” object: FALSE". The node-label check used
    `all(a == b)` (recycling, TRUE for an empty graph); now `identical()`.
    *Tests*: `test_classes.R`.
- [ ] **4.2 Documentation fixes** (`201477f`): `getEdges(unique)` default;
  `hgdp` has 51 populations (not 52); `@meta$colors` is a data.frame;
  `hasCosts()` example comment.
- [ ] **4.3 NEWS entries** (`c54c1bd`), in a new "development version"
  section.

---

## 5. Not changed: points for discussion

These need a decision or were out of scope for safe, small fixes.

- [ ] **5.1 Units: miles vs km.** `setDistCosts()` returns great-circle
  distances in **miles** (the `fields` default), while `buffer()` takes `d`
  in **km**. I only documented it: changing it would alter every cost
  computed so far. Consider a `miles = FALSE` argument, or switching to km
  with a NEWS entry.
- [ ] **5.2 `gData` objects are tied to the global environment.** `gData`
  methods find their graph with `get(x@gGraph.name, envir = .GlobalEnv)`.
  They fail inside functions, `lapply()`, packages or parallel workers where
  the graph is not global. The tests already have to `assign()` into
  `.GlobalEnv`. This is the main architectural limitation of the package;
  an explicit `graph` argument (with the name as fallback) would remove it.
- [ ] **5.3 `dijkstraFrom()` on a `gGraph` requires the whole graph to be
  connected** (`areConnected(x, getNodes(x))`), so it fails on e.g.
  `worldgraph.10k`. Paths could be computed only to the nodes reachable from
  `start` (a test currently pins the error).
- [ ] **5.4 Global RNG side effect.** `connectivityPlot(seed = )` and
  `plot.gPath(seed = )` call `set.seed()`, changing the user's random stream
  after the call. Consider restoring `.Random.seed` on exit.
- [ ] **5.5 `hgdpPlus` size.** Its documentation says `hgdp` (51) plus 24
  native American populations (75), but it has 76 rows.
- [ ] **5.6 `assignByPolygon()` when `attr` is not in the layer** prints the
  layer with `cat()`/`print()` and returns `NULL` instead of signalling an
  error or warning. The `gGraph` method now returns `x` unchanged in that
  case (it used to fail for graphs with at most one attribute row).
- [ ] **5.7 `assignByRaster()` nearest node** uses `st_nearest_feature()`
  with s2 switched off, i.e. planar distances in degrees: increasingly
  inaccurate towards the poles. Raster cells are also assigned to their
  nearest node, so nodes in a coarse raster can get `NA`.
- [ ] **5.8 Interactive functions (not run, inspection only).**
  `geo.add.edges(mode = "area")` draws the selected `refObj` edges using the
  coordinates of `x` with `refObj` indices (wrong segments drawn when `x` is
  not `refObj`), and `setEdges()` then fails if the edges involve nodes
  missing from `x`. `geo.change.attr(restore.edges = TRUE)` has the same
  risk.
- [ ] **5.9 Dead or no-op code.**
  - `.closeOneKnn()` "caches" `x@meta$.xyz` in a local copy, so the cache is
    never reused. If it ever were persisted, `[` would keep a stale index.
  - `plotEdges()` ends with `eval(last.points)`, but `last.points` is a
    list of calls once sticky points exist, and `eval(<list>)` does not
    evaluate them. Making it work as intended would recurse when
    `sticky.edges = TRUE` stores a `plotEdges()` call.
- [ ] **5.10 Smaller robustness points.**
  - `[` on a `gData` with character `i` keeps only the first location of
    each node (`match()`), although several locations can share a node.
  - `isReachable()` returns a single `FALSE` (not one value per location)
    when the reference node is isolated.
  - The `gData` validity method calls `stop()`/`warning()` instead of
    returning messages, so `is.gData()` errors on a `gData` without
    `gGraph.name`.
  - `getEdges(unique = TRUE)` and `getCosts(unique = TRUE)` rely on string
    comparison of node names and on splitting names at `"."`. This works for
    the package's numeric names but breaks with names containing dots.
  - `setCosts(method = "function")` with an asymmetric `FUN` gives the two
    directions of an undirected edge different weights (not checked how
    `graphNEL` resolves this).
  - `areNeighbours()` does not check that the nodes exist (unknown nodes give
    `FALSE`).
