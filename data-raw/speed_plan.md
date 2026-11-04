---
editor_options: 
  markdown: 
    wrap: 72
---

# Performance plan for geoGraph (branch `speed`)

Review of the whole package (`R/`, 42 files) for performance
bottlenecks, starting from `dev` at `9f4fce4` (version 2.1.0), November
2026. Every hotspot below was **measured**, and the main fixes were
**prototyped and checked to give identical results** to the current
code. This document is meant to be handed to an agent (or a person) who
will implement the changes; section 2 says how to work, sections 3–6
say what to do.

Supporting files, all in `data-raw/` (excluded from the build by
`.Rbuildignore`):

-   `data-raw/speed/benchmark.R`: benchmark harness (before/after
    numbers for every item).
-   `data-raw/speed/proto_shortestPaths.R`: verified drop-in replacement
    for `RBGL::sp.between()` (item P1.1).
-   `data-raw/speed/proto_edgeTable.R`: verified integer-space edge
    helpers and graph builder (items P0, P1.2, P1.3, P1.6, P2.5).
-   `data-raw/speed/verify_prototypes.R`: re-runs the equivalence checks
    of the prototypes against the current code (all print `TRUE` on
    `dev` at `9f4fce4`). Run it again after implementing, pointing it
    at the new package functions, as a quick regression check.

------------------------------------------------------------------------

## 1. Summary

### Where the time goes

Almost all of the slow code falls into five patterns:

1.  **`RBGL::sp.between()`** (used by `dijkstraFrom()`,
    `dijkstraBetween()`, `polygonBetween()`). For *every path* it calls
    `match()` against all node names, subsets the 40k-element named
    list of edge weights by name, and builds the path by growing a
    vector with `c()`. About 2.7 ms per path on a 40k graph, so
    thousands of paths take minutes, and an unreachable target costs
    \~15 s on its own.
2.  **`graph::subGraph()`** (used by `[` on a `gGraph`, hence
    `dropDeadNodes()`, `keepMaxConnectedSet()`, `x[isInArea(x)]`).
    It runs `match(droppedNodes, neighbours)` for every kept node:
    O(kept × dropped), i.e. quadratic.
3.  **Per-node R loops with name lookups or `%in%` on a big set**:
    `keepMaxConnectedSet()` (`i %in% maxSet.idx` inside a 40k loop),
    `getCosts(res.type = "asIs", unique = TRUE)` (`res[[node]]`, a
    linear scan of 40k names, inside a 40k loop), `areNeighbours()`,
    `polygonBetween()` outlines, `buffer()`.
4.  **Rebuilding a `graphNEL` from a nested edge list**
    (`new("graphNEL", edgeL = list(list(edges, weights)))` in
    `setCosts()`, `dropDeadEdges()`, `combineCosts()`,
    `keepMaxConnectedSet()`, `makeHexGrid()`; `ftM2graphNEL()` in
    `makeSquareGrid()`). The nested form makes `graph` write every
    weight twice (both directions of every undirected edge). Building
    from a character edge list and assigning weights once is 2× faster
    and gives identical graphs.
5.  **Generic spatial machinery used where a k-d tree suffices**:
    `assignByRaster()` uses `sf::st_nearest_feature()` (GEOS) on
    millions of raster cells, then `tidyr::nest()` + `sapply()`.
    `FNN::get.knnx()` on the same planar coordinates gives the
    *identical* assignment 10–13× faster.

### Top items (measured on `worldgraph.40k`: 40,962 nodes, 29,458 edges; `rawgraph.40k`: 122,880 edges)

| Item | Function(s) | Now | After (prototype) | Output |
|----|----|---:|---:|----|
| P1.1 | `dijkstraFrom()` (gGraph, \~10.5k connected nodes) | \~28 s (est., 2.7 ms/path) | 1.5 s | identical |
| P1.1 | `RBGL::sp.between`, 1 start → 2,000 targets | 5.4 s | 0.68 s | identical |
| P1.1 | `dijkstraBetween(hgdp)` (51 locations, 1,275 paths) | 5.5–7.6 s | 3.1–3.5 s | identical |
| P1.1 | any path to an unreachable node | 14.8 s *per target* | \~0 | identical |
| P1.2 | `x[every other node]` | 5.4 s | 0.47 s | identical |
| P1.2 | same, graph without costs | 12.2 s | 0.19 s | identical |
| P1.3 | `keepMaxConnectedSet(worldgraph.40k)` | 11.9–14.3 s | 0.67 s | identical |
| P1.3 | `dropDeadNodes(worldgraph.40k)` | 6.9–9.5 s | \~0.5 s (est.) | identical |
| P1.4 | `assignByRaster()`, 175k cells | 1.8 s | 0.14 s | identical |
| P1.4 | `assignByRaster()`, 4.4M cells | 59 s | \~5 s | identical |
| P1.5 | `getCosts(res.type = "asIs", unique = TRUE)` | 5.8–7.6 s | 0.46 s | identical |
| P1.6 | `setCosts()` on a 136k-node hex grid | 7.5 s | 3.9 s | identical |
| P1.6 | `setCosts()` / `dropDeadEdges()` on worldgraph.40k | 0.98 / 1.3 s | 0.54 / 0.94 s | identical |
| P1.7 | `makeHexGrid(spacing = 15)` over Europe (136k nodes) | 39 s | \~31 s (P1.7) → \~19 s (+P2.10) (est.) | identical (P2.10: see item) |
| P2.1 | `plotEdges()` / `plot(edges = TRUE)` | 1.0–2.2 s | \<0.5 s (est.) | see item |
| P2.2 | `geom_ggraph(edges = TRUE)` + `ggplot_build()` on rawgraph.40k | 4.8–6.5 s | \~3 s (est.) | identical geometry |
| P2.3 | `.gpathDf()` (611 paths) | 0.28 s | 0.004 s | identical |
| P2.5 | `areNeighbours()`, 5,000 pairs | 0.74 s | 0.04 s | identical |
| P2.5 | `setEdges(remove = 5,000 edges)` | 4.3 s | 1.1 s | identical |
| P2.6 | `hasCosts()` | 0.31 s | 0.04 s | identical |

Expected overall effect: the common workflows of the vignettes
(`dijkstraBetween()` / `dijkstraFrom()` on 40k graphs, subsetting,
`keepMaxConnectedSet()`, `setCosts()`, `assignByRaster()`,
`makeHexGrid()`) become 2–20× faster; nothing that is currently fast
gets slower. No new package dependency is needed for Phases 0–2.

### Phases

-   **Phase 0**: two internal helpers (`.edgeTable()`,
    `.graphFromTable()`) that everything else builds on.
-   **Phase 1**: high-impact fixes with identical outputs (P1.1–P1.7).
-   **Phase 2**: medium/low-impact fixes, still identical outputs or
    trivially equivalent (P2.1–P2.12).
-   **Phase 3**: options that change results or add a dependency: **do
    not implement without the maintainer's decision** (P3.1–P3.5).

### Measured and *not* worth changing

`is.gGraph()`/validity (\~1 ms), `closestNode()` (0.15 s for 100,000
locations), `isInArea()`, `findLand()` (0.3 s), `assignByPolygon()`
(0.4 s), `rnaturalearth::ne_countries()` (0.1 s first call, cached
afterwards), `RBGL::connectedComp()` (0.19 s), `areConnected()`,
`dijkstraBuffer()` (0.3 s, mostly `edgeWeights()`), `gPath2dist()`,
`plot()` without edges (0.2 s), `connectivityPlot()` (0.3 s).

------------------------------------------------------------------------

## 2. How to work (instructions for the implementing agent)

### Setup

The container used for this review had no R; this worked (Ubuntu 24.04):

``` sh
apt-get install -y --no-install-recommends r-base-core r-base-dev r-cran-sf \
  r-cran-terra r-cran-dplyr r-cran-fields r-bioc-graph r-bioc-rbgl r-cran-fnn \
  r-cran-ggplot2 r-cran-tidyr r-cran-tibble r-cran-magrittr \
  r-cran-rnaturalearthdata r-cran-testthat r-cran-pkgload r-cran-bench
mkdir -p ~/R/lib
Rscript -e 'install.packages(c("rnaturalearth", "dggridR"), lib = "~/R/lib",
  repos = "https://cloud.r-project.org")'   # dggridR compiles for ~15 min
export R_LIBS_USER=~/R/lib
```

### Commands

-   Tests: `Rscript -e 'testthat::test_local(".")'`. Baseline on `dev`:
    **3,639 expectations, 0 failures, 0 skips, \~95 s**.
-   Benchmarks: `Rscript data-raw/speed/benchmark.R` (quick set, \~2
    min), `--slow` adds the multi-minute cases, `--only=dijkstra,raster`
    filters by group. Baseline numbers are in Appendix A.
-   Before finishing: `R CMD build . && R CMD check --no-manual
    geoGraph_*.tar.gz` and `lintr::lint_package()` (no new lints).

### Rules

1.  **One item = one commit**, with its tests. Put the before/after
    benchmark lines in the commit message. Do the Phase 0 helpers
    first; most items depend on them.
2.  **Outputs must stay identical** unless an item is marked ⚠. For
    every rewritten function, add a test that compares the new result
    with a *reference* on small and medium graphs (e.g. `worldgraph.10k`
    subsets):
    -   for P1.1, the reference is `RBGL::sp.between()` itself (still
        available): compare with `identical()` after re-ordering
        `sp.between`'s output by `paste(start, finish, sep = ":")`;
    -   for graph rebuilds, compare `nodes()`, `edges()`,
        `edgeWeights()`, `edgemode()` and `length(g@edgeData@data)`
        with `identical()`. **Do not** compare the `graphNEL` objects
        with `identical()`: the internal order of `@edgeData@data`
        entries can differ while the graph is the same;
    -   where the old implementation is the only reference, keep a copy
        of it in `tests/testthat/helper-reference.R` (test-only code)
        and compare against it.
3.  **No change to exported signatures, return types, names or
    ordering** of results (node order, path order, list names,
    attribute names, row names), unless marked ⚠.
4.  **No new dependencies** in Phases 0–2. Do not call RBGL's compiled
    routines directly (`.Call("BGL_...", PACKAGE = "RBGL")`): R CMD
    check flags foreign calls into another package, which CRAN rejects.
    Use exported functions (`RBGL::dijkstra.sp()`,
    `RBGL::connectedComp()`, `graph::edgeData<-`, ...).
5.  Keep `Depends: R (>= 3.5.0)`: no `\(x)`, no `|>`, no
    `sequence(nvec, from)` (R 4.0); `lengths()`, `rep.int()`,
    `.mapply()`, `vapply()`, `split()` are fine.
6.  The package already reads `@edgeData@data` directly (`dropCosts()`,
    `hasCosts()`, `dropDeadEdges()`, `combineCosts()`), so using it is
    acceptable; prefer the `graph` API when it is not slower.
7.  Tests that read `@edgeData@data` positionally exist
    (`tests/testthat/test_setCosts.R:19-23` and `:51-53`). They remain
    valid as long as both compared graphs are built by the same code
    path; do not "fix" them by changing their logic.
8.  Add a NEWS entry per user-visible speed-up under the development
    version, and document every ⚠ change.

------------------------------------------------------------------------

## 3. Phase 0: shared helpers

### P0. `.edgeTable()` and `.graphFromTable()` (new file `R/utils-graph.R`)

Prototype: `data-raw/speed/proto_edgeTable.R` (verified).

-   `.edgeTable(g, weights = TRUE)` returns
    `list(from = <int>, to = <int>, w = <num>)`, one element per
    **directed** edge (an undirected edge appears twice), in the order
    of `g@edgeL`, which is also the order of
    `unlist(edgeWeights(g))` and of `edgeMatrix(g, duplicates = TRUE)`
    (verified). `from`/`to` are node positions.
-   `.graphFromTable(nodes, from, to, w = NULL, edgemode =
    "undirected")` builds the `graphNEL`:
    1.  edge list as a list of **character** vectors (not
        `list(edges = , weights = )`): with the nested form `graph`
        initialises every weight through `edgeData<-`, which is the
        expensive part;
    2.  if `w` is `NULL`, nothing else: the graph has no stored edge
        data, exactly like `subGraph()` of an unweighted graph;
    3.  otherwise `edgeDataDefaults(g, "weight") <- 1`, then assign all
        weights **with the graph temporarily in directed mode**, so
        that `edgeData<-` writes each `from|to` key exactly once with
        its own weight. In undirected mode, `edgeData<-` also writes
        `to|from`, which (a) doubles the work and (b) is what swaps the
        weights of an asymmetric `setCosts(method = "function")` (see
        P3.3). The prototype sets `g@graphData$edgemode`; first try
        `graph::edgemode(g) <- "directed"` and back, and keep it if it
        is not slower (it may re-validate the graph).
-   Results (rawgraph.40k, 245,760 directed edges): current nested
    construction 1.8–2.3 s, helper 0.87 s; identical `edges()`,
    `edgeWeights()` and `RBGL::dijkstra.sp()` distances.
-   Tests: round trip `g -> .edgeTable -> .graphFromTable` on
    `worldgraph.10k` (weighted), `dropCosts(rawgraph.10k)` (no weights),
    a 2-node graph, an edgeless graph, and a graph with an isolated
    node; check that the result passes `validObject()` and that
    `isDirected()` is unchanged.
-   Note: numeric keys `(from - 1) * N + to` (used in several items to
    match edges) are exact for any graph below \~9e7 nodes; use them
    instead of `paste()` keys.

------------------------------------------------------------------------

## 4. Phase 1: high impact

### P1.1 Fast shortest paths: replace `RBGL::sp.between()`

-   **Where**: `R/dijkstraFrom.R:53` and `:98`,
    `R/dijkstraBetween.R:76`; indirectly `polygonBetween()`.
-   **Problem** (`Rprof` of `sp.between` on worldgraph.40k): 58% in its
    `getw()` (per path, `eW[nl]` subsets a 40,962-element named list by
    name, then a per-step loop), 16% in `nodeind()` (per path,
    `match()` against all node names), 30% in `extractPath()` (grows
    the path with `c()`, O(L²)); results are also accumulated with
    `ans[[name]] <- ...` (growing a list by name). Each call to
    `RBGL::dijkstra.sp()` without precomputed weights also costs 0.42 s
    in `edgeWeights()` (sp.between does pass them; keep doing so). An
    unreachable target makes `extractPath()` loop over all nodes: 14.8 s
    per such target.
-   **Fix**: add an internal `.shortestPaths(graph, start, finish, eW =
    NULL)` returning exactly what `sp.between()` returns, but **one
    element per requested pair, in the requested order, named
    `"start:finish"`** (duplicates kept). Prototype:
    `data-raw/speed/proto_shortestPaths.R`. Algorithm:
    1.  `eW <- unlist(edgeWeights(g), use.names = FALSE)` once;
        `em <- edgeMatrix(g, duplicates = !isDirected(g))`; numeric
        edge keys `(em[1, ] - 1) * N + em[2, ]`.
    2.  For each unique start: `pen <- RBGL::dijkstra.sp(g, start, eW)$penult`.
        Walk back from **all** targets of that start at once
        (vectorised over targets; the loop runs once per step of the
        longest path, not once per path). BGL sets `penult[v] = v` for
        unreachable `v`: stop those and return `NA` (as `sp.between`
        does, without the warning).
    3.  All step weights at once: concatenate the paths, look up
        `eW[match((u - 1) * N + v, key)]`, name them
        `paste(from, to, sep = "--")` (`"->"` for directed graphs),
        `split()` by path.
    4.  `length = sum(<step weights>)` per path (same summation as
        `sp.between`: keeps results bit-identical, do **not** use the
        Dijkstra distance); a single-node path (start == finish) has
        `length = NA`, `length_detail = list(NA)` as in `sp.between`.
-   Then in `dijkstraFrom()` (both methods) and `dijkstraBetween()`
    call `.shortestPaths()` with explicit `start`/`finish` vectors of
    equal length and drop the re-ordering code (`gsub(".*:", ...)`,
    `res[paste(start, finish, sep = ":")]`): the helper already returns
    the requested order. Keep the `gPath` attributes exactly as now.
-   **Verified**: `identical()` to `sp.between()` (re-ordered) for 1 →
    2,000 targets, 50 nodes pairwise, start == finish, unreachable
    targets and duplicated pairs; `identical()` `gPath` objects from
    `dijkstraBetween(hgdp)`, `dijkstraFrom(hgdp, ori)` and
    `dijkstraFrom(<connected Europe gGraph>)`.
-   **Gains**: 1 → 2,000 paths 5.4 → 0.68 s; 10,507 paths 1.5 s
    (sp.between: \~2.7 ms/path, i.e. \~28 s); `dijkstraBetween(hgdp)`
    7.6 → 3.5 s; `dijkstraFrom(gGraph, 611 nodes)` 0.27 → 0.06 s.
-   **Remaining cost** of the pairwise case is one
    `RBGL::dijkstra.sp()` per start (\~37 ms on a 40k graph, 28 ms of
    which is its internal `edgeMatrix()`); see P3.1 for going further.
-   Also: the gData method of `dijkstraFrom()` computes connected
    components twice (`isConnected(x)` then `areConnected(...)`, 0.2 s
    each on 40k): compute `.connectedSets()` once and test both
    conditions on it (keep both error messages).
-   **Tests**: new `test_shortestPaths.R` comparing with
    `RBGL::sp.between()` on `worldgraph.10k` (connected subsets,
    pairwise, cross, start == finish, unreachable, duplicates, a
    directed graph built with `.graphFromTable(..., edgemode =
    "directed")`). Existing `test_dijkstra.R`, `test_gPath2dist.R`,
    `test_polygonBetween.R` must pass unchanged.

### P1.2 Subsetting `gGraph` objects without `graph::subGraph()`

-   **Where**: `R/basicMethods.R:70`.
-   **Problem**: `subGraph()` runs `match(killedNodes, x)` for each kept
    node: O(kept × dropped). Keeping half of worldgraph.40k: 5.4 s;
    12.2 s for `dropCosts(rawgraph.40k)`.
-   **Fix**: internal `.subsetGraph(g, idx)` (prototype in
    `proto_edgeTable.R`): map old positions to new ones
    (`newpos[idx] <- seq_along(idx)`), keep table rows whose two ends
    are kept, remap, rebuild with `.graphFromTable()`, carrying weights
    only if the graph has stored edge data
    (`length(g@edgeData@data) > 0`) and keeping `edgemode(g)`. In `[`,
    replace the `subGraph()` call by
    `res@graph <- .subsetGraph(x@graph, i)` where `i` is already
    integer/logical at that point (character `i` is converted with
    `match()` above). Node order must follow `i`, as now.
-   **Verified identical** (nodes, edges, weights, number of stored
    entries, edgemode) for half of worldgraph.40k (5.4 → 0.47 s), half
    of an unweighted rawgraph.40k (12.2 → 0.19 s) and the Europe subset
    (0.72 → 0.32 s).
-   **Watch**: `subGraph()` also copies `@nodeData`; geoGraph never
    sets node data (attributes live in `@nodes.attr`), but keep
    `nodeData` defaults/data for the kept nodes if present, to be safe.
-   **Tests**: compare with `subGraph()` (still callable as reference)
    for logical, integer, character and re-ordered `i`; on weighted and
    unweighted graphs; check `validObject()`.

### P1.3 `keepMaxConnectedSet()` and `dropDeadNodes()`

-   **Where**: `R/keepMaxConnected.R:29-46`, `R/dropDead.R:69-77`.
-   **Problem**: `i %in% maxSet.idx` (and `edges %in% maxSet.idx`)
    inside a loop over all nodes is O(N × \|set\|); the graph is then
    rebuilt from a nested list and `dropDeadNodes()` subsets it through
    `subGraph()` (P1.2). 11.9–14.3 s on worldgraph.40k.
-   **Fix**: `keepMaxConnectedSet()` becomes "subset to the largest
    connected set": `idx <- sort(match(maxSet, getNodes(x)))`, then
    `x[idx]` (which uses P1.2). This is what the current code computes:
    removing all edges not inside the set and then dropping nodes
    without edges keeps exactly the nodes of the set, in their original
    order (verified identical nodes/edges/weights; 13.2 → 0.67 s).
    Edge cases to check against the current code and preserve, with
    tests: ties between largest sets (`which.max()` keeps the first),
    and an edgeless graph, where every set is a single node and the
    current code then drops nodes without edges (run it to see what it
    returns: probably an empty graph, or an error).
-   `dropDeadNodes()`: replace `unique(as.vector(getEdges(x,
    "matNames")))` + character subsetting with an integer mask
    (`has.edge <- lengths(edges(g)) > 0`, or from `.edgeTable()`) and
    `x[which(has.edge)]`. Note that the current code keeps nodes in
    order of first appearance in the edge matrix, which is increasing
    node order because the first column is sorted: check with a test
    that the order is unchanged.

### P1.4 `assignByRaster()`

-   **Where**: `R/assignByRaster.R:58-98`.
-   **Problem** (2.3M-cell world raster, 39.5 s in total):
    `sf::st_nearest_feature()` \~34 s, `st_as_sf()` 1.9 s,
    `as.data.frame(raster, xy = TRUE)` 1.5 s, `group_by` + `nest` +
    `left_join` + `sapply` \~1.3 s.
-   **Fix** (identical results):
    1.  cell centres and values without data frames or `sf`:
        `xy <- terra::xyFromCell(raster, seq_len(terra::ncell(raster)))`,
        `v <- terra::values(raster, mat = FALSE)`; drop `NA` cells, as
        `as.data.frame()` currently does;
    2.  if the raster is not in lon/lat, project the points to
        EPSG:4326 (`terra::project(xy, from = terra::crs(raster), to =
        "EPSG:4326")`, or `sf::sf_project()`), as the current
        `st_transform()` does;
    3.  nearest node with `FNN::get.knnx(unname(getCoords(graph)), xy,
        k = 1)`: **Euclidean distance on lon/lat, exactly what
        `st_nearest_feature()` does with s2 off** (verified: identical
        node ids for all 175,000 cells of a Europe raster). FNN is
        already imported;
    4.  `split(v, factor(nid, levels = seq_len(n.nodes)))`, then
        `.collapseNodeAttribute()` adapted to take plain vectors
        (`NULL`/empty → `NA_real_`, same `na.rm`, `any`/`all`
        coercion and scalar check as now). Drop `dplyr`/`tidyr`/
        `tibble` from this function (check whether they are still used
        elsewhere before touching `DESCRIPTION`).
-   **Gains**: 175k cells 1.8 → 0.14 s (13×, identical values); 2.3M
    cells \~39 → \~4 s; 4.4M cells 59 → \~5 s (estimates from the
    measured parts).
-   **Ties**: with regular rasters and regular grids (`makeSquareGrid`)
    a cell can be equidistant from two nodes; GEOS and FNN may break
    the tie differently. Add a test on a square grid with cells exactly
    midway; if results differ, document it (it is arbitrary in both
    cases) rather than trying to emulate GEOS.
-   The great-circle alternative is P3.2 (⚠, changes results).

### P1.5 `getCosts(res.type = "asIs", unique = TRUE)`

-   **Where**: `R/accessors.R:396-406`.
-   **Problem**: `lapply(names(res), function(node) res[[node]] ...)`:
    `[[` with a name scans the 40,962 names linearly, inside a loop
    over all nodes. 5.8–7.6 s.
-   **Fix**: iterate over names and elements together:
    `.mapply(function(node, e) if (length(e)) e[node < names(e)] else e,
    list(names(res), res), NULL)`, then `names(res) <- names(<weights>)`
    (and compute `edgeWeights()` once, not twice as now). Verified
    `identical()`, 6.35 → 0.46 s. Keep the string comparison here
    (semantic change is P3.4).

### P1.6 Vectorise cost setters and edge-filtering functions on the helpers

-   **Where**: `setCosts()` (`R/setCosts.R:122-146`), `dropDeadEdges()`
    (`R/dropDead.R:43-61`), `combineCosts()`
    (`R/combineCosts.R:81-99`), `setDistCosts()`
    (`R/setDistCosts.R:51-63`).
-   **Fix**:
    -   `setCosts()`, methods `"mean"`/`"product"`: `et <-
        .edgeTable(g, weights = FALSE)`; `w <- (nodeCosts[et$from] +
        nodeCosts[et$to]) / 2` (or the product); `.graphFromTable()`.
        Method `"function"`: keep calling `FUN` **once per node** with
        `(nodeCosts[i], nodeCosts[<neighbours of i>], ...)` (users'
        functions may not be vectorised; `max()` would silently break
        if called on whole vectors), collect the weights in table
        order, and pass them to `.graphFromTable()` **re-ordered to the
        reverse direction** (`w[match((to - 1) * N + from, (from - 1) *
        N + to)]`) so that the stored weights stay exactly as today
        (see P3.3 for the quirk this preserves).
    -   `dropDeadEdges()`: `keep <- et$w <= thres`, rebuild.
    -   `combineCosts()`: compare `nodes()` and the integer edge tables
        (`identical(et1$from, et2$from) && identical(et1$to, et2$to)`)
        instead of two `lapply()`s, combine `et1$w` and `et2$w`
        vectorised for `"sum"`/`"product"`; for `"function"` keep the
        per-node call as now; rebuild once.
    -   `setDistCosts()`: compute distances on the table and rebuild
        with `.graphFromTable()`, or keep `edgeData<-` but switch the
        graph to directed mode while assigning (see P0) to halve the
        work. Distances are symmetric, so values are identical.
-   **Verified** (worldgraph.40k): `setCosts` 0.98 → 0.54 s,
    `dropDeadEdges` 1.3 → 0.94 s, identical nodes/edges/weights; on a
    136k-node hex grid `setCosts(node.values = )` 7.5 → 3.9 s; on
    rawgraph.40k 1.8 → 0.87 s.
-   **Tests**: existing ones pass; add equivalence tests against the
    previous implementations (kept in `helper-reference.R`) for all
    three `setCosts` methods, including an **asymmetric** `FUN`
    (e.g. `function(a, b) a + 2 * b`), `node.values`, `cost.rules`.

### P1.7 Grid constructors: avoid default weights and node renaming

-   **Where**: `R/makeHexGrid.R:119-139`, `R/makeSquareGrid.R:159`,
    `R/classes.R:393-396`.
-   **Problem** (`makeHexGrid(spacing = 15)` over Europe, 136k nodes,
    39 s): 25 s in `dggridR::dgGEO_to_SEQNUM()` on 2.07M sample points
    (see P2.10), \~7 s building the `graphNEL` from a nested edge list
    (default weights of 1 written twice per edge), and 3 s in the
    `gGraph` constructor running `nodes(input$graph) <-
    rownames(coords)` although the names are already identical.
    `makeSquareGrid(size = 90000)`: 6 s, mostly `ftM2graphNEL()` doing
    the same weight initialisation.
-   **Fix**:
    -   `initialize("gGraph")`: only call `nodes(input$graph) <-` when
        `!identical(nodes(input$graph), rownames(x@coords))`.
    -   `makeHexGrid()` and `makeSquareGrid()`: build with
        `.graphFromTable(node.ids, from, to, w = rep(1, length(from)))`
        using both directions of each edge. **Keep the stored default
        weights of 1**: `hasCosts()`, `dropDeadEdges()` and
        `combineCosts()` look at `length(@edgeData@data)`, so a grid
        without stored weights would behave differently (e.g.
        `combineCosts()` would refuse it).
    -   While there: `makeSquareGrid()` fails for grids of 100,000
        nodes or more (see section 7); building from integer positions
        fixes it.
-   **Tests**: existing `test_makeHexGrid.R` / `test_makeSquareGrid.R`
    pass; compare `edges()`/`edgeWeights()` with the previous
    implementation on small grids.

------------------------------------------------------------------------

## 5. Phase 2: medium and low impact

### P2.1 `plotEdges()` and `plot(edges = TRUE)`

-   **Where**: `R/plot_gGraph.R:301-405`.
-   **Problem** (profile, worldgraph.40k: 1.0–2.0 s; rawgraph.40k
    2.2 s): `hasCosts()` and `getCosts(res.type = "vector", unique =
    TRUE)` both call `edgeWeights()` (\~0.4 s each); `getCosts(unique)`
    then `strsplit()`s 59k–246k names; `getEdges(unique = TRUE)` and
    the `%in% rownames(keptCoords)` tests work on strings.
-   **Fix**: work on one `.edgeTable(g)`: `keep <- et$from < et$to &
    in.area[et$from] & in.area[et$to]` with `in.area` a logical vector
    from `isInArea(res.type = "logical")`; weights are `et$w[keep]`;
    coordinates by integer index. `useCosts` default from the table
    (`length(unique(et$w)) > 1`, plus the "no stored data" check of
    `hasCosts()`), without a second `edgeWeights()`.
-   ⚠ **Order/orientation**: today "unique" edges are chosen by
    *string* comparison of node names (`"10" < "9"`), so the drawn
    orientation and order of segments differ from an integer
    `from < to`. Drawing is unaffected (a segment is the same both
    ways; lwd/lty follow their edge as long as edges and weights come
    from the same table), but the code must not mix the string-based
    `getEdges()`/`getCosts()` with the integer table. Do not change
    `getEdges()`/`getCosts()` themselves here (P3.4).
-   Also in `plot()` (`R/plot_gGraph.R:139-180`): the visible nodes and
    their colours are computed twice when a shape is drawn; compute
    them once after the background is plotted (behaviour identical:
    the second computation is the one used).

### P2.2 ggplot edge layer

-   **Where**: `.dfToSfLines()` (`R/plot_ggplot.R:272-283`),
    `.ggraphEdgesDf()`.
-   **Problem**: one `sf::st_linestring()` call per edge: 3.4 s for
    rawgraph.40k's 122,880 edges; `ggplot_build()` of the edge layer
    4.8–6.5 s in total.
-   **Fix** (identical geometry, verified with `identical()`): build
    each 2×2 matrix and set the class directly:
    `structure(matrix(c(x, xend, y, yend), 2, 2), class = c("XY",
    "LINESTRING", "sfg"))`, then one `sf::st_sfc(..., crs = 4326)`:
    3.4 → 1.6 s. `.ggraphEdgesDf()` should use integer indices from
    `.edgeTable()` (see P2.1 for the orientation caveat:
    `.ggraphEdgesDf()` output is internal, but its `from`/`to` columns
    end up in the layer data).
-   The test `test_plot_ggplot.R:40-42` requires `LINESTRING`s and one
    row per edge: keep both. Going further (one `MULTILINESTRING`, or
    `geom_segment()` instead of `geom_sf()`) would change the layer
    structure: only with the maintainer's agreement.

### P2.3 `.gpathDf()` and `.dfToSfPaths()`

-   **Where**: `R/plot_ggplot.R:249-298`.
-   **Fix**: build the data frame in one go: `nd <- lapply(gp, "[[",
    "path_detail")`, keep paths with at least 2 nodes, `path_id =
    rep.int(which(keep), L)`, `order = sequence(L)`, coordinates from
    `xy[unlist(nd), ]` (with `unname()`). Verified equal (row names
    aside: check what the current `rbind()` produces and match it), 0.28
    → 0.004 s for 611 paths. In `.dfToSfPaths()`, split the coordinate
    matrix (not the data frame) by `path_id`.

### P2.4 `plot.gPath()`

-   **Where**: `R/plot_gPath.R:33-63`.
-   **Problem**: one `geo.segments()` (and thus one `par("usr")` and one
    `segments()` call) per path: 0.6–0.7 s for 1,275 paths, linear in
    the number of paths.
-   **Fix**: build all segments of all paths at once (consecutive node
    pairs of each path, by integer index into `attr(x, "xy")`), repeat
    `col[i]`/`lwd[i]` per segment, and call `geo.segments()` once (it
    already handles per-segment `col`/`lwd`/`lty` since fix 1.11 of the
    code review).
-   Drawing order changes slightly: antimeridian-crossing segments of
    all paths are drawn after all other segments, instead of path by
    path. Mention it in NEWS; check `test_plot.gPath.R` and
    `test_geo.segments.R`.

### P2.5 `areNeighbours()` and `setEdges()`

-   **Where**: `R/areNeighbours.R:38-49`, `R/setEdges.R:61-85`.
-   `areNeighbours()`: replace `mapply()` over pairs (each doing
    `edg[[A]]`, a linear name scan) with numeric keys (prototype
    `areNeighbours_core()` in `proto_edgeTable.R`). Unknown nodes keep
    returning `FALSE`. Verified identical, 0.74 → 0.04 s for 5,000
    pairs.
-   `setEdges(remove = )`: `graph::removeEdge()` is fine for a few
    edges (0.1 s) but takes 4.3 s for 5,000. Above a threshold (e.g.
    100 edges) filter the edge table by keys (both directions) and
    rebuild with `.graphFromTable()`: verified identical edges and
    weights, 4.3 → 1.1 s. Same idea for `add =` (3.4 s for 5,000 edges
    with `addEdge()`): append rows for both directions, weights from
    `costs`; check what `addEdge()` does with edges that already exist
    and with self-loops, and reproduce it.

### P2.6 `hasCosts()`

-   **Where**: `R/hasCosts.R:31-38`.
-   **Fix**: read the weights straight from the store,
    `vapply(g@edgeData@data, function(e) e$weight, numeric(1))`, which
    avoids `edgeWeights()` (0.31 → 0.04 s). Only valid when every edge
    has a stored weight: if `length(g@edgeData@data)` differs from the
    number of directed edges, fall back to the current code (missing
    entries default to 1 in `edgeWeights()`).

### P2.7 `polygonBetween()` outlines

-   **Where**: `R/polygonBetween.R:70-76`.
-   **Fix**: one pass over the directed edge list: a polygon node is on
    the outline if it has a neighbour outside the polygon:
    `E <- getEdges(g, "matNames"); out <- E[, 1] %in% P & !(E[, 2] %in%
    P); P[P %in% E[out, 1]]`. Verified identical (keeps `P`'s order;
    nodes without neighbours are not outline nodes, as now), 0.52 →
    0.23 s for Africa on rawgraph.40k. Most of `polygonBetween()`'s
    time is the paths: P1.1.

### P2.8 `buffer()`

-   **Where**: `R/buffer.R:87-108`.
-   **Fix**: same breadth-first search, but on an integer adjacency
    list (`split(et$to, et$from)`) with a logical `visited` vector
    instead of `setdiff()` against a growing vector of names. Compute
    the distances exactly as now (same `fields::rdist.earth()` call on
    the same subsets, so boundary cases cannot flip) and return names
    in the same order. Verified identical, 0.26 → 0.08 s (worldgraph,
    5,000 km), 0.24 → 0.10 s (rawgraph, 3,000 km). Low priority.

### P2.9 `connectivityPlot()`

-   **Where**: `R/connectivityPlot.R:51-54` and `:97-117`.
-   **Fix**: compute a membership vector once (`set.id <- rep(NA,
    N); set.id[match(set_k, nodes)] <- k`) and index colours by it,
    instead of one named assignment per set (O(N × sets)). Small gain
    on the bundled graphs (0.3 s total); matters for graphs with
    thousands of islands.

### P2.10 `makeHexGrid()` sample points

-   **Where**: `R/makeHexGrid.R:78-88`, `.makeSamplePoints()`.
-   **Problem**: the box is sampled on a regular lon/lat grid with step
    `cell.size / 333` *degrees*, i.e. \~1/3 of a cell at the equator but
    much denser at high latitude (at 60°N a degree of longitude is half
    as long). 2.07M points for Europe at 9.7 km cells: 25 s in
    `dggridR::dgGEO_to_SEQNUM()`.
-   **Fix**: keep the latitude step, but make the longitude step depend
    on latitude so that the spacing *in km* is the same as today at the
    equator: `lon.step = lat.step / cos(lat)` (capped near the poles),
    one row of points per latitude. Every cell is still hit by at least
    as many points (in km) as the current scheme guarantees at the
    equator, so interior cells cannot be missed. For Europe (35–70°N)
    this roughly halves the number of points.
-   ⚠ Partial cells along the edge of the box may differ (a cell that
    was only touched by an extra sample point may be dropped). Verify on
    several boxes (equator, Europe, polar, crossing the antimeridian)
    whether the node set is identical; if it is not, either keep the
    current sampling or get the maintainer's agreement.

### P2.11 `gData` / validity / constructor calls (minor)

-   `dijkstraFrom()` gData method: one `.connectedSets()` instead of
    two (see P1.1).
-   `closestNode()` recomputes the unit-sphere coordinates of all nodes
    at every call (`R/closestNode.R:167`, the "cache" is a local copy,
    see code review 5.9): \~5 ms on 40k nodes, not worth a cache that
    `[` would have to invalidate. Leave it, but remove the misleading
    `x@meta$.xyz` logic if touching the file.

### P2.12 `getEdges(res.type = "matId")`

-   **Where**: `R/accessors.R:301-307`. `unlist(res)` on the `edgeL`
    list of lists works only because each element has a single
    `edges` component; use `.edgeTable()` to make it explicit (no
    speed change, 0.1 s).

------------------------------------------------------------------------

## 6. Phase 3: needs a decision (do not implement without approval)

### P3.1 Faster all-pairs: avoid one `dijkstra.sp()` per source

After P1.1, `dijkstraBetween()` on n locations costs n − 1 calls to
`RBGL::dijkstra.sp()`, each recomputing `edgeMatrix()` (28 ms of 37 ms
on a 40k graph): 500 locations ≈ 18 s. Options:

-   add `igraph` (Suggests or Imports) and compute distances for all
    sources in one C call (`igraph::distances()`), paths with
    `igraph::shortest_paths()`. ⚠ For equal-cost paths (very common on
    grids with uniform costs) igraph may return a *different* path of
    the same length, so `path_detail` would change while `length` and
    `gPath2dist()` would not;
-   or add a distances-only function (e.g. `dijkstraDist()`) that never
    builds paths, for the common case where only `gPath2dist()` is used
    afterwards.

### P3.2 `assignByRaster()` on great-circle distances ⚠

Use the same unit-sphere `FNN` search as `closestNode()` instead of
planar degrees (code review 5.7). Same speed as P1.4, more accurate, but
**12% of the cells of a Europe raster (35–70°N) change node**. Changes
results.

### P3.3 `setCosts(method = "function")` with an asymmetric `FUN` ⚠

Verified: today the edge `i -> j` gets `FUN(cost_j, cost_i)` (the
weights are swapped by `edgeData<-` in undirected mode, code review
5.10). P1.6 preserves this on purpose. Fixing it is one argument in
`.graphFromTable()`, but changes results for asymmetric functions.

### P3.4 `getEdges(unique = TRUE)` / `getCosts(unique = TRUE)` on integer positions ⚠

They choose one direction per edge by *string* comparison of node names
(`"10" < "9"`) and split names on `"."` (code review 5.10). Integer
comparison would be faster (0.62 s → \<0.1 s for both on worldgraph.40k)
and correct for any node names, but changes the orientation and order
of the returned rows; `getEdges()` and `getCosts()` must change together
(they are used in pairs, e.g. by `plotEdges()`).

### P3.5 Faster `edgeWeights()`

`edgeWeights()` costs 0.4–0.9 s on 40k graphs and is called by many
functions. Reading the store by key
(`g@edgeData@data[paste(from, to, sep = "|")]`) is 3× faster
(0.87 → 0.28 s, identical), but relies on `graph` internals. Only worth
it once Phases 1–2 have removed the redundant calls.

------------------------------------------------------------------------

## 7. Bugs found while benchmarking (not performance; fix separately)

1.  **`makeSquareGrid()` fails for 100,000 nodes or more**:
    `makeSquareGrid(n.lon = 400, n.lat = 250)` errors with "node names
    in 'ft' must be in 'V'". The `from`/`to` matrix is double, and
    `as.character(1e5)` is `"1e+05"`, not `"100000"`
    (`R/makeSquareGrid.R:150-159`). Fix: `storage.mode(ft) <-
    "integer"` (or P1.7). Add a test with \> 100,000 nodes (or
    reproduce with a smaller check of the conversion).
2.  `makeHexGrid(spacing = 15)` returns 9.7 km cells (136k nodes for
    Europe) because `dgconstruct(resround = "down")` picks the coarsest
    resolution whose spacing is *below* the request (res 11 would be
    \~16.8 km). Possibly intended; worth documenting in `@param
    spacing`, since it can multiply the grid size by \~3.
3.  `RBGL::sp.between()` returns `path_detail = NA` (with a warning
    from `as.numeric("NA")`) for unreachable targets after looping over
    all nodes; geoGraph checks connectivity first, so it is only
    reachable through direct use. P1.1 keeps the `NA` result.

------------------------------------------------------------------------

## Appendix A. Baseline (`Rscript data-raw/speed/benchmark.R`, `dev` at `9f4fce4`)

4-core container, R 4.3.3, graph 1.80.0, RBGL 1.78.0, sf 1.0-15, terra
1.7-65, dggridR 3.1.1. Timings vary by \~±20% between runs.

```         
costs      setCosts(worldgraph.40k, mean)                                0.896 s
costs      setCosts(rawgraph.40k, node.values)                           1.954 s
costs      setDistCosts(rawgraph.40k)                                    1.647 s
costs      dropDeadEdges(worldgraph.40k, 10)                             1.274 s
costs      combineCosts(worldgraph.40k, worldgraph.40k)                  1.529 s
costs      hasCosts(worldgraph.40k)                                      0.326 s
costs      getCosts(vector, unique)                                      0.401 s
costs      getCosts(asIs, unique)                                        5.838 s
costs      getEdges(matNames, unique)                                    0.106 s
subset     worldgraph.40k[Europe]                                        0.800 s
subset     worldgraph.40k[every other node]                              4.471 s
subset     dropDeadNodes(worldgraph.40k)                                 6.918 s
subset     keepMaxConnectedSet(worldgraph.40k)                          11.856 s
subset     areNeighbours(5000 pairs)                                     0.738 s
subset     setEdges(remove = 5000 edges)                                 4.599 s
subset     setEdges(add = ~5000 edges)                                   3.123 s
dijkstra   dijkstraBetween(hgdp) [51 locations, 1275 paths]              5.454 s
dijkstra   dijkstraFrom(hgdp, Addis Ababa)                               0.835 s
dijkstra   dijkstraBuffer(worldgraph.40k, d = 5000)                      0.308 s
dijkstra   polygonBetween(France, Spain) [209 paths]                     1.625 s
buffer     buffer(worldgraph.40k, 1 node, d = 5000 km)                   0.216 s
buffer     buffer(rawgraph.40k, 51 nodes, d = 300 km)                    0.866 s
buffer     closestNode(1e5 locations)                                    0.158 s
raster     assignByRaster(175k cells)                                    1.708 s
raster     findLand(worldgraph.40k)                                      0.300 s
raster     assignByPolygon(worldgraph.40k)                               0.406 s
grid       makeHexGrid(Europe, spacing = 30) [15k nodes]                 4.540 s
grid       makeSquareGrid(size = 90000)                                  6.054 s
plot       plot(worldgraph.40k, reset = TRUE)                            0.192 s
plot       plot(worldgraph.40k, edges = TRUE, reset = TRUE)              0.949 s
plot       plotEdges(rawgraph.40k)                                       2.218 s
plot       plot(gPath with 1275 paths)                                   0.684 s
plot       connectivityPlot(worldgraph.40k)                              0.361 s
ggplot     ggplot_build(autoplot(worldgraph.40k))                        2.045 s
ggplot     ggplot_build(geom_ggraph(rawgraph.40k, edges))                4.806 s
ggplot     .gpathDf + .dfToSfPaths(1275 paths)                           0.749 s
```

Slow cases (`--slow`, measured separately): `dijkstraFrom()` on the
\~10.5k-node connected Afro-Eurasian component of worldgraph.40k
\~28 s (extrapolated from 8,000 paths = 21.5 s);
`assignByRaster()` on a 2.3M-cell world raster 39.5 s;
`makeHexGrid(Europe, spacing = 15)` (136k nodes) 39–40 s.

## Appendix B. Suggested order of commits

1.  P0 helpers + tests.
2.  P1.2 (`[`), then P1.3 (`keepMaxConnectedSet`, `dropDeadNodes`).
3.  P1.1 (`.shortestPaths`) + switch `dijkstraFrom`/`dijkstraBetween`.
4.  P1.6 (cost setters), P1.7 (grids + `makeSquareGrid` bug), P1.5.
5.  P1.4 (`assignByRaster`).
6.  P2.1–P2.9, P2.12 in any order; P2.10 last (needs checking on
    several boxes).
7.  NEWS, re-run `benchmark.R`, update Appendix A with an "after"
    column, R CMD check.
