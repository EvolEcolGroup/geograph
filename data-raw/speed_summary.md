# Speed branch: what was done and what was decided

Summary of the implementation of `data-raw/speed_plan.md` on the `speed`
branch (November 2026). The plan says *what* to change and why; this file
records what was actually done, the decisions taken on the way, and what is
left for the maintainer. Section 8 and Appendix A of the plan contain the same
facts in more detail.

-   **Base**: `6bc2b91` (`dev` at `9f4fce4` plus the plan).
-   **Work**: 21 commits: 19 for plan items (one each), then one commit of
    lint fixes for the tests and one for NEWS and the plan. Nothing has been
    pushed.
-   **Size**: 24 files in `R/` (+572 / −279 lines) and 21 test files
    (+1,000 lines, all additions).
-   **Results**: identical to the previous code, except for the two
    behaviour changes the plan anticipated (section 5 below).

------------------------------------------------------------------------

## 1. How the work was done

The rules of section 2 of the plan were followed:

-   **One item per commit**: each commit carries its own tests and its
    before/after benchmark lines in the commit message.
-   **Identical outputs**: every rewritten function is checked against a
    reference with `identical()` (or with `expect_identical()` on its
    parts), on small and medium graphs.
    -   For graph rebuilds, the comparison covers `nodes()`, `edges()`,
        `edgeWeights()`, `edgemode()` and the stored edge data, never the
        `graphNEL` objects themselves.
    -   Where the old implementation is the only reference, a copy of it
        is kept in `tests/testthat/helper-reference.R`: 20 reference
        functions plus 4 comparison helpers, used only by the tests.
    -   Shortest paths are compared with `RBGL::sp.between()` itself.
    -   Nearest-node search is compared with `sf::st_nearest_feature()`.
-   **Dependencies**: no new dependencies, and no `.Call()` into RBGL's
    compiled code.
-   **Compatibility**: R ≥ 3.5 syntax only, and no change to exported
    signatures, return types, names or ordering.
-   **Phase 3 was not touched**, as the plan requires the maintainer's
    approval for it.

------------------------------------------------------------------------

## 2. Shared helpers (`R/utils-graph.R`, new)

Most items rest on a few internal helpers. They work on node *positions*
(integers) instead of node names:

| Helper | Role |
|---|---|
| `.edgeTable(g)` | Edges as integer `from`/`to` positions plus weights, read straight from `@edgeL` and `@edgeData`. |
| `.graphFromTable(nodes, from, to, w)` | Builds a `graphNEL` from a flat edge list, writing each weight once. Weights are assigned while the graph is temporarily directed (setting `@graphData$edgemode` directly, because `edgemode<-` resets `edgeRenderInfo`). |
| `.reverseWeights()`, `.nestedWeights()` | Reproduce two quirks of the nested `graphNEL` constructor, so rebuilt graphs stay identical (decision 1 in section 3). |
| `.subsetGraph(g, idx)` | Replaces `graph::subGraph()` for `[`. |
| `.shortestPaths(g, start, finish)` | Replaces `RBGL::sp.between()`: one `dijkstra.sp()` per unique start, with vectorised traceback. |
| `.addEdges()`, `.removeEdges()` | Emulate `graph::addEdge()` and `removeEdge()` by position. |

Numeric edge keys `(from - 1) * N + to` replace `"from|to"` string lookups
wherever pairs of nodes are matched.

------------------------------------------------------------------------

## 3. Commits

| Commit | Item | Change | Before → after |
|---|---|---|---|
| `1e1f078` | P0 | Edge-table helpers and graph builder. | — |
| `5620c7a` | P1.2 | `[` on `gGraph` uses `.subsetGraph()`. It falls back to `subGraph()` for `NA` or duplicated indices. | every other node 3.0 → 0.3 s |
| `9e3cc8d` | P1.3 | `keepMaxConnectedSet()` subsets to the largest set; `dropDeadNodes()` keeps nodes that have edges. | 8.2 → 0.5 s; 4.5 → 0.3 s |
| `a71b916` | P1.1 | `dijkstraFrom()`, `dijkstraBetween()` and `polygonBetween()` use `.shortestPaths()`. | `dijkstraBetween(hgdp)` 3.2 → 1.7 s |
| `8e50615` | P2.11 | `dijkstraFrom()` on a `gData` computes the connected sets once. | — |
| `1dcd576` | P1.6 | `setCosts()`, `dropDeadEdges()`, `combineCosts()` and `setDistCosts()` work on the edge table. | 1.5–2.1× |
| `1f92385` | P1.7 | Grids are built from edge tables; `new("gGraph")` skips renaming nodes that are already named correctly. Fixes `makeSquareGrid()` for 100,000 or more nodes. | `makeSquareGrid(90000)` 2.3 → 1.5 s |
| `2cd0660` | P1.5 | `getCosts(asIs, unique = TRUE)` no longer looks up nodes by name. | 3.1 → 0.2 s |
| `9aa0bfc` | P1.4 | `assignByRaster()` uses an `FNN` k-d tree and plain vectors. `tidyr` and `tibble` are dropped from Imports. | 2.3M cells 14.5 → 2.2 s |
| `b0278c3` | P2.5 | `areNeighbours()` uses numeric keys; `setEdges()` adds and removes edges by position. | remove 5,000 edges 2.1 → 0.08 s |
| `aee27e5` | P2.6 | `hasCosts()` reads the stored weights directly. | 0.2 → 0.03 s |
| `2077904` | P2.1 | `plotEdges()` and `plot(edges = TRUE)` select edges from one table. | 0.85 → 0.5 s |
| `b0b3f1d` | P2.2 | The ggplot edge layer builds segments without `st_linestring()` per edge. | 2.6 → 2.1 s |
| `062feb9` | P2.3 | `geom_gpath()` builds the data of all paths at once. | 0.31 → 0.05 s |
| `8af9326` | P2.4 | `plot()` on a `gPath` draws all segments in one call. | 0.25 → 0.03 s |
| `f8329cb` | P2.7 | `polygonBetween()` finds the outline nodes in one pass. | — |
| `87394ba` | P2.8 | `buffer()` searches on positions with a logical `visited` vector. | 0.19 → 0.05 s |
| `f41c9c9` | P2.9 | `connectivityPlot()` colours nodes with one lookup. | 3.2 → 0.1 s (2,000 sets) |
| `32e37d1` | P2.12 | `getEdges(matId)` reads edge positions explicitly. | — |
| `d7b6328` | — | Lint fixes in the new tests. | — |
| `aeea907` | — | NEWS, plus section 8 and Appendix A of the plan. | — |

Timings are from one back-to-back run of `data-raw/speed/benchmark.R --slow`
on the implementation machine (24 cores, R 4.5.2, graph 1.88.1, RBGL 1.86.0,
sf 1.1.3, terra 1.9.50). They vary by about ±30% between runs. The full table
is in Appendix A of the plan.

------------------------------------------------------------------------

## 4. Decisions taken during implementation

These are the points where the plan left room or turned out to be wrong.
Each one was resolved in favour of identical results.

1.  **Asymmetric weights stay swapped in every rebuild, not only in
    `setCosts(method = "function")`.**
    -   *Finding*: the nested `graphNEL` constructor stores, for each
        edge, the weight given for its *reverse*. This also affects
        `dropDeadEdges()` and `combineCosts()`. The constructor also
        turns integer weights into doubles when some node has no edge,
        and refuses non-numeric weights.
    -   *Decision*: `.reverseWeights()` and `.nestedWeights()` reproduce
        all of this, so results are unchanged.
    -   *Still open*: the real fix is P3.3, which needs a decision.
2.  **Edges keep their string-based direction in plots.**
    -   *Decision*: P2.1 and P2.2 keep today's choice of direction for
        each edge, computed on the integer table. This is the same
        string comparison of node names that `getEdges(unique = TRUE)`
        makes.
    -   *Effect*: segment order and orientation are unchanged, so these
        items need no ⚠ warning. Switching to integer comparison is part
        of P3.4.
3.  **`setEdges()` emulates `addEdge()` and `removeEdge()` instead of
    rebuilding the graph.**
    -   *Reason*: a rebuild would change the stored edge data of graphs
        without costs (edges added there get a default weight of `1L`
        and partial edge data). It would also change the order of the
        edge lists.
    -   *Effect*: the emulation keeps all of this identical and is still
        about 10–28× faster.
4.  **`[` falls back to `graph::subGraph()` for `NA` or duplicated
    indices**, so those edge cases (including their errors) behave
    exactly as before.
5.  **`keepMaxConnectedSet()` keeps two old behaviours**: an edgeless graph
    gives an empty graph, and a graph without stored costs comes back with
    stored weights of 1.
6.  **`getEdges(matId)` does not use `.edgeTable()`.** The bundled graphs
    store positions as doubles, and `.edgeTable()` would coerce them to
    integers. The `matId` matrix must keep the stored type to stay
    identical.
7.  **`closestNode()` was left alone**, as P2.11 suggested. Its cost is
    about 5 ms, not worth a cache that `[` would have to invalidate.
8.  **`tidyr` and `tibble` were removed from Imports.** After P1.4 nothing
    in the package used them. `FNN` was already imported, for
    `closestNode()`.

------------------------------------------------------------------------

## 5. Behaviour changes users can see

The plan anticipated both of these changes. Both are in NEWS, and the first
is also in `?assignByRaster`.

-   **`assignByRaster()` ties.**
    -   A cell whose centre is exactly equidistant from several nodes can
        now go to a different one of these nodes. The choice was
        arbitrary before too.
    -   *Example*: a 1° raster centred halfway between the nodes of a 1°
        square grid has 81 of 100 cells assigned differently.
    -   No difference was found at other resolutions, and the values
        match `sf::st_nearest_feature()` otherwise.
    -   A test checks that every cell still goes to one of its nearest
        nodes.
-   **`plot()` on a `gPath`.** Segments crossing the antimeridian are now
    drawn after all other segments, instead of path by path. Only the
    drawing order changes.

### Bug fixed

-   `makeSquareGrid()` failed for 100,000 or more nodes with "node names in
    'ft' must be in 'V'", because `as.character(1e5)` is `"1e+05"`. This is
    bug 1 of section 7 of the plan, fixed by P1.7.

------------------------------------------------------------------------

## 6. Not done, and why

-   **P2.10, latitude-dependent sampling in `makeHexGrid()`: not
    implemented.**
    -   *Test*: 10 combinations of box and spacing.
    -   *Result*: the node set changed in 6 of them, by 1–6 cells along
        the box edges. The table is in section 8 of the plan.
    -   *Why not*: the plan says to keep the current sampling unless the
        maintainer agrees.
    -   *What it would give*: roughly half the sample points for Europe.
-   **Phase 3 (P3.1–P3.5)** is untouched, because each item needs a
    decision.
-   **Bug 2 of section 7 (documentation of `makeHexGrid(spacing)`)** is
    still open; the plan said to fix it separately.
-   **Bug 3 of section 7** no longer matters: `.shortestPaths()` returns
    `NA` for unreachable targets without the warning `sp.between()` gave.

------------------------------------------------------------------------

## 7. Where the plan's estimates were off

-   **`makeHexGrid()` (P1.7)**: building the graph of the 136k-node grid
    went from 3.2 s to 2.0 s. The whole call still takes about 18 s,
    because almost all of its time is spent in `dggridR`.
-   **`sp.between()`** took 4–8 s on this machine for the 10.5k-path case,
    not the 28 s in the plan. The new code takes 2–2.3 s.
-   **Benchmark noise** was about ±30% on this machine. Functions that were
    not changed (`dijkstraBuffer()`, `closestNode()`, `findLand()`,
    `assignByPolygon()`) show this noise in the table. Medians of 5 runs
    were used to confirm that they are unchanged.
-   **NEWS corrections**: two NEWS entries were first written with wrong
    timings (P1.2 inverted, P1.7 overstated from a single noisy run). Both
    were corrected in `aeea907`.

------------------------------------------------------------------------

## 8. Verification

On the final commit:

-   **Tests**: `testthat::test_local(".")` runs 4,216 expectations with
    0 failures, 0 skips and 0 errors (3,639 on `dev`). The new test files are
    `test_utils_graph.R` and `test_shortestPaths.R`, and equivalence tests
    were added to the existing test files of every changed function.
-   **Package check**: `R CMD check --no-manual` returns Status OK.
-   **Lint**: `lintr::lint_package()` reports no new lints. The
    `object_usage_linter` hits seen at first came from lintr reading the
    older installed geoGraph; they disappear once this branch is
    installed.
-   **Prototypes**: `data-raw/speed/verify_prototypes.R` prints `TRUE` on
    every line.

------------------------------------------------------------------------

## 9. Next steps for the maintainer

1.  **Review the branch.** Start with `R/utils-graph.R` and
    `tests/testthat/helper-reference.R`; the other changes are mostly
    switches to these helpers.
2.  **Decide on P2.10**: accept small changes in the edge cells of
    `makeHexGrid()`, or keep the current sampling.
3.  **Decide on the Phase 3 items.**
    -   P3.3 (asymmetric weights) and P3.4 (string direction) are the
        two quirks the branch deliberately preserves.
    -   P3.1 matters for all-pairs distances on many locations.
4.  **Document `makeHexGrid(spacing)`'s rounding** (bug 2).
5.  **Push `speed` and open a pull request** when ready.
