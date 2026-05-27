# Find nodes reachable within a cost threshold

The function `dijkstraBuffer` identifies all nodes reachable from a
given origin within a specified maximum cumulative cost, where the
distance is defined as the cumulative least-cost path over weighted
edges.

## Usage

``` r
dijkstraBuffer(x, origin, d, res.type = c("nodes", "gGraph"), ...)
```

## Arguments

- x:

  a valid
  [`gGraph`](https://evolecolgroup.github.io/geograph/dev/reference/gGraph-class.md)
  object with edge costs defined (see
  [`setCosts`](https://evolecolgroup.github.io/geograph/dev/reference/setCosts.md)).

- origin:

  either a character string naming a node in `x`, or a `data.frame`,
  list, or numeric vector of length 2 giving longitude and latitude of
  the origin location.

- d:

  numeric. Maximum cumulative cost defining the reachable area.

- res.type:

  a character string indicating the output format:

  - `"nodes"`: a character vector of reachable node names (default).

  - `"gGraph"`: the input
    [`gGraph`](https://evolecolgroup.github.io/geograph/dev/reference/gGraph-class.md)
    with a new logical node attribute `reachable` indicating which nodes
    fall within the cost threshold.

- ...:

  further arguments passed to other methods (currently unused).

## Value

A character vector of reachable node names when `res.type = "nodes"`, or
a
[`gGraph`](https://evolecolgroup.github.io/geograph/dev/reference/gGraph-class.md)
object with a new logical node attribute `reachable` when
`res.type = "gGraph"`.

## Details

Internally, the function computes single-source shortest paths using
Dijkstra's algorithm from the origin node and returns all nodes whose
minimum path cost does not exceed the specified threshold.

## See also

[`dijkstraFrom`](https://evolecolgroup.github.io/geograph/dev/reference/dijkstraFrom.md)
and
[`gPath2dist`](https://evolecolgroup.github.io/geograph/dev/reference/gPath2dist.md)
for the underlying path computations.
[`buffer`](https://evolecolgroup.github.io/geograph/dev/reference/buffer.md)
for geographic distance-based buffers.
[`setCosts`](https://evolecolgroup.github.io/geograph/dev/reference/setCosts.md)
to define edge costs before running `dijkstraBuffer`.

Other dijkstra_methods:
[`dijkstraBetween()`](https://evolecolgroup.github.io/geograph/dev/reference/dijkstraBetween.md),
[`dijkstraFrom()`](https://evolecolgroup.github.io/geograph/dev/reference/dijkstraFrom.md),
[`gPath2dist()`](https://evolecolgroup.github.io/geograph/dev/reference/gPath2dist.md)

## Examples

``` r
## get all nodes reachable within cost 10 from node the origin (here Zurich)
zurich <- data.frame(lon = 8.55, lat = 47.37)

## set costs for the graph and calculate the buffer
graph <- setCosts(rawgraph.10k, attr.name = "habitat", method = "mean")
x2 <- dijkstraBuffer(graph, origin = zurich, d = 10, res.type = "gGraph")

## plot reachable nodes in dark blue, all others transparent
col.rules <- data.frame(
  reachable = c(TRUE, FALSE),
  color     = c("darkblue", "transparent")
)
plot(x2, col.rules = col.rules, reset = TRUE)

```
