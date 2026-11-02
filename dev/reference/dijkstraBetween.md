# Find the shortest path between nodes in a graph

This function finds the shortest path between nodes in a graph using
Dijkstra's algorithm. It can be applied to both gGraph and gData
objects.

## Usage

``` r
dijkstraBetween(x, ...)

# S4 method for class 'gGraph'
dijkstraBetween(x, from, to)

# S4 method for class 'gData'
dijkstraBetween(x)
```

## Arguments

- x:

  A
  [`gGraph`](https://evolecolgroup.github.io/geograph/dev/reference/gGraph-class.md)
  or
  [`gData`](https://evolecolgroup.github.io/geograph/dev/reference/gData-class.md)
  object.

- ...:

  Additional arguments passed to other methods (currently not used).

- from:

  A character vector of starting node IDs.

- to:

  A character vector of ending node IDs. If missing, or identical to
  `from`, paths are computed between all pairs of nodes in `from`.

## Value

A
[`gPath`](https://evolecolgroup.github.io/geograph/dev/reference/gPath-class.md)
object:

- if `to` is missing or identical to `from` (and has more than one
  node), one path for each pair of nodes `from[i]`, `from[j]` with
  `i < j`, in the order used by
  [`dist`](https://rdrr.io/r/stats/dist.html) objects; this is always
  the case for the
  [`gData`](https://evolecolgroup.github.io/geograph/dev/reference/gData-class.md)
  method, which uses all locations of `x`;

- otherwise, one path from each node of `from` to each node of `to`
  (`length(from) * length(to)` paths, `from` varying slowest).

[`gPath2dist`](https://evolecolgroup.github.io/geograph/dev/reference/gPath2dist.md)
turns these into a [`dist`](https://rdrr.io/r/stats/dist.html) object or
a `length(from) x length(to)` matrix, respectively.

## Details

The function uses the RBGL package to compute the shortest paths. It
checks for the connectivity of the graph and handles cases where there
are duplicated paths.

## Functions

- `dijkstraBetween(gGraph)`: Method for gGraph

- `dijkstraBetween(gData)`: Method for gData

## See also

Other dijkstra_methods:
[`dijkstraBuffer()`](https://evolecolgroup.github.io/geograph/dev/reference/dijkstraBuffer.md),
[`dijkstraFrom()`](https://evolecolgroup.github.io/geograph/dev/reference/dijkstraFrom.md),
[`gPath2dist()`](https://evolecolgroup.github.io/geograph/dev/reference/gPath2dist.md)

## Examples

``` r
## select a few populations from the HGDP dataset
hgdp.sub <- hgdp[getData(hgdp)$Population %in%
  c("French", "Balochi", "BantuKenya", "Papuan", "Pima")]
hgdp.path <- dijkstraBetween(hgdp.sub) # compute shortest path
```
