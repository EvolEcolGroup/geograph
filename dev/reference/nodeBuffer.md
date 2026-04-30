# node buffer

This function identifies all nodes that are reachable from a given
origin node within a specified maximum distance, where the distance is
defined as the cumulative least-cost path over weighted edges (e.g.
topographic cost).

## Usage

``` r
nodeBuffer(graph, origin, max_distance, map_distances = TRUE)
```

## Arguments

- graph:

  An `igraph` object representing the spatial graph.

- origin:

  Either a character string naming a node, or a numeric vector / list /
  data.frame of length 2 giving longitude and latitude.

- max_distance:

  Numeric. Maximum cumulative cost the feature is assumed to be able to
  diffuse.

- map_distances:

  Logical. If `TRUE`, return the Graph object with new a node attribute
  called 'difusion_area' indicating the diffusion area with TRUE for all
  nodes reachable in order to map it on the gGraph object. If `FALSE`
  (default), return only the vector of node IDs within the diffusion
  area.

## Value

If `map_distances = FALSE`, a character vector of node IDs reachable
within `max_distance`.

If `map_distances = TRUE`, the input `gGraph` object with an added
logical node attribute `diffusion_area`.

## Details

Internally, the function computes single-source shortest paths (Dijkstra
algorithm) from the origin node and returns all nodes whose minimum path
cost does not exceed the specified threshold.

## See also

[`dijkstraFrom`](https://evolecolgroup.github.io/geograph/dev/reference/dijkstraFrom.md),
[`gPath2dist`](https://evolecolgroup.github.io/geograph/dev/reference/dijkstra-methods.md)
