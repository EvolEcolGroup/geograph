# node buffer

This function identifies all nodes that are reachable from a given
origin node within a specified maximum distance, where the distance is
defined as the cumulative least-cost path over weighted edges (e.g.
topographic cost).

## Usage

``` r
nodeBuffer(graph, origin, max.distance, map.distances = TRUE)
```

## Arguments

- graph:

  An `igraph` object representing the spatial graph.

- origin:

  Either a character string naming a node, or a numeric vector / list /
  data.frame of length 2 giving longitude and latitude.

- max.distance:

  Numeric. Maximum cumulative cost the feature is assumed to be able to
  diffuse.

- map.distances:

  Logical. If `TRUE`, return the Graph object with new a node attribute
  called 'diffusion_area' indicating the diffusion area with TRUE for
  all nodes reachable in order to map it on the gGraph object. If
  `FALSE` (default), return only the vector of node IDs within the
  diffusion area.

## Value

If `map.distances = FALSE`, a character vector of node IDs reachable
within `max.distance`.

If `map.distances = TRUE`, the input `gGraph` object with an added
logical node attribute `diffusion_area`.

## Details

Internally, the function computes single-source shortest paths (Dijkstra
algorithm) from the origin node and returns all nodes whose minimum path
cost does not exceed the specified threshold.

## See also

[`dijkstraFrom`](https://evolecolgroup.github.io/geograph/dev/reference/dijkstraFrom.md),
[`gPath2dist`](https://evolecolgroup.github.io/geograph/dev/reference/gPath2dist.md)

## Examples

``` r
# create a small graph over Europe
geo.box <- c(xmin = -10, xmax = 30, ymin = 35, ymax = 60)
ggraph <- createNewGraph(geo.box, spacing = 1000)
#> Resolution: 4, Area (km^2): 629710.644103813, Spacing (km): 783.739159045648, CLS (km): 895.60184164835
# set uniform edge costs (required before running nodeBuffer)
ggraph <- setCosts(ggraph, node.values = rep(10, length(getNodes(ggraph))))

# get all nodes reachable within a cost of 15 from node "1"
reachable <- nodeBuffer(ggraph, origin = getNodes(ggraph)[1],
                       max.distance = 15, map.distances = FALSE)
# same but mapped back onto the graph as a node attribute
ggraph <- nodeBuffer(ggraph, origin = getNodes(ggraph)[1],
                    max.distance = 15, map.distances = TRUE)
# use a spatial origin instead of a node ID
ggraph <- nodeBuffer(ggraph, origin = data.frame(lon = 10, lat = 47),
                    max.distance = 15, map.distances = TRUE)
```
