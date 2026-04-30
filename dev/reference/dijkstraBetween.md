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

  A character vector of ending node IDs.

## Value

A gPath object (TODO link with a full description of gPath).

## Details

The function uses the RBGL package to compute the shortest paths. It
checks for the connectivity of the graph and handles cases where there
are duplicated paths.

## Functions

- `dijkstraBetween(gGraph)`: Method for gGraph

- `dijkstraBetween(gData)`: Method for gData

## See also

Other dijkstra_methods:
[`dijkstraFrom()`](https://evolecolgroup.github.io/geograph/dev/reference/dijkstraFrom.md)

## Examples

``` r
## select a few populations from the HGDP dataset
hgdp.sub <- hgdp[getData(hgdp)$Population %in%
  c("French", "Balochi", "BantuKenya", "Papuan", "Pima")]
hgdp.path <- dijkstraBetween(hgdp.sub) # compute shortest path
#> Loading required package: RBGL
plot(hgdp.sub)
plot(hgdp.path)
```
