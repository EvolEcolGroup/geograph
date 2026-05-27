# Find the minimum cost path

This function finds the shortest path from a given 'source' node to all
other nodes in the graph using Dijkstra's algorithm. It can be applied
to both gGraph and gData objects.

## Usage

``` r
dijkstraFrom(x, start)

# S4 method for class 'gGraph'
dijkstraFrom(x, start)

# S4 method for class 'gData'
dijkstraFrom(x, start)
```

## Arguments

- x:

  A
  [`gGraph`](https://evolecolgroup.github.io/geograph/dev/reference/gGraph-class.md)
  or
  [`gData`](https://evolecolgroup.github.io/geograph/dev/reference/gData-class.md)
  object.

- start:

  a character string naming the 'source' node.

## Value

A gPath object (TODO link with a full description of gPath).

## Details

The function uses the RBGL package to compute the shortest paths. It
checks for the connectivity of the graph and handles cases where there
are duplicated paths.

## Functions

- `dijkstraFrom(gGraph)`: method for gGraph

- `dijkstraFrom(gData)`: method for gData

## See also

Other dijkstra_methods:
[`dijkstraBetween()`](https://evolecolgroup.github.io/geograph/dev/reference/dijkstraBetween.md),
[`dijkstraBuffer()`](https://evolecolgroup.github.io/geograph/dev/reference/dijkstraBuffer.md),
[`gPath2dist()`](https://evolecolgroup.github.io/geograph/dev/reference/gPath2dist.md)

## Examples

``` r
# Using a gData object:

# select a few populations from the HGDP dataset
hgdp.sub <- hgdp[getData(hgdp)$Population %in%
  c("Orcadian", "Adygei", "Russian", "Basque")]

# select a location of another HGDP population
french.hgdp <- hgdp[getData(hgdp)$Population %in%
  c("French")]

# Choose an origin node
french <- getNodes(french.hgdp)

my.path <- dijkstraFrom(hgdp.sub, french)
```
