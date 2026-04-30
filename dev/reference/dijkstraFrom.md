# Find the minimum cost path

minimum costs paths to nodes from a given 'source' node.

## Usage

``` r
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

## See also

Other dijkstra_methods:
[`dijkstraBetween()`](https://evolecolgroup.github.io/geograph/dev/reference/dijkstraBetween.md)

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
french <- french.hgdp@nodes.id

my.path <- dijkstraFrom(hgdp.sub, french)
```
