# Tests if location reachable from nodes

Tests if one location (actually, the closest node to it) is reachable
from the set of nodes of a
[gData](https://evolecolgroup.github.io/geograph/reference/gData-class.md)
object.

## Usage

``` r
isReachable(x, loc)
```

## Arguments

- x:

  a
  [gData](https://evolecolgroup.github.io/geograph/reference/gData-class.md)
  object.

- loc:

  location, specified as a list of two components giving respectively
  the longitude and the latitude. Alternatively, it can be a matrix-like
  object with one row and two columns.

## Value

a named boolean vector, with one element per node of the input
[gData](https://evolecolgroup.github.io/geograph/reference/gData-class.md)
object, and names corresponding to node names.

## See also

Other connectivity_functions:
[`areConnected()`](https://evolecolgroup.github.io/geograph/reference/areConnected.md),
[`areNeighbours()`](https://evolecolgroup.github.io/geograph/reference/areNeighbours.md),
[`isConnected,gData-method`](https://evolecolgroup.github.io/geograph/reference/isConnected.md)

## Examples

``` r
# Select African populations Mandenka, Yoruba, and San
hgdp.sub <- hgdp[getData(hgdp)$Population %in%
  c("Mandenka", "Yoruba", "San")]
# Get a location that is reachable
location <- getCoords(hgdp[getData(hgdp)$Population == "BantuKenya"])
# Check these are reachable
isReachable(x = hgdp.sub, loc = location)
#>  6433 15411 13518 
#>  TRUE  TRUE  TRUE 
```
