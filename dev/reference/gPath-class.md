# Formal class "gPath"

The class `gPath` is an S3 class storing the results of shortest path
computations between nodes in a
[`gGraph`](https://evolecolgroup.github.io/geograph/dev/reference/gGraph-class.md)
object. Paths are computed using Dijkstra's algorithm via the RBGL
package and represent the minimum-cost routes connecting pairs of nodes
in the graph.

## Details

`gPath` objects are primarily created as outputs from
[`dijkstraFrom`](https://evolecolgroup.github.io/geograph/dev/reference/dijkstraFrom.md),
[`dijkstraBetween`](https://evolecolgroup.github.io/geograph/dev/reference/dijkstraBetween.md),
and
[`polygonBetween`](https://evolecolgroup.github.io/geograph/dev/reference/polygonBetween.md)
applied to
[`gGraph`](https://evolecolgroup.github.io/geograph/dev/reference/gGraph-class.md)
or
[`gData`](https://evolecolgroup.github.io/geograph/dev/reference/gData-class.md)
objects. The structure is based on the output from RBGL's `sp.between`
function, enhanced with geographic coordinate information.

## Creating gPath objects

`gPath` objects are created by dijkstra methods. Direct construction is
not recommended.

## Structure

A named list where each element represents a path between two nodes,
containing:

- `path_detail`: a character vector of node names from source to
  destination.

- `length`: a numeric value giving the total cost of the path.

An `xy` attribute stores a matrix of spatial coordinates (longitude and
latitude) for all nodes referenced in the paths, with node identifiers
as row names.

## See also

[`dijkstraFrom`](https://evolecolgroup.github.io/geograph/dev/reference/dijkstraFrom.md),
[`dijkstraBetween`](https://evolecolgroup.github.io/geograph/dev/reference/dijkstraBetween.md)
and
[`polygonBetween`](https://evolecolgroup.github.io/geograph/dev/reference/polygonBetween.md)
to create `gPath` objects.
[`plot.gPath`](https://evolecolgroup.github.io/geograph/dev/reference/plot.gPath.md)
to visualize paths.
[`gPath2dist`](https://evolecolgroup.github.io/geograph/dev/reference/gPath2dist.md)
to extract distances.
[`gGraph`](https://evolecolgroup.github.io/geograph/dev/reference/gGraph-class.md)
and
[`gData`](https://evolecolgroup.github.io/geograph/dev/reference/gData-class.md)
for related classes.

## Examples

``` r
ori <- closestNode(worldgraph.40k, cbind(33, 10))
myPath <- dijkstraFrom(hgdp, ori)

## examine the structure
length(myPath) # number of paths
#> [1] 52
myPath[[1]]$path_detail # nodes in first path
#>  [1] "32713" "32712" "32711" "33351" "33350" "33990" "33989" "34629" "34628"
#> [10] "34627" "35267" "35266" "35906" "35905" "36545" "36544" "36543" "36542"
#> [19] "36541" "36540" "37180" "37820" "38460" "39101" "39741" "39740" "40380"
#> [28] "40379" "40378" "40377" "5891"  "6531"  "40375" "39734" "39093" "38452"
#> [37] "37811" "37170" "36529" "35888" "35887" "35886" "35885" "35884" "35243"
#> [46] "35242" "34601" "33960" "33319" "32678" "32037" "31396" "30755" "30114"
#> [55] "29473" "29472" "29471" "28830" "28189" "27548" "26907" "26906" "26905"
#> [64] "26904" "26903" "26902" "26901" "26900" "26899" "26898"
myPath[[1]]$length # cost of first path
#> [1] 73
myPath[[1]]$length_detail # details for each step
#> [[1]]
#> 32713--32712 32712--32711 32711--33351 33351--33350 33350--33990 33990--33989 
#>            1            1            1            1            1            1 
#> 33989--34629 34629--34628 34628--34627 34627--35267 35267--35266 35266--35906 
#>            1            1            1            1            1            1 
#> 35906--35905 35905--36545 36545--36544 36544--36543 36543--36542 36542--36541 
#>            1            1            1            1            1            1 
#> 36541--36540 36540--37180 37180--37820 37820--38460 38460--39101 39101--39741 
#>            1            1            1            1            1            1 
#> 39741--39740 39740--40380 40380--40379 40379--40378 40378--40377  40377--5891 
#>            1            1            1            1            1            1 
#>   5891--6531  6531--40375 40375--39734 39734--39093 39093--38452 38452--37811 
#>            1            1            1            1            1            1 
#> 37811--37170 37170--36529 36529--35888 35888--35887 35887--35886 35886--35885 
#>            1            1            1            1            1            1 
#> 35885--35884 35884--35243 35243--35242 35242--34601 34601--33960 33960--33319 
#>            1            1            1            1            1            1 
#> 33319--32678 32678--32037 32037--31396 31396--30755 30755--30114 30114--29473 
#>            1            1            1            1            1            1 
#> 29473--29472 29472--29471 29471--28830 28830--28189 28189--27548 27548--26907 
#>            1            1            1            1            1            1 
#> 26907--26906 26906--26905 26905--26904 26904--26903 26903--26902 26902--26901 
#>            1            3            3            1            1            1 
#> 26901--26900 26900--26899 26899--26898 
#>            1            1            1 
#> 

## get coordinates of nodes in paths
head(attr(myPath, "xy"))
#>            lon      lat
#> 32713 33.01964 10.37995
#> 32712 32.40262 11.25346
#> 32711 31.77773 12.12624
#> 33351 32.35828 12.97658
#> 33350 31.72473 13.84543
#> 33990 32.31169 14.69338

## plot the paths
plot(worldgraph.40k, col = NA, reset = TRUE)
#> Spherical geometry (s2) switched off
#> Spherical geometry (s2) switched on
plot(myPath)
```
