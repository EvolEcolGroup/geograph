# Get nodes of a gGraph or gData object

The function `getNodes` returns the names of nodes in a
[`gGraph`](https://evolecolgroup.github.io/geograph/dev/reference/gGraph-class.md)
or
[`gData`](https://evolecolgroup.github.io/geograph/dev/reference/gData-class.md)
object.

## Usage

``` r
getNodes(x, ...)

# S4 method for class 'gGraph'
getNodes(x, ...)

# S4 method for class 'gData'
getNodes(x, ...)
```

## Arguments

- x:

  a valid
  [`gGraph`](https://evolecolgroup.github.io/geograph/dev/reference/gGraph-class.md)
  or
  [`gData`](https://evolecolgroup.github.io/geograph/dev/reference/gData-class.md)
  object.

- ...:

  additional arguments passed to other methods (currently unused).

## Value

A character vector of node names.

## Functions

- `getNodes(gGraph)`: Method for gGraph objects

- `getNodes(gData)`: Method for gData objects

## See also

[`getGraph`](https://evolecolgroup.github.io/geograph/dev/reference/getGraph.md),
[`getCoords`](https://evolecolgroup.github.io/geograph/dev/reference/getCoords.md),
[`getNodesAttr`](https://evolecolgroup.github.io/geograph/dev/reference/getNodesAttr.md)

Other accessor_methods:
[`getColors()`](https://evolecolgroup.github.io/geograph/dev/reference/getColors.md),
[`getCoords()`](https://evolecolgroup.github.io/geograph/dev/reference/getCoords.md),
[`getCosts()`](https://evolecolgroup.github.io/geograph/dev/reference/getCosts.md),
[`getData()`](https://evolecolgroup.github.io/geograph/dev/reference/getData.md),
[`getEdges()`](https://evolecolgroup.github.io/geograph/dev/reference/getEdges.md),
[`getGraph()`](https://evolecolgroup.github.io/geograph/dev/reference/getGraph.md),
[`getNodesAttr()`](https://evolecolgroup.github.io/geograph/dev/reference/getNodesAttr.md),
[`setColors()`](https://evolecolgroup.github.io/geograph/dev/reference/setColors.md),
[`setGraph()`](https://evolecolgroup.github.io/geograph/dev/reference/setGraph.md)

## Examples

``` r
head(getNodes(worldgraph.10k))
#> [1] "1" "2" "3" "4" "5" "6"
getNodes(hgdp)
#>   28179   11012   22532   23709   24988   28833   26917   28836   21797   39741 
#> "26898" "11652" "22532" "23709" "24988" "28833" "26917" "28836" "21797" "39741" 
#>   39740   39740   16798   16798   22561   19359   21280   13597   20000   16162 
#> "39740" "39740" "16798" "16798" "22561" "19359" "21280" "13597" "20000" "16162" 
#>   13759    7348   13365   10816    5654   40768   30164    6433   15411   20543 
#> "13760"  "7348" "13365" "10816"  "5655" "40768" "30164"  "6433" "15411" "20543" 
#>   26955   13518    8583   34111   18189   20755   34111     899   20110    5389 
#> "26955" "13518"  "8583" "34111" "18189" "20755" "34111"   "899" "20110"  "5389" 
#>    1539   36661   28323   37309   16265   35388   28322   33480   18842   27148 
#>  "1539" "36661" "28323" "37309" "16265" "35388" "28322" "33480" "19483" "27148" 
#>   11457 
#> "11457" 
```
