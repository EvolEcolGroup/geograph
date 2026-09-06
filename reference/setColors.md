# Set color rules for a gGraph object

The function `setColors` sets the color rules stored in the
`@meta$colors` slot of a
[`gGraph`](https://evolecolgroup.github.io/geograph/reference/gGraph-class.md)
object. Color rules control how node attribute values are mapped to
colors when plotting.

## Usage

``` r
setColors(x, col.rules)
```

## Arguments

- x:

  a valid
  [`gGraph`](https://evolecolgroup.github.io/geograph/reference/gGraph-class.md)
  object.

- col.rules:

  a two-column `data.frame` mapping attribute values to colors.

## Value

A
[`gGraph`](https://evolecolgroup.github.io/geograph/reference/gGraph-class.md)
object with the updated color rules.

## Details

Color rules must be provided as a two-column `data.frame`. The first
column must be named after the node attribute and contain its possible
values; the second column must be named `"color"` and contain valid R
color strings.

## See also

[`getColors`](https://evolecolgroup.github.io/geograph/reference/getColors.md)
to retrieve colors or color rules.

Other accessor_methods:
[`getColors()`](https://evolecolgroup.github.io/geograph/reference/getColors.md),
[`getCoords()`](https://evolecolgroup.github.io/geograph/reference/getCoords.md),
[`getCosts()`](https://evolecolgroup.github.io/geograph/reference/getCosts.md),
[`getData()`](https://evolecolgroup.github.io/geograph/reference/getData.md),
[`getEdges()`](https://evolecolgroup.github.io/geograph/reference/getEdges.md),
[`getGraph()`](https://evolecolgroup.github.io/geograph/reference/getGraph.md),
[`getNodes()`](https://evolecolgroup.github.io/geograph/reference/getNodes.md),
[`getNodesAttr()`](https://evolecolgroup.github.io/geograph/reference/getNodesAttr.md),
[`setGraph()`](https://evolecolgroup.github.io/geograph/reference/setGraph.md)

## Examples

``` r
## get current rules
col.rules <- getColors(worldgraph.10k, res.type = "rules")
col.rules
#>            habitat       color
#> 1              sea        blue
#> 2             land       green
#> 3         mountain       brown
#> 4       landbridge light green
#> 5 oceanic crossing  light blue
#> 6  deselected land   lightgray

## modify a color
col.rules$color[col.rules$habitat == "sea"] <- "lightblue"

## set back
x <- setColors(worldgraph.10k, col.rules)
getColors(x, res.type = "rules")
#>            habitat       color
#> 1              sea   lightblue
#> 2             land       green
#> 3         mountain       brown
#> 4       landbridge light green
#> 5 oceanic crossing  light blue
#> 6  deselected land   lightgray
```
