# Get colors associated to nodes of a gGraph object

The function `getColors` returns either the color rules stored in a
[`gGraph`](https://evolecolgroup.github.io/geograph/dev/reference/gGraph-class.md)
object (`res.type = "rules"`) or a vector of colors for each node based
on a specified node attribute (`res.type = "colors"`).

## Usage

``` r
getColors(x, ...)

# S4 method for class 'gGraph'
getColors(
  x,
  nodes = "all",
  attr.name = NULL,
  col.rules = NULL,
  res.type = c("colors", "rules"),
  ...
)
```

## Arguments

- x:

  a valid
  [`gGraph`](https://evolecolgroup.github.io/geograph/dev/reference/gGraph-class.md)
  object.

- ...:

  other arguments passed to other methods (currently unused).

- nodes:

  a vector of node names or indices, or `"all"` for all nodes (default).
  Only used when `res.type = "colors"`.

- attr.name:

  a character string giving the name of the node attribute to use for
  color assignment. Required when `res.type = "colors"`.

- col.rules:

  a two-column `data.frame` mapping attribute values to colors. If
  `NULL`, uses `x@meta$colors`. Only used when `res.type = "colors"`.

- res.type:

  a character string indicating the output type:

  - `"colors"`: a named character vector of colors, one per node.

  - `"rules"`: the color rules `data.frame` stored in `x@meta$colors`.

## Value

A named character vector of colors when `res.type = "colors"`, or a
`data.frame` of color rules when `res.type = "rules"`.

## Details

Color rules are stored as a two-column `data.frame` in `x@meta$colors`.
The first column is named after the node attribute and contains its
possible values; the second column is named `"color"` and contains valid
R color strings.

## Functions

- `getColors(gGraph)`: Method for gGraph objects

## See also

[`setColors`](https://evolecolgroup.github.io/geograph/dev/reference/setColors.md)
to set color rules.
[`getNodesAttr`](https://evolecolgroup.github.io/geograph/dev/reference/getNodesAttr.md)
to retrieve node attributes.

Other accessor_methods:
[`getCoords()`](https://evolecolgroup.github.io/geograph/dev/reference/getCoords.md),
[`getCosts()`](https://evolecolgroup.github.io/geograph/dev/reference/getCosts.md),
[`getData()`](https://evolecolgroup.github.io/geograph/dev/reference/getData.md),
[`getEdges()`](https://evolecolgroup.github.io/geograph/dev/reference/getEdges.md),
[`getGraph()`](https://evolecolgroup.github.io/geograph/dev/reference/getGraph.md),
[`getNodes()`](https://evolecolgroup.github.io/geograph/dev/reference/getNodes.md),
[`getNodesAttr()`](https://evolecolgroup.github.io/geograph/dev/reference/getNodesAttr.md),
[`setColors()`](https://evolecolgroup.github.io/geograph/dev/reference/setColors.md),
[`setGraph()`](https://evolecolgroup.github.io/geograph/dev/reference/setGraph.md)

## Examples

``` r
## get color rules
getColors(worldgraph.10k, res.type = "rules")
#>            habitat       color
#> 1              sea        blue
#> 2             land       green
#> 3         mountain       brown
#> 4       landbridge light green
#> 5 oceanic crossing  light blue
#> 6  deselected land   lightgray

## get node colors based on habitat attribute
head(getColors(worldgraph.10k, attr.name = "habitat"))
#>      1      2      3      4      5      6 
#> "blue" "blue" "blue" "blue" "blue" "blue" 
```
