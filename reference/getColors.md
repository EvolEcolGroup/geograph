# Get colors associated to edges of a gGraph object

The function `getColors` returns the colors associated to the nodes of a
[gGraph](https://evolecolgroup.github.io/geograph/reference/gGraph-class.md)
object, based on a specified node attribute.

## Usage

``` r
getColors(x, ...)

# S4 method for class 'gGraph'
getColors(x, nodes = "all", attr.name, col.rules = NULL, ...)
```

## Arguments

- x:

  a valid
  [gGraph](https://evolecolgroup.github.io/geograph/reference/gGraph-class.md).

- ...:

  other arguments passed to other methods.

- nodes:

  a vector of character strings or of integers identifying nodes by
  their name or their index. Can be "all", in which case all nodes are
  considered.

- attr.name:

  a character string indicating the name of node attribute to be used to
  define colors.

- col.rules:

  a matrix giving the rules for plotting attribute values with different
  colors. See details.

## Value

A vector of characters being valid colors.  

## Details

Colors are based on a node attribute, that is, on a column of the
`nodes.attr` data.frame. This attribute should have a finite number of
values, and would most likely be a factor. Correspondence between values
of this variable and colors must be provided in the `@meta\$color` slot,
or as `col.rules` argument. Color rules mus be provided as a two-column
matrix; the first column contains values of a node attribute, and is
named after this attribute; the second must be named "color", and
contain valid colors.

See example section to know how this slot should be designed.

## Functions

- `getColors(gGraph)`: Method for gGraph objects

## Examples

``` r

worldgraph.10k # there is a node attribute 'habitat'
#> 
#> === gGraph object ===
#> 
#> @coords: spatial coordinates of 10242 nodes
#>         lon       lat
#> 1 -180.0000  90.00000
#> 2  144.0000 -90.00000
#> 3  -33.7806  27.18924
#> ...
#> 
#> @nodes.attr: 1 nodes attributes
#>   habitat
#> 1     sea
#> 2     sea
#> 3     sea
#> ...
#> 
#> @meta: list of meta information with 2 items
#> [1] "$colors" "$costs" 
#> 
#> @graph:
#> A graphNEL graph with undirected edges
#> Number of Nodes = 10242 
#> Number of Edges = 6954 
worldgraph.10k@meta$color
#>            habitat       color
#> 1              sea        blue
#> 2             land       green
#> 3         mountain       brown
#> 4       landbridge light green
#> 5 oceanic crossing  light blue
#> 6  deselected land   lightgray

head(getNodes(worldgraph.10k))
#> [1] "1" "2" "3" "4" "5" "6"
head(getColors(worldgraph.10k, res.type = "vector", attr.name = "habitat"))
#>      1      2      3      4      5      6 
#> "blue" "blue" "blue" "blue" "blue" "blue" 
```
