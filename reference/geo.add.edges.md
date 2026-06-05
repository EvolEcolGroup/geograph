# Add and remove edges from a gGraph object

The functions `geo.add.edges` and `geo.remove.edges` allow one to add or
remove edges interactively with a
[gGraph](https://evolecolgroup.github.io/geograph/reference/gGraph-class.md)
object. When adding edges, two approaches are possible:  
- click vertices defining new edges (mode="points")  
- select an area in which all edges from a reference graph are added
(mode="area").  

## Usage

``` r
geo.add.edges(x, mode = c("points", "area", "all"), refObj = "rawgraph.40k")
```

## Arguments

- x:

  a valid
  [gGraph](https://evolecolgroup.github.io/geograph/reference/gGraph-class.md)
  object.

- mode:

  a character string indicating the mode for addition or removal of
  edges. 'points': user is expected to click vertices to indicate edges.
  'area': user is expected to click two points defining a rectangular
  area within which all edges are selected. 'all': all edges from the
  reference graph are added to the current object.

- refObj:

  a valid
  [gGraph](https://evolecolgroup.github.io/geograph/reference/gGraph-class.md)
  object, used as a reference when adding edges. When selecting an area
  inside which edges are added, all edges existing in this area in
  `refObj` are added to `x`. Alternatively, a character string can be
  provided, corresponding to one of the following datasets:
  'rawgraph.10k', rawgraph.40k'.

## Value

A
[gGraph](https://evolecolgroup.github.io/geograph/reference/gGraph-class.md)
object with newly added or removed edges.

## See also

[`setEdges`](https://evolecolgroup.github.io/geograph/reference/setEdges.md)
to non-interactively add or remove edges in a
[gGraph](https://evolecolgroup.github.io/geograph/reference/gGraph-class.md)
object.

## Examples

``` r
if (FALSE) { # \dontrun{
plot(worldgraph.10k, reset = TRUE)

## zooming in
geo.zoomin(list(x = c(-6, 38), y = c(35, 73)))
title("Europe")

## remove edges
geo.remove.edges(worldgraph.10k) # points mode
geo.remove.edges(worldgraph.10k, mode = "area") # area mode

## add edges
geo.add.edges(worldgraph.10k) # points mode
geo.add.edges(worldgraph.10k, mode = "area") # area mode
} # }
```
