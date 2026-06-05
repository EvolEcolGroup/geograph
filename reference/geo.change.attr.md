# Change values of a node attribute

The functions `geo.change.attr` changes values of a given node attribute
for a set of selected nodes of a
[gGraph](https://evolecolgroup.github.io/geograph/reference/gGraph-class.md)
object.

## Usage

``` r
geo.change.attr(
  x,
  mode = c("points", "area"),
  attr.name,
  attr.value,
  only.name = NULL,
  only.value = NULL,
  newCol = "black",
  restore.edges = FALSE,
  refObj = "rawgraph.40k"
)
```

## Arguments

- x:

  a valid
  [gGraph](https://evolecolgroup.github.io/geograph/reference/gGraph-class.md)
  object.

- mode:

  a character string indicating whether selected nodes are clicked one
  by one ('points') or by defining a rectangular area ('area').

- attr.name:

  the name of the node attribute to be modified.

- attr.value:

  the new value of attribute assigned to selected nodes.

- only.name:

  (optional) in area mode, the name of a node attribute to add an extra
  selection criterion. See details.

- only.value:

  (optional) in area mode, and if `only.name` is specified, the values
  of `only.name` that can be selected. See details.

- newCol:

  a character string giving the new color for the attribute value.

- restore.edges:

  a logical indicating whether edges stemming from the modified nodes
  should be re-added to the graph, using `refObj` as a reference. This
  is useful when connectivity is to be redefined using
  [`setCosts`](https://evolecolgroup.github.io/geograph/reference/setCosts.md)
  for nodes that were previously disconnected.

- refObj:

  a character string or a
  [gGraph](https://evolecolgroup.github.io/geograph/reference/gGraph-class.md)
  object, used as reference when re-adding edges. If a character string
  is provided, it must match one of the following dataset:
  'rawgraph.10k', 'rawgraph.40k'.

## Value

A
[gGraph](https://evolecolgroup.github.io/geograph/reference/gGraph-class.md)
object with modified node attributes.

## Details

The argument `only.name` allows one to perform a more accurate selection
of nodes whose attribute is changed, by specifying values (`only.value`)
of an attribute (`only.name`) that can be selected. For instance, one
may want to define new attributes for nodes of worldgraph.10k that are
exclusively on land: this would be done by specifying
`only.name="habitat"` and `only.value="land"`.

## Examples

``` r
if (FALSE) { # \dontrun{
plot(worldgraph.10k, reset = TRUE)

## have to click here for an area
## all nodes are modified in the area
x <- geo.change.attr(worldgraph.10k, mode = "area", attr.name = "habitat", attr.value = "fancy
habitat", newCol = "pink") # modify selected area

plot(x, reset = TRUE) # modification in the whole selected area

## have to click here for an area
## only nodes on land are modified
x <- geo.change.attr(x, mode = "area", attr.name = "habitat", attr.value = "fancy2
habitat", newCol = "purple", only.name = "habitat", only.value = "land")

plot(x, reset = TRUE) # modification in the whole selected area
} # }
```
