# Add and remove edges from a gGraph object

The function `setEdges` allows one to add or remove edges in a
[gGraph](https://evolecolgroup.github.io/geograph/reference/gGraph-class.md)
by directly specifying the relevant nodes, as a list or a data.frame.
This low-level function is called by `geo.add.edges` and
`geo.remove.edges`.

## Usage

``` r
setEdges(x, ...)

# S4 method for class 'gGraph'
setEdges(x, add = NULL, remove = NULL, costs = NULL, ...)
```

## Arguments

- x:

  a valid
  [gGraph](https://evolecolgroup.github.io/geograph/reference/gGraph-class.md)
  object.

- ...:

  other arguments passed to other methods (currently unused).

- add:

  a list or a dataframe containing node names of edges to be added. The
  first element of the list (or column of the data.frame) gives starting
  nodes of edges; the second gives ending nodes. Hence, the nodes of the
  i-th edge are `add[[1]][i]` and `add[[2]][i]` if `add` is a list, and
  `add[i,]` if `add` is a data.frame.

- remove:

  same as `add` argument, but edges are removed.

- costs:

  a numeric vector providing costs of the edges to be added. `costs[i]`
  is the weight of the i-th edge.

## Value

A
[gGraph](https://evolecolgroup.github.io/geograph/reference/gGraph-class.md)
object with newly added or removed edges.

## Functions

- `setEdges(gGraph)`: Method for gGraph object

## See also

[`geo.add.edges`](https://evolecolgroup.github.io/geograph/reference/geo.add.edges.md)
and
[`geo.remove.edges`](https://evolecolgroup.github.io/geograph/reference/geo.add.edges.md)
to interactively add or remove edges in a
[gGraph](https://evolecolgroup.github.io/geograph/reference/gGraph-class.md)
object.  

[`getEdges`](https://evolecolgroup.github.io/geograph/reference/getEdges.md)
to retrieve edges in different formats.
