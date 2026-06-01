# Add and remove edges from a gGraph object

The function `setEdges` allows one to add or remove edges in a
[gGraph](https://evolecolgroup.github.io/geograph/dev/reference/gGraph-class.md)
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
  [gGraph](https://evolecolgroup.github.io/geograph/dev/reference/gGraph-class.md)
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
[gGraph](https://evolecolgroup.github.io/geograph/dev/reference/gGraph-class.md)
object with newly added or removed edges.

## Functions

- `setEdges(gGraph)`: Method for gGraph object

## See also

[`getEdges`](https://evolecolgroup.github.io/geograph/dev/reference/getEdges.md)
to retrieve edges.
[`geo.add.edges`](https://evolecolgroup.github.io/geograph/dev/reference/geo.add.edges.md)
and
[`geo.remove.edges`](https://evolecolgroup.github.io/geograph/dev/reference/geo.add.edges.md)
for interactive versions.

## Examples

``` r
# check which nodes are neighbours of node "1"
head(getEdges(worldgraph.10k, res.type = "matNames"))
#>      Vi   Vj    
#> [1,] "67" "9955"
#> [2,] "67" "387" 
#> [3,] "67" "68"  
#> [4,] "67" "9953"
#> [5,] "67" "388" 
#> [6,] "68" "388" 

# remove an edge between two neighbouring nodes
node.from <- "1"
node.to <- getEdges(worldgraph.10k, res.type = "matNames")[1, 2]

x <- setEdges(worldgraph.10k,
  remove = data.frame(from = node.from, to = node.to)
)

# verify the edge is gone
areNeighbours(node.from, node.to, getGraph(x))
#> 1->9955 
#>   FALSE 

# add it back
x <- setEdges(x, add = data.frame(from = node.from, to = node.to))
areNeighbours(node.from, node.to, getGraph(x))
#> 1->9955 
#>    TRUE 
```
