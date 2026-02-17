# Get rid of some 'dead' edges or nodes

The functions `dropDeadEdges` and `dropDeadNodes` are used to remove
'dead edges' and 'dead nodes'.  

## Usage

``` r
dropDeadEdges(x, thres)
```

## Arguments

- x:

  a valid
  [gGraph](https://evolecolgroup.github.io/geograph/dev/reference/gGraph-class.md).

- thres:

  a numeric value indicating the threshold cost for an edge to be
  removed. All costs strictly greater than `thres` will be removed.

## Value

A
[gGraph](https://evolecolgroup.github.io/geograph/dev/reference/gGraph-class.md)
object.

## Details

Dead edges are edges associated to a prohibitive cost, that is, edges
that no longer imply connectivity between two nodes.  

Dead nodes are nodes that are not connected to any other node, thus not
having any role in the connectivity of a graph.  

## Examples

``` r
if (FALSE) { # \dontrun{
plot(worldgraph.10k, reset = TRUE)
x <- dropDeadNodes(worldgraph.10k)
plot(x)
} # }
```
