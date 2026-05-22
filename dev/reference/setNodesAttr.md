# Set node attributes in a gGraph object

The function `setNodesAttr` adds or replaces a node attribute in a
[`gGraph`](https://evolecolgroup.github.io/geograph/dev/reference/gGraph-class.md)
object.

## Usage

``` r
setNodesAttr(x, attr.name, values, ...)
```

## Arguments

- x:

  a valid
  [`gGraph`](https://evolecolgroup.github.io/geograph/dev/reference/gGraph-class.md)
  object.

- attr.name:

  a character string giving the name of the attribute to set.

- values:

  a vector of values, one per node.

- ...:

  additional arguments passed to other methods (currently unused).

## Value

A
[`gGraph`](https://evolecolgroup.github.io/geograph/dev/reference/gGraph-class.md)
object with the new node attribute added or replaced.

## See also

[`getNodesAttr`](https://evolecolgroup.github.io/geograph/dev/reference/getNodesAttr.md)
to retrieve node attributes,
[`setCosts`](https://evolecolgroup.github.io/geograph/dev/reference/setCosts.md)
to set edge costs.

## Examples

``` r
### for gGraphs
node.attr <- getNodesAttr(rawgraph.10k, attr.name = "habitat")
neigh.list <- rawgraph.10k@graph@edgeL

# reclassify sea nodes as "coast" if they have any land neighbors
levels(node.attr$habitat) <- c(levels(node.attr$habitat), "coast")
for (i.node in seq_len(nrow(node.attr))) {
  if (node.attr$habitat[i.node] == "sea") {
    neighbour.names <- neigh.list[[i.node]]$edges
    neighbour.land.values <- node.attr[neighbour.names, "habitat"]
    if (any(neighbour.land.values %in% c("land"))) {
      node.attr$habitat[i.node] <- "coast"
    }
  }
}

# create coast graph
coastGraph <- setNodesAttr(rawgraph.10k, attr.name = "habitat", values = node.attr$habitat)

colors <- data.frame(
  habitat = c("sea", "land", "coast"),
  color = c("blue", "green", "lightblue")
)

coastGraph <- setColors(coastGraph, col.rules = colors)

plot(coastGraph, reset = TRUE)

```
