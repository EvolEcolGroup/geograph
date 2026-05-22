# Plot connected sets of a gGraph object

The function `connectivityPlot` plots the connected sets of a
[`gGraph`](https://evolecolgroup.github.io/geograph/dev/reference/gGraph-class.md)
or
[`gData`](https://evolecolgroup.github.io/geograph/dev/reference/gData-class.md)
object with different colors. Isolated nodes (i.e. belonging to no
connected set of size \> 1) are plotted in light gray.

## Usage

``` r
connectivityPlot(x, ...)

# S4 method for class 'gGraph'
connectivityPlot(x, ..., seed = NULL)

# S4 method for class 'gData'
connectivityPlot(x, ..., seed = NULL)
```

## Arguments

- x:

  a valid
  [`gGraph`](https://evolecolgroup.github.io/geograph/dev/reference/gGraph-class.md)
  or
  [`gData`](https://evolecolgroup.github.io/geograph/dev/reference/gData-class.md)
  object.

- ...:

  other arguments passed to other methods.

- seed:

  an optional integer giving the seed to be used when randomizing
  colors. A given seed will always produce the same set of colors.
  `NULL` by default, meaning colors are randomized each time a plot is
  drawn.

## Value

A named character vector of colors, one per node, returned invisibly.

## Functions

- `connectivityPlot(gGraph)`: Method for gGraph objects

- `connectivityPlot(gData)`: Method for gData objects

## Examples

``` r
# plot connected sets of a gGraph object
connectivityPlot(worldgraph.10k)


# plot connected sets of a gData object
connectivityPlot(hgdp)
```
