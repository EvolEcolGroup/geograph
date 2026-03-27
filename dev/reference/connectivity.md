# Check connectivity of a gGraph object

The functions `areNeighbours`, `areConnected` and the method
`isConnected` test connectivity in different ways.  

## Usage

``` r
connectivityPlot(x, ...)

# S4 method for class 'gGraph'
connectivityPlot(x, ..., seed = NULL)

# S4 method for class 'gData'
connectivityPlot(x, col.gGraph = 0, ..., seed = NULL)
```

## Arguments

- x:

  a valid
  [gGraph](https://evolecolgroup.github.io/geograph/dev/reference/gGraph-class.md)
  object.

- ...:

  other arguments passed to other methods.

- seed:

  an optional integer giving the seed to be used when randomizing
  colors. One given seed will always give the same set of colors. NULL
  by default, meaning colors are randomized each time a plot is drawn.

- col.gGraph:

  a character string or a number indicating the color of the nodes to be
  used when plotting the
  [gGraph](https://evolecolgroup.github.io/geograph/dev/reference/gGraph-class.md)
  object. Defaults to '0', meaning that nodes are invisible.

## Value

- `areNeighbours`: a vector of logical, having one value for each couple
  of nodes.  

- `areConnected`: a single logical value, being TRUE if nodes form a
  connected set.  

- `isConnected`: a single logical value, being TRUE if nodes of the
  object form a connected set.  

## Details

- `isConnected`: tests if the nodes of a
  [gData](https://evolecolgroup.github.io/geograph/dev/reference/gData-class.md)
  object form a connected set. Note that this is a method for
  [gData](https://evolecolgroup.github.io/geograph/dev/reference/gData-class.md),
  the generic being defined in the `graph` package.  

- `connectivityPlot`: plots connected sets of a
  [gGraph](https://evolecolgroup.github.io/geograph/dev/reference/gGraph-class.md)
  or a
  [gData](https://evolecolgroup.github.io/geograph/dev/reference/gData-class.md)
  object with different colors.  

In `connectivityPlot`, isolated nodes (i.e. belonging to no connected
set of size \> 1) are plotted in light gray.

## Examples

``` r
connectivityPlot(rawgraph.10k)

connectivityPlot(worldgraph.10k)

```
