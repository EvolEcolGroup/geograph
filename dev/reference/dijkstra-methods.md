# Shortest path using Dijkstra algorithm

The methods `dijkstraFrom` and `dijkstraBetween` are wrappers of
procedures implemented in RBGL package, designed for
[gGraph](https://evolecolgroup.github.io/geograph/dev/reference/gGraph-class.md)
and
[gData](https://evolecolgroup.github.io/geograph/dev/reference/gData-class.md)
object.  

## Usage

``` r
dijkstraBetween(x, ...)

# S4 method for class 'gGraph'
dijkstraBetween(x, from, to)

# S4 method for class 'gData'
dijkstraBetween(x)

dijkstraFrom(x, ...)

# S4 method for class 'gGraph'
dijkstraFrom(x, start)

# S4 method for class 'gData'
dijkstraFrom(x, start)

# S3 method for class 'gPath'
plot(x, col = "rainbow", lwd = 3, ...)

gPath2dist(m, diag = FALSE, upper = FALSE, res.type = c("dist", "vector"))
```

## Arguments

- x:

  a
  [gGraph](https://evolecolgroup.github.io/geograph/dev/reference/gGraph-class.md)
  or a
  [gData](https://evolecolgroup.github.io/geograph/dev/reference/gData-class.md)
  object. For plotting method of `gPath` objects, a `gPath` object.

- ...:

  further arguments passed to the `segments` method.

- from:

  a vector of character strings giving node names.

- to:

  a vector of character strings giving node names.

- start:

  a character string naming the 'source' node.

- col:

  a character string indicating a color or a palette of colors to be
  used for plotting edges.

- lwd:

  a numeric value indicating the width of edges.

- m:

  a `gPath` object obtained by `dijkstraBetween`.

- diag, upper:

  unused parameters added for consistency with `as.dist`.

- res.type:

  a character string indicating what type of result should be returned:
  a `dist` object ('dist'), or a vector of distances ('vector'). Note
  that 'dist' should only be required for pairwise data, as output by
  dijkstraBetween (as opposed to dijkstraFrom).

## Value

A "gPath" object. These are basically the outputs of RBGL's `sp.between`
function (see `?sp.between`), with a class attribute set to "gPath", and
an additional slot 'xy' containing geographic coordinates of the nodes
involved in the paths.  

## Details

`dijkstraFrom` finds minimum costs paths to nodes from a given 'source'
node.  

`dijkstraBetween` finds minimum costs paths between all possible pairs
of nodes given two sets of nodes.  

All these functions return objects with S3 class "gPath". These objects
can be plotted using `plot.gPath`.

`gPath2dist` extracts the pairwise distances from the `gPath` returned
by `dijkstraBetween` and returns a `dist` object. Note that if the
`gPath` does not contain pairwise information, a warning will be issued,
but the resulting output will likely be meaningless.  

In 'dijkstraBetween', paths are sought all possible pairs of nodes
between 'from' and 'to'.

## Examples

``` r
if (FALSE) { # \dontrun{

## plotting
world <- worldgraph.40k
par(mar = rep(.1, 4))
plot(world, reset = TRUE)

## check connectivity
isConnected(hgdp) # must be ok

## Lowest cost path from an hypothetical origin
ori.coord <- list(33, 10) # one given location long/lat
points(data.frame(ori.coord), pch = "x", col = "black", cex = 3) # an 'x' shows the putative origin
ori <- closestNode(world, ori.coord) # assign it the closest node

myPath <- dijkstraFrom(hgdp, ori) # compute shortest path

## plotting
plot(world, pch = "") # plot the world
points(hgdp, lwd = 3) # plot populations
points(data.frame(ori.coord), pch = "x", col = "black", cex = 3) # add origin
plot(myPath) # plot the path
} # }
```
