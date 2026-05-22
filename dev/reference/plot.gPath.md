# Plot a gPath object

This method plots a
[`gPath`](https://evolecolgroup.github.io/geograph/dev/reference/gPath-class.md)
object, which is the output of the
[`dijkstraBetween`](https://evolecolgroup.github.io/geograph/dev/reference/dijkstraBetween.md),
[`polygonBetween`](https://evolecolgroup.github.io/geograph/dev/reference/polygonBetween.md)
and
[`dijkstraFrom`](https://evolecolgroup.github.io/geograph/dev/reference/dijkstraFrom.md)
functions.

## Usage

``` r
# S3 method for class 'gPath'
plot(x, col = "rainbow", lwd = 3, seed = NULL, ...)

# S3 method for class 'gPath'
print(x, ...)
```

## Arguments

- x:

  a
  [`gPath`](https://evolecolgroup.github.io/geograph/dev/reference/gPath-class.md)
  object

- col:

  a character string indicating a color or a palette of colors to be
  used for plotting edges.

- lwd:

  a numeric value indicating the width of edges.

- seed:

  an optional integer value to set the seed for random color generation
  when `col = "rainbow"`.

- ...:

  further arguments passed to
  [`geo.segments`](https://evolecolgroup.github.io/geograph/dev/reference/geo.segments.md).

## Value

NULL.

## Functions

- `print(gPath)`: Print a summary of a gPath object

## See also

Other plotting_methods:
[`plot-gData`](https://evolecolgroup.github.io/geograph/dev/reference/plot-gData.md),
[`plot-gGraph`](https://evolecolgroup.github.io/geograph/dev/reference/plot-gGraph.md)

## Examples

``` r
hgdp.sub <- hgdp[getData(hgdp)$Population %in%
  c("French", "Balochi", "BantuKenya", "Papuan", "Pima")]
hgdp.path <- dijkstraBetween(hgdp.sub) # compute shortest path

## plotting
plot(worldgraph.40k, reset = TRUE, pch = "")
points(hgdp.sub, lwd = 1) # plot populations
plot(hgdp.path) # plot the path


## printing
print(hgdp.path)
#> 
#> === gPath object ===
#> 
#>  number of paths: 10 
#> 
#>  available paths (id_origin:id_destination): 24988:16798 24988:7348 24988:40768 ...
#> 
#> each path, accessible with [[]] has elements 'length', 'path_detail' and 'length_detail'
#> x and y coordinates of all nodes are stored as an attribute 'xy'; see ?gPath for details
```
