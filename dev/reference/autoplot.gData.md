# Default ggplot for a [`gData`](https://evolecolgroup.github.io/geograph/dev/reference/gData-class.md)

Default ggplot for a
[`gData`](https://evolecolgroup.github.io/geograph/dev/reference/gData-class.md)

## Usage

``` r
# S3 method for class 'gData'
autoplot(object, show.gGraph = TRUE, edges = FALSE, ...)
```

## Arguments

- object:

  a
  [`gData`](https://evolecolgroup.github.io/geograph/dev/reference/gData-class.md).

- show.gGraph:

  logical; overlay the linked gGraph if available.

- edges:

  logical; whether to draw edges.

- ...:

  unused.

## Value

a ggplot.

## See also

Other ggplot_methods:
[`autoplot.gGraph()`](https://evolecolgroup.github.io/geograph/dev/reference/autoplot.gGraph.md),
[`geom_gdata()`](https://evolecolgroup.github.io/geograph/dev/reference/geom_gdata.md),
[`geom_ggraph()`](https://evolecolgroup.github.io/geograph/dev/reference/geom_ggraph.md),
[`geom_gpath()`](https://evolecolgroup.github.io/geograph/dev/reference/geom_gpath.md)

## Examples

``` r
autoplot(hgdp)
```
