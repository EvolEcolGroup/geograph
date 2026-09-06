# Default ggplot for a [`gGraph`](https://evolecolgroup.github.io/geograph/reference/gGraph-class.md)

Default ggplot for a
[`gGraph`](https://evolecolgroup.github.io/geograph/reference/gGraph-class.md)

## Usage

``` r
# S3 method for class 'gGraph'
autoplot(object, edges = TRUE, ...)
```

## Arguments

- object:

  a
  [`gGraph`](https://evolecolgroup.github.io/geograph/reference/gGraph-class.md).

- edges:

  logical; whether to draw edges.

- ...:

  unused.

## Value

a ggplot.

## See also

Other ggplot_methods:
[`autoplot.gData()`](https://evolecolgroup.github.io/geograph/reference/autoplot.gData.md),
[`geom_gdata()`](https://evolecolgroup.github.io/geograph/reference/geom_gdata.md),
[`geom_ggraph()`](https://evolecolgroup.github.io/geograph/reference/geom_ggraph.md),
[`geom_gpath()`](https://evolecolgroup.github.io/geograph/reference/geom_gpath.md)

## Examples

``` r
autoplot(worldgraph.10k)
```
