# ggplot layer for a [`gData`](https://evolecolgroup.github.io/geograph/dev/reference/gData-class.md)

Adds sample localities of a
[`gData`](https://evolecolgroup.github.io/geograph/dev/reference/gData-class.md)
to a ggplot as a
[`ggplot2::geom_sf()`](https://ggplot2.tidyverse.org/reference/ggsf.html)
layer.

## Usage

``` r
geom_gdata(
  data = NULL,
  mapping = ggplot2::aes(),
  original = FALSE,
  stat = "sf",
  position = "identity",
  na.rm = FALSE,
  show.legend = NA,
  ...
)
```

## Arguments

- data:

  a
  [`gData`](https://evolecolgroup.github.io/geograph/dev/reference/gData-class.md)
  object.

- mapping:

  aesthetics.

- original:

  logical. If TRUE, plot at the original sample locations; if FALSE
  (default), plot at the assigned-node coordinates on the linked gGraph.

- stat, position, na.rm, show.legend, ...:

  forwarded to
  [`ggplot2::geom_sf()`](https://ggplot2.tidyverse.org/reference/ggsf.html).

## Value

a ggplot layer.

## See also

Other ggplot_methods:
[`autoplot.gData()`](https://evolecolgroup.github.io/geograph/dev/reference/autoplot.gData.md),
[`autoplot.gGraph()`](https://evolecolgroup.github.io/geograph/dev/reference/autoplot.gGraph.md),
[`geom_ggraph()`](https://evolecolgroup.github.io/geograph/dev/reference/geom_ggraph.md),
[`geom_gpath()`](https://evolecolgroup.github.io/geograph/dev/reference/geom_gpath.md)

## Examples

``` r
library(ggplot2)
ggplot() +
 geom_gdata(data = hgdp, color = "black", size = 1.5) +
 theme_void()
```
