# ggplot layer for a [`gGraph`](https://evolecolgroup.github.io/geograph/reference/gGraph-class.md)

Adds nodes (and optionally edges) of a
[`gGraph`](https://evolecolgroup.github.io/geograph/reference/gGraph-class.md)
to a ggplot as
[`ggplot2::geom_sf()`](https://ggplot2.tidyverse.org/reference/ggsf.html)
layers.

## Usage

``` r
geom_ggraph(
  data = NULL,
  mapping = ggplot2::aes(),
  edges = FALSE,
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
  [`gGraph`](https://evolecolgroup.github.io/geograph/reference/gGraph-class.md)
  object.

- mapping:

  aesthetics. Variables from the `gGraph's` node attributes may be used
  (e.g. `aes(color = habitat)`).

- edges:

  logical; whether to draw edges. Defaults to `FALSE`.

- stat, position, na.rm, show.legend, ...:

  forwarded to
  [`ggplot2::geom_sf()`](https://ggplot2.tidyverse.org/reference/ggsf.html).

## Value

one or two ggplot layers (edges under nodes).

## See also

Other ggplot_methods:
[`autoplot.gData()`](https://evolecolgroup.github.io/geograph/reference/autoplot.gData.md),
[`autoplot.gGraph()`](https://evolecolgroup.github.io/geograph/reference/autoplot.gGraph.md),
[`geom_gdata()`](https://evolecolgroup.github.io/geograph/reference/geom_gdata.md),
[`geom_gpath()`](https://evolecolgroup.github.io/geograph/reference/geom_gpath.md)

## Examples

``` r
library(ggplot2)
ggplot() +
  geom_ggraph(data = worldgraph.10k, aes(color = habitat), edges = TRUE, size = 0.3) +
  scale_color_manual(values = c(land = "grey70", sea = "lightblue", coast = "grey70")) +
  coord_sf(crs = "+proj=ortho +lat_0=40 +lon_0=-80") +
  theme_void()
```
