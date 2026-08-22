# ggplot layer for a gPath

Adds a
[`gPath`](https://evolecolgroup.github.io/geograph/dev/reference/gPath-class.md)
(from
[`dijkstraBetween()`](https://evolecolgroup.github.io/geograph/dev/reference/dijkstraBetween.md)
or
[`dijkstraFrom()`](https://evolecolgroup.github.io/geograph/dev/reference/dijkstraFrom.md))
to a ggplot as
[`ggplot2::geom_sf()`](https://ggplot2.tidyverse.org/reference/ggsf.html)
linestrings.

## Usage

``` r
geom_gpath(
  data = NULL,
  mapping = ggplot2::aes(),
  stat = "sf",
  position = "identity",
  na.rm = FALSE,
  show.legend = NA,
  ...
)
```

## Arguments

- data:

  a `gPath` object.

- mapping:

  aesthetics.

- stat, position, na.rm, show.legend, ...:

  forwarded to
  [`ggplot2::geom_sf()`](https://ggplot2.tidyverse.org/reference/ggsf.html).

## Value

a ggplot layer.

## See also

Other ggplot_methods:
[`autoplot.gData()`](https://evolecolgroup.github.io/geograph/dev/reference/autoplot.gData.md),
[`autoplot.gGraph()`](https://evolecolgroup.github.io/geograph/dev/reference/autoplot.gGraph.md),
[`geom_gdata()`](https://evolecolgroup.github.io/geograph/dev/reference/geom_gdata.md),
[`geom_ggraph()`](https://evolecolgroup.github.io/geograph/dev/reference/geom_ggraph.md)

## Examples

``` r
library(ggplot2)
addis <- list(lon = 38.74, lat = 9.03)
addis_node <- closestNode(worldgraph.40k, addis)
myPath <- dijkstraFrom(hgdp, addis_node)
ggplot() +
  geom_ggraph(data = worldgraph.40k, aes(color = habitat),
              edges = FALSE, size = 1, show.legend = FALSE) +
  scale_color_manual(values = c(land = "grey70", sea = "lightblue", coast = "grey70")) +
  geom_gpath(data = myPath, color = "firebrick", linewidth = 0.4) +
  geom_gdata(data = hgdp, color = "black", size = 1.5) +
  coord_sf(crs = "+proj=ortho +lat_0=40 +lon_0=30") +
  theme_void()
```
