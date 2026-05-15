# Get the data component of a gData object

The function `getData` returns the data stored in the `@data` slot of a
[`gData`](https://evolecolgroup.github.io/geograph/dev/reference/gData-class.md)
object.

## Usage

``` r
getData(x, ...)

# S4 method for class 'gData'
getData(x, ...)
```

## Arguments

- x:

  a valid
  [`gData`](https://evolecolgroup.github.io/geograph/dev/reference/gData-class.md)
  object.

- ...:

  additional arguments passed to other methods (currently unused).

## Value

The data stored in the `@data` slot of the input object, typically a
data.frame where each row corresponds to a sampled location.

## Functions

- `getData(gData)`: Method for gData objects

## See also

[`getCoords`](https://evolecolgroup.github.io/geograph/dev/reference/getCoords.md)
to retrieve coordinates.
[`getNodes`](https://evolecolgroup.github.io/geograph/dev/reference/getNodes.md)
to retrieve matched node identifiers.
[`getNodesAttr`](https://evolecolgroup.github.io/geograph/dev/reference/getNodesAttr.md)
to retrieve node attributes from the underlying
[`gGraph`](https://evolecolgroup.github.io/geograph/dev/reference/gGraph-class.md).

Other accessor_methods:
[`getColors()`](https://evolecolgroup.github.io/geograph/dev/reference/getColors.md),
[`getCoords()`](https://evolecolgroup.github.io/geograph/dev/reference/getCoords.md),
[`getCosts()`](https://evolecolgroup.github.io/geograph/dev/reference/getCosts.md),
[`getEdges()`](https://evolecolgroup.github.io/geograph/dev/reference/getEdges.md),
[`getGraph()`](https://evolecolgroup.github.io/geograph/dev/reference/getGraph.md),
[`getNodes()`](https://evolecolgroup.github.io/geograph/dev/reference/getNodes.md),
[`getNodesAttr()`](https://evolecolgroup.github.io/geograph/dev/reference/getNodesAttr.md)

## Examples

``` r
## get the data stored in the hgdp dataset
head(getData(hgdp))
#>   Population Region Label  n Latitude Longitude Genetic.Div
#> 1   Orcadian EUROPE     1 15       59        -3   0.7258820
#> 2     Adygei EUROPE     2 17       44        39   0.7297802
#> 3    Russian EUROPE     3 25       61        40   0.7319749
#> 4     Basque EUROPE     4 24       43         0   0.7191268
#> 5     French EUROPE     5 28       46         2   0.7312109
#> 6    Italian EUROPE     6 13       46        10   0.7280693
```
