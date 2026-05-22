# Get coordinates of nodes in a gGraph or gData object

The function `getCoords` returns the coordinates (longitude and
latitude) of nodes in a
[`gGraph`](https://evolecolgroup.github.io/geograph/dev/reference/gGraph-class.md)
or
[`gData`](https://evolecolgroup.github.io/geograph/dev/reference/gData-class.md)
object.

## Usage

``` r
getCoords(x, ...)

# S4 method for class 'gGraph'
getCoords(x, ...)

# S4 method for class 'gData'
getCoords(x, original = TRUE, ...)
```

## Arguments

- x:

  a valid
  [`gGraph`](https://evolecolgroup.github.io/geograph/dev/reference/gGraph-class.md)
  or
  [`gData`](https://evolecolgroup.github.io/geograph/dev/reference/gData-class.md)
  object.

- ...:

  additional arguments passed to other methods (currently unused).

- original:

  logical. If `TRUE` (default), returns the original location
  coordinates; if `FALSE`, returns the coordinates of the matched nodes
  on the
  [`gGraph`](https://evolecolgroup.github.io/geograph/dev/reference/gGraph-class.md)
  grid.

## Value

A matrix with two columns, `lon` and `lat`, giving the longitude and
latitude of each node or location.

## Functions

- `getCoords(gGraph)`: Method for gGraph objects

- `getCoords(gData)`: Method for gData objects

## See also

[`getNodes`](https://evolecolgroup.github.io/geograph/dev/reference/getNodes.md),
[`getGraph`](https://evolecolgroup.github.io/geograph/dev/reference/getGraph.md)

Other accessor_methods:
[`getColors()`](https://evolecolgroup.github.io/geograph/dev/reference/getColors.md),
[`getCosts()`](https://evolecolgroup.github.io/geograph/dev/reference/getCosts.md),
[`getData()`](https://evolecolgroup.github.io/geograph/dev/reference/getData.md),
[`getEdges()`](https://evolecolgroup.github.io/geograph/dev/reference/getEdges.md),
[`getGraph()`](https://evolecolgroup.github.io/geograph/dev/reference/getGraph.md),
[`getNodes()`](https://evolecolgroup.github.io/geograph/dev/reference/getNodes.md),
[`getNodesAttr()`](https://evolecolgroup.github.io/geograph/dev/reference/getNodesAttr.md),
[`setColors()`](https://evolecolgroup.github.io/geograph/dev/reference/setColors.md),
[`setGraph()`](https://evolecolgroup.github.io/geograph/dev/reference/setGraph.md)

## Examples

``` r
head(getCoords(worldgraph.10k))
#>          lon       lat
#> 1 -179.99999  90.00000
#> 2  144.00002 -90.00000
#> 3  -33.78060  27.18924
#> 4  -32.44279  25.47768
#> 5  -31.14781  23.76450
#> 6  -29.89332  22.05309

## for a gData object
getCoords(hgdp)
#>          lon   lat
#> 26898   -3.0  59.0
#> 11652   39.0  44.0
#> 22532   40.0  61.0
#> 23709    0.0  43.0
#> 24988    2.0  46.0
#> 28833   10.0  46.0
#> 26917    9.0  40.0
#> 28836   11.0  43.0
#> 21797    3.0  32.0
#> 39741   35.0  31.0
#> 39740   35.0  32.0
#> 39740   35.0  32.0
#> 16798   66.5  30.5
#> 16798   66.5  30.5
#> 22561   74.0  36.5
#> 19359   70.0  33.5
#> 21280   71.5  36.0
#> 13597   64.0  26.0
#> 20000   70.5  33.5
#> 16162   69.0  25.5
#> 13760  155.0  -6.0
#> 7348   143.0  -4.0
#> 13365  -68.0   3.0
#> 10816  -63.0 -10.0
#> 5655   -91.0  19.0
#> 40768 -108.0  29.0
#> 30164   37.0  -3.0
#> 6433   -12.0  12.0
#> 15411    5.0   8.0
#> 20543   17.0   4.0
#> 26955   29.0   1.0
#> 13518   20.0 -21.0
#> 8583   114.0  37.5
#> 34111  100.0  21.0
#> 18189  124.0  48.5
#> 20755  133.5  47.5
#> 34111  100.0  22.0
#> 899    109.0  28.0
#> 20110  126.5  50.5
#> 5389   119.0  27.0
#> 1539   109.0  29.0
#> 36661  101.0  36.0
#> 28323   81.5  43.5
#> 37309  103.0  28.0
#> 16265  119.0  48.5
#> 35388  100.0  26.0
#> 28322   81.0  44.0
#> 33480  105.0  12.0
#> 19483  138.0  38.0
#> 27148  129.5  63.0
#> 11457  -62.0 -11.0
#> 30221   65.0  67.0

## coordinates of matched grid nodes instead of original locations
getCoords(hgdp, original = FALSE)
#>                lon         lat
#> 26898   -5.1726015  57.7615506
#> 11652   38.8504848  44.5216114
#> 22532   40.2354150  61.1399261
#> 23709    0.6982805  42.7522099
#> 24988    1.4896811  45.6608528
#> 28833    9.8760463  46.2838378
#> 26917    9.1818250  39.4827589
#> 28836   11.3792027  43.2587060
#> 21797    3.3885446  31.6726561
#> 39741   34.8358977  31.0356522
#> 39740   34.8205639  32.0660443
#> 39740   34.8205639  32.0660443
#> 16798   66.9381488  30.5937628
#> 16798   66.9381488  30.5937628
#> 22561   73.8579637  36.7856264
#> 19359   69.6578004  33.7396797
#> 21280   72.0000020  35.7902220
#> 13597   63.6772867  26.3596569
#> 20000   70.8288636  33.7555803
#> 16162   68.6799700  25.5879872
#> 13760  155.6124731  -6.3858047
#> 7348   142.8499777  -4.2550149
#> 13365  -67.5328297   2.7997825
#> 10816  -63.0608863 -10.5483802
#> 5655   -90.6425289  18.3184991
#> 40768 -108.0000066  28.6407249
#> 30164   37.1168633  -2.8667984
#> 6433   -12.0558749  11.4985868
#> 15411    5.5052618   8.2252989
#> 20543   16.6834713   3.6938094
#> 26955   29.2084502   0.8638006
#> 13518   19.5183140 -20.5739863
#> 8583   114.3085023  37.4334255
#> 34111  100.1775444  21.4526714
#> 18189  124.6327704  48.1687856
#> 20755  133.1282690  47.1709940
#> 34111  100.1775444  21.4526714
#> 899    109.1211786  27.9242363
#> 20110  127.0215061  50.3812990
#> 5389   119.3596594  27.2640712
#> 1539   109.1349619  28.9644485
#> 36661  100.6377011  35.6568284
#> 28323   81.9635678  43.3690716
#> 37309  102.4312678  28.0527993
#> 16265  118.6223542  48.1576353
#> 35388   99.9912305  26.5584802
#> 28322   81.4539537  44.3789868
#> 33480  104.9839622  12.1070063
#> 19483  138.9505681  37.7066945
#> 27148  130.3634903  62.7228224
#> 11457  -61.8701384 -10.6016019
#> 30221   64.7691130  66.6678236
```
