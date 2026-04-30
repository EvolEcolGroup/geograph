# Get nodes attributes from gGraph/gData object

The function `getNodesAttr` returns the values of a set of variables
associated to the nodes (i.e. node attributes) of a
[gGraph](https://evolecolgroup.github.io/geograph/dev/reference/gGraph-class.md)
or
[gData](https://evolecolgroup.github.io/geograph/dev/reference/gData-class.md)
object.

## Usage

``` r
getNodesAttr(x, ...)

# S4 method for class 'gGraph'
getNodesAttr(x, nodes = NULL, attr.name = NULL, ...)

# S4 method for class 'gData'
getNodesAttr(x, attr.name = NULL, ...)
```

## Arguments

- x:

  a valid
  [gGraph](https://evolecolgroup.github.io/geograph/dev/reference/gGraph-class.md)
  or
  [gData](https://evolecolgroup.github.io/geograph/dev/reference/gData-class.md)
  object.

- ...:

  other arguments passed to other methods (currently unused).

- nodes:

  an optional integer, logical, or character string indicating the
  subset of nodes to be used. If NULL, all nodes are used.

- attr.name:

  an optional character string indicating which node attributes should
  be returned. If provided, it must match at least one of the columns of
  `x@nodes.attr`.

## Value

A data.frame with the requested nodes attributes. Nodes are displayed in
rows, variables in columns.

## Functions

- `getNodesAttr(gGraph)`: Method for gGraph objects

- `getNodesAttr(gData)`: Method for gData objects

## See also

Most other accessors are documented in
[gGraph](https://evolecolgroup.github.io/geograph/dev/reference/gGraph-class.md)
and
[gData](https://evolecolgroup.github.io/geograph/dev/reference/gData-class.md)
manpages.  

## Examples

``` r

## gGraph method
head(getNodesAttr(worldgraph.40k))
#>   habitat
#> 1     sea
#> 2     sea
#> 3     sea
#> 4     sea
#> 5     sea
#> 6     sea


## gData method
getNodesAttr(hgdp)
#>         habitat
#> 26898     coast
#> 11652     coast
#> 22532      land
#> 23709      land
#> 24988      land
#> 28833      land
#> 26917     coast
#> 28836     coast
#> 21797      land
#> 39741     coast
#> 39740     coast
#> 39740.1   coast
#> 16798      land
#> 16798.1    land
#> 22561      land
#> 19359      land
#> 21280      land
#> 13597     coast
#> 20000      land
#> 16162      land
#> 13760     coast
#> 7348      coast
#> 13365      land
#> 10816      land
#> 5655      coast
#> 40768      land
#> 30164      land
#> 6433       land
#> 15411      land
#> 20543      land
#> 26955      land
#> 13518      land
#> 8583       land
#> 34111      land
#> 18189      land
#> 20755      land
#> 34111.1    land
#> 899        land
#> 20110      land
#> 5389       land
#> 1539       land
#> 36661      land
#> 28323      land
#> 37309      land
#> 16265      land
#> 35388      land
#> 28322      land
#> 33480      land
#> 19483     coast
#> 27148      land
#> 11457      land
#> 30221      land
```
