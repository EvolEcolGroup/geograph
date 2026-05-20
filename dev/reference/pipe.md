# Pipe operator

See `magrittr::%>%` for details.

## Usage

``` r
lhs %>% rhs
```

## Arguments

- lhs:

  A value or the magrittr placeholder.

- rhs:

  A function call using the magrittr semantics.

## Value

The result of calling `rhs(lhs)`.

## Examples

``` r
getNodesAttr(worldgraph.10k) %>% head()
#>   habitat
#> 1     sea
#> 2     sea
#> 3     sea
#> 4     sea
#> 5     sea
#> 6     sea
```
