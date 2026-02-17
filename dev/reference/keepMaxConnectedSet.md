# Keep only the largest connected set

This function removes all nodes that are not part of the largest
connected set.

## Usage

``` r
keepMaxConnectedSet(x)
```

## Arguments

- x:

  a
  [gGraph](https://evolecolgroup.github.io/geograph/dev/reference/gGraph-class.md)
  object

## Value

a
[gGraph](https://evolecolgroup.github.io/geograph/dev/reference/gGraph-class.md)
object with only the nodes from the largest set remaining

## Examples

``` r
max_set <- keepMaxConnectedSet(worldgraph.10k)
plot(max_set)

```
