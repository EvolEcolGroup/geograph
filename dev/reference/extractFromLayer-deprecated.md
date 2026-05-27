# Assign node attributes from a polygon layer

Deprecated. Use
[`assignByPolygon`](https://evolecolgroup.github.io/geograph/dev/reference/assignByPolygon.md)
instead.

## Usage

``` r
# S4 method for class 'ANY'
extractFromLayer(x, ...)
```

## Arguments

- x:

  a matrix, `data.frame`, list, valid
  [`gGraph`](https://evolecolgroup.github.io/geograph/dev/reference/gGraph-class.md),
  or valid
  [`gData`](https://evolecolgroup.github.io/geograph/dev/reference/gData-class.md)
  object.

- ...:

  further arguments passed to other methods.

## Value

See
[`assignByPolygon`](https://evolecolgroup.github.io/geograph/dev/reference/assignByPolygon.md)
for details.

## See also

[`geoGraph-deprecated`](https://evolecolgroup.github.io/geograph/dev/reference/geoGraph-deprecated.md)

## Examples

``` r
if (FALSE) { # \dontrun{
plot(worldgraph.10k, reset = TRUE)

## retrieve continent info for all nodes
## (might take a few seconds)
x <- extractFromLayer(worldgraph.10k, layer = "world", attr = "continent")
x
table(getNodesAttr(x, attr.name = "continent"))


## subset Africa
temp <- getNodesAttr(x, attr.name = "continent") == "Africa"
temp[is.na(temp)] <- FALSE
x <- x[temp]
plot(x, reset = TRUE)
} # }
```
