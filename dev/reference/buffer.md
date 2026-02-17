# Compute buffers around locations for gGraph and gData objects

The generic function `buffer` finds buffers around specified locations
of a
[gGraph](https://evolecolgroup.github.io/geograph/dev/reference/gGraph-class.md)
or a
[gData](https://evolecolgroup.github.io/geograph/dev/reference/gData-class.md)
object. Different format for the output are available.

## Usage

``` r
# S4 method for class 'gGraph'
buffer(x, nodes, d, res.type = c("nodes", "gGraph"), ...)

# S4 method for class 'gData'
buffer(x, d, res.type = c("nodes", "gData", "gGraph"), ...)
```

## Arguments

- x:

  a valid
  [gGraph](https://evolecolgroup.github.io/geograph/dev/reference/gGraph-class.md)
  or
  [gData](https://evolecolgroup.github.io/geograph/dev/reference/gData-class.md)
  object.

- nodes:

  a character vector identifying the nodes around which buffers should
  be computed.

- d:

  the radius of the buffer, in km.

- res.type:

  the type of result that should be returned (see section `value`.

- ...:

  further arguments passed to specific methods.

## Value

The output depends on the value of the argument `res.type`:  

- `nodes`: a vector of characters identifying the nodes of the
  buffers.  

- `gGraph`: a
  [gGraph](https://evolecolgroup.github.io/geograph/dev/reference/gGraph-class.md)
  object with a new attribute "buffer" (TRUE: within buffers; FALSE:
  outside buffers), and new color rules for this attribute in
  `@meta$buf.colors`.  

- `gData`: a
  [gData](https://evolecolgroup.github.io/geograph/dev/reference/gData-class.md)
  object including all the nodes of the buffers.  

## Details

The computed buffers are sets of nodes lying within a given distance of
specified locations. All nodes of a buffer need to be connected to the
location they surround.

## Examples

``` r
#### gGraph example ####
## zoom in to an area
plot(worldgraph.10k, reset = TRUE)
#> Spherical geometry (s2) switched off

geo.zoomin(list(x = c(-6, 38), y = c(35, 73)))

## identify one node
oneNodeXY <- c(getCoords(worldgraph.10k)[9299, 1], getCoords(worldgraph.10k)[9299, 2])
points(oneNodeXY[1], oneNodeXY[2], col = "red")


## find some buffers
buffer(worldgraph.10k, "9299", 100) # nothing around 100km
#> [1] "9299"
buffer(worldgraph.10k, "9299", 500)
#>  [1] "9299" "8979" "9619" "9300" "9298" "8978" "9620" "8980" "8658" "9618"
#> [11] "9940" "9301" "9621" "9297" "8977" "8657" "9941"
buf500km <- buffer(worldgraph.10k, "9299", 500, res = "gGraph")
plot(buf500km, col.rules = buf500km@meta$buf.colors)

buf1000km <- buffer(worldgraph.10k, "9299", 1000, res = "gGraph")
plot(buf1000km, col.rules = buf1000km@meta$buf.colors)



#### gData example ####
x <- hgdp[27:30] # retain a subset of hgdp
plot(x, reset = TRUE, col.g = "lightgrey", pch.node = 20)
buf.200 <- buffer(x, 200, res = "gData")
buf.400 <- buffer(x, 400, res = "gData")
buf.600 <- buffer(x, 600, res = "gData")
buf.1000 <- buffer(x, 1000, res = "gData")
points(buf.1000, col.node = "black")
points(buf.600, col.node = "yellow")
points(buf.400, col.node = "gold")
points(buf.200, col.node = "orange")
title("Different buffers for a gData \n(100km, 200km, 500km)")

```
