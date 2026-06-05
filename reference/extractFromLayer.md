# Retrieves node attributes from a layer

The generic function `extractFromLayer` uses information from a GIS
shapefile to define node attributes. For each node, information is
retrieved from the layer and assigned to that node.  

## Usage

``` r
extractFromLayer(x, ...)

# S4 method for class 'matrix'
extractFromLayer(x, layer = "world", attr = "all", ...)

# S4 method for class 'data.frame'
extractFromLayer(x, layer = "world", attr = "all", ...)

# S4 method for class 'list'
extractFromLayer(x, layer = "world", attr = "all", ...)

# S4 method for class 'gGraph'
extractFromLayer(x, layer = "world", attr = "all", ...)

# S4 method for class 'gData'
extractFromLayer(x, layer = "world", attr = "all", ...)
```

## Arguments

- x:

  a matrix, a data.frame, a list, a valid
  [gGraph](https://evolecolgroup.github.io/geograph/reference/gGraph-class.md),
  or a valid
  [gData](https://evolecolgroup.github.io/geograph/reference/gData-class.md)
  object. For matrix and data.frame, input must have two columns giving
  longitudes and latitudes of locations being considered. For list,
  input must have two components being vectors giving longitudes and
  latitudes of locations.

- ...:

  further arguments to be passed to other methds. Currently not used.

- layer:

  a shapefile of the class `SpatialPolygonsDataFrame` (see
  `readShapePoly` in maptools package to import such data from a GIS
  shapefile). Alternatively, a character string indicating one shapefile
  released with geoGraph; currently, only 'world' is available (see
  `?data(worldshape)`).

- attr:

  a character vector giving names of the variables to be extracted from
  the layer. If 'all', all available variables are extracted. In case of
  problem, available names are displayed with the error message.
  Available data are also stored in `layer@data`.

## Value

The output depends on the nature of the input:  
- `matrix, data.frame, list`: a data.frame with one row per location,
and as many columns as requested variables ('attributes').  

- `gGraph`: a
  [gGraph](https://evolecolgroup.github.io/geograph/reference/gGraph-class.md)
  object with new node attributes (`@nodes.attr` slot). If nodes
  attributes already existed, new attributes are added as new columns.  

- `gData`: a
  [gData](https://evolecolgroup.github.io/geograph/reference/gData-class.md)
  object with new data associated to locations (`@data` slot). New
  information is merge to older information according to the type of
  data being stored.  

## Details

Nodes can be specified in different ways, including by providing a
[gGraph](https://evolecolgroup.github.io/geograph/reference/gGraph-class.md)
or a
[gData](https://evolecolgroup.github.io/geograph/reference/gData-class.md)
object. Outputs match the input formats.

## See also

[`findLand`](https://evolecolgroup.github.io/geograph/reference/findLand.md),
to find which locations are on land.

## Examples

``` r
if (FALSE) { # \dontrun{

plot(worldgraph.10k, reset = TRUE)


## see what info is available
names(worldshape@data)
unique(worldshape@data$CONTINENT)


## retrieve continent info for all nodes
## (might take a few seconds)
x <- extractFromLayer(worldgraph.10k, layer = worldshape, attr = "CONTINENT")
x
table(getNodesAttr(x, attr.name = "CONTINENT"))


## subset Africa
temp <- getNodesAttr(x, attr.name = "CONTINENT") == "Africa"
temp[is.na(temp)] <- FALSE
x <- x[temp]
plot(x, reset = TRUE)
} # }
```
