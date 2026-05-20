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

# S4 method for class 'numeric'
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
  [gGraph](https://evolecolgroup.github.io/geograph/dev/reference/gGraph-class.md),
  or a valid
  [gData](https://evolecolgroup.github.io/geograph/dev/reference/gData-class.md)
  object. For matrix and data.frame, input must have two columns giving
  longitudes and latitudes of locations being considered. For list,
  input must have two components being vectors giving longitudes and
  latitudes of locations.

- ...:

  further arguments to be passed to other methods. Currently not used.

- layer:

  a shapefile of the class `sf` (see
  [`sf::st_read()`](https://r-spatial.github.io/sf/reference/st_read.html)
  to import a GIS shapefile). Alternatively, a character string
  indicating one shapefile released with geoGraph; currently, only
  'world' is available.

- attr:

  a character vector giving names of the variables to be extracted from
  the layer. If 'all', all available variables are extracted. In case of
  problem, available names are displayed with the error message.

## Value

The output depends on the nature of the input:  
- `matrix, data.frame, list`: a data.frame with one row per location,
and as many columns as requested variables ('attributes').  

- `gGraph`: a
  [gGraph](https://evolecolgroup.github.io/geograph/dev/reference/gGraph-class.md)
  object with new node attributes (`@nodes.attr` slot). If nodes
  attributes already existed, new attributes are added as new columns.  

- `gData`: a
  [gData](https://evolecolgroup.github.io/geograph/dev/reference/gData-class.md)
  object with new data associated to locations (`@data` slot). New
  information is merge to older information according to the type of
  data being stored.  

## Details

Nodes can be specified in different ways, including by providing a
[gGraph](https://evolecolgroup.github.io/geograph/dev/reference/gGraph-class.md)
or a
[gData](https://evolecolgroup.github.io/geograph/dev/reference/gData-class.md)
object. Outputs match the input formats.

## Functions

- `extractFromLayer(matrix)`: Method for matrix input

- `extractFromLayer(data.frame)`: Method for data.frames input

- `extractFromLayer(numeric)`: Method for numeric vector input

- `extractFromLayer(list)`: Method for numeric list input

- `extractFromLayer(gGraph)`: Method for numeric gGraph objects

- `extractFromLayer(gData)`: Method for numeric gData objects

## Note

The gGraph method should be carefully used, output is going to be heavy.

## See also

[`findLand`](https://evolecolgroup.github.io/geograph/dev/reference/findLand.md),
to find which locations are on land.

## Examples

``` r

plot(worldgraph.10k, reset = TRUE)


## retrieve continent info for all nodes
## (might take a few seconds)
x <- extractFromLayer(worldgraph.10k, layer = "world", attr = "continent")
#> although coordinates are longitude/latitude, st_intersects assumes that they
#> are planar
x
#> 
#> === gGraph object ===
#> 
#> @coords: spatial coordinates of 10242 nodes
#>         lon       lat
#> 1 -180.0000  90.00000
#> 2  144.0000 -90.00000
#> 3  -33.7806  27.18924
#> ...
#> 
#> @nodes.attr: 2 nodes attributes
#>   habitat continent
#> 1     sea      <NA>
#> 2     sea      <NA>
#> 3     sea      <NA>
#> ...
#> 
#> @meta: list of meta information with 2 items
#> [1] "$colors" "$costs" 
#> 
#> @graph:
#> A graphNEL graph with undirected edges
#> Number of Nodes = 10242 
#> Number of Edges = 6954 
table(getNodesAttr(x, attr.name = "continent"))
#> continent
#>        Africa    Antarctica          Asia        Europe North America 
#>           603           242           628           455           481 
#>       Oceania South America 
#>           170           361 


## subset Africa
temp <- getNodesAttr(x, attr.name = "continent") == "Africa"
temp[is.na(temp)] <- FALSE
x <- x[temp]
plot(x, reset = TRUE)

```
