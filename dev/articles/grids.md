# Making custom grids in geoGraph

## Using discrete global grids in *geoGraph*

This vignette illustrates how to create custom graphs in *geoGraph*
using discrete global grids. This is especially useful when working over
larger areas of the world, where a rectangular grid would introduce
messy distortions. Using the `dggridR` package, we can create a custom
hexagonal grids that provides a more accurate representation of the
Earth’s surface. We will then use this grid to create a `gGraph` object
in *geoGraph* and assign environmental information to the nodes of the
graph. This allows us to create a cost surface for connectivity analysis
that takes into account both land/water classification and elevation
variability.

#### Getting the elevation data

In the first step, we define the area of interest using a bounding box
and then use the `get_elev_raster` function from the `elevatr` package
to retrieve topographical and bathymetry data for that area. The
resulting data is then converted to a `SpatRaster` object using the
`rast` function from the `terra` package.

``` r

# define the area of interest (Europe)
area.of.interest <- st_as_sfc(
  st_bbox(c(
    xmin = -25,
    xmax = 25,
    ymin = 34,
    ymax = 50
  ), crs = 4326)
)

aoi.bound <- st_sf(geometry = area.of.interest)

elevation.data <- get_elev_raster(
  locations = aoi.bound,
  z = 4,
  clip = "locations"
)

# convert to SpatRaster
spatr <- rast(elevation.data)
plot(spatr)
```

![](grids_files/figure-html/getting%20topographic%20data-1.png)

##### Makeing a land mask

Using this `SpatRaster` we can create a land mask by using the
`make_land_mask` function from the `pastclim` package. This function
identifies land and ocean areas based on the simple logic of moving the
ocean up and down given the current relief profile for the time of
choice. In this case, we use the present time (0 years before present)
to create a land mask for our area of interest.

``` r

# make land mask (everything above 0m is land)
land.mask <- make_land_mask(spatr, time_bp = 0) # using the present time
land.mask[is.na(land.mask)] <- 0
plot(land.mask)
```

![](grids_files/figure-html/making%20land%20mask-1.png)

#### Creating the graph object

Next, we create a new graph object using the `createNewGraph` function
from the `geoGraph` package. We specify the geographic bounding box for
for our area of interest and set the spacing between nodes in the graph.
The resulting graph object represents a grid over the area of interest
with cells of the about 16km spacing, and we can visualize the graph
structure using the `plot` and `plotEdges` functions.

``` r

gGraph <- createNewGraph(geo.box = aoi.bound, spacing = 20)
```

    ## Resolution: 11, Area (km^2): 287.933536398634, Spacing (km): 16.758963498128, CLS (km): 19.147021538141

``` r

plot(gGraph, reset = TRUE)
plotEdges(gGraph)
```

![](grids_files/figure-html/creating%20graph%20object-1.png)

#### Adding the land/water mask to the graph

We then use the `assignRasterPoints` function to add the land/water mask
to the graph. This function takes the graph object and the land mask
raster as inputs and assigns the land/water values to the corresponding
cells in the graph. In this example we classify each cell with land
points in it as land by defining the `fun` parameter in
`collapseNodeAttribute` as ‘min’, which means that if there is at least
one water point in the cell, the cell will be classified as water. We
can then visualize the resulting graph with the land/water mask by
plotting the graph and coloring the nodes based on the land/water
values.

``` r

waterGraph <- assignRasterPoints(
  graph = gGraph,
  raster = land.mask,
  layer.name = "water"
)

# now we collapse the water attribute to get a land/sea classification for each node
waterLandGraph <- collapseNodeAttribute(
  graph = waterGraph,
  attribute = "water",
  fun = min,
  na.rm = TRUE
)

# add this to the habitat attribute
habitat <- factor(waterLandGraph@nodes.attr$water,
  levels = c(0, 1),
  labels = c("sea", "land")
)

# reclassify the nodes in the gGraph object accordingly in the metadata
waterLandGraph@nodes.attr$habitat <- habitat # TODO can we use a setAttribute here?

colors <- data.frame(
  habitat = c("sea", "land"),
  cost = c("blue", "green")
)

# change colors associated with the land meta info
waterLandGraph@meta$colors <- colors

plot(waterLandGraph, reset = TRUE)
```

![](grids_files/figure-html/unnamed-chunk-1-1.png)

##### Setting costs for the different habitat types

Finally, we can set the costs for moving through different habitat types
based on their classification. Note that the cost here is arbitrary and
can be adjusted based on the specific context of the analysis and the
species or features being studied. Costs are inversely related to the
ease of movement through a cell, so higher costs indicate more difficult
terrain. For example, we can assign a cost of 10 for moving through sea
cells and a cost of 1 for moving through land cells. If we now look at
the least cost path between two cities (Madrid and Naples) using the
`dijkstraBetween` function, we can see that the path tends to avoid sea
cells and prefers to move through land cells.

``` r

# set the costs for sea cells to 10 and land cells to 1
sea.cost <- 10
land.cost <- 1
waterLandGraph@meta$costs <- data.frame(
  habitat = c("sea", "land"),
  cost = c(sea.cost, land.cost)
)

waterLandGraph <- setCosts(waterLandGraph, method = "mean", attr.name = "habitat")


point1 <- c(-3.7038, 40.4168) # Madrid
point2 <- c(14.2681, 40.8522) # Naples
cities.dat <- rbind.data.frame(point1, point2)
colnames(cities.dat) <- c("lon", "lat")
row.names(cities.dat) <- c("Madrid", "Naples")
cities <- new("gData", coords = cities.dat[, 1:2], gGraph.name = "waterLandGraph")
path <- dijkstraBetween(cities)
plot(waterLandGraph, reset = TRUE)
plot(path, col = "red", lwd = 2)
```

![](grids_files/figure-html/least%20cost%20path-1.png)

#### Assigning elevation data to graph nodes

In the next step we can then use the `assignRasterPoints` function again
to add elevation data to the nodes of the graph. In this case we need to
chose how to incorporate the elevation data into the graph, since each
node will have multiple raster points associated with it. So before
using the `collapseNodeAttribute` function to get a single elevation
value for each node, we can first have a look at the standard deviation
of the elevation values for each node, which gives us an idea of the
variability of elevation within each cell. This can be useful for
identifying rugged areas, which typically have high variability in
elevation and a higher cost of traveling through associated. For this we
set the `fun` parameter in the `collapseNodeAttribute` function to `sd`,
which calculates the standard deviation of the elevation values for each
node. We can then visualize the distribution of the standard deviation
of elevation values for land cells by plotting a density plot.

``` r

elevationGraph <- assignRasterPoints(
  graph = waterLandGraph,
  raster = spatr,
  layer.name = "elevation"
)

# now we collapse the elevation attribute to get the sd of the elevation for each node
elevationGraph <- collapseNodeAttribute(
  graph = elevationGraph,
  attribute = "elevation",
  fun = sd,
  na.rm = TRUE
)

# have a look at the distribution of sd.elevation values for land cells
sd.ele <- getNodesAttr(elevationGraph) |>
  mutate(
    elevation = ifelse(habitat == "land", elevation, NA_real_)
  ) |>
  pull(elevation)

# plot the density
plot(density(log(sd.ele), na.rm = TRUE),
  main = "Density of SD Elevation for Land Cells",
  xlab = "SD Elevation (m)", ylab = "Density"
)
```

![](grids_files/figure-html/assigning%20elevation%20data-1.png)

#### Reclassify the nodes into ‘rugged’ and ‘very rugged’

Looking at the distribution of the standard deviation of elevation
values for land cells, we can see that there are some cells with very
high variability in elevation, which likely correspond to mountainous
areas. We can then reclassify the nodes into ‘rugged’ and ‘very rugged’
based on an elevation threshold. Here we pick arbitrary thresholds for
the standard deviation of elevation values to classify cells as ‘rugged’
and ‘very rugged’. Cells with a standard deviation of elevation greater
than 100 are classified as ‘rugged’, while those with a standard
deviation greater than 150 are classified as ‘very rugged’. We can then
update the habitat attribute in the graph accordingly and visualize the
resulting graph with the new habitat classifications.

``` r

# set a threshold for rugged cells
rugged.threshold <- 150
very.rugged.threshold <- 300

## now we identify rugged and very rugged cells as those with sd.elevation > threshold
habitat.new <- case_when(
  is.na(sd.ele) ~ "sea",
  sd.ele > very.rugged.threshold ~ "very_rugged",
  sd.ele > rugged.threshold ~ "rugged",
  TRUE ~ "land"
)

# update the habitat attribute in the graph
# reclassify the nodes in the gGraph object accordingly in the metadata
elevationGraph@nodes.attr$habitat <- habitat.new


colors <- data.frame(
  habitat = c("sea", "land", "rugged", "very_rugged"),
  cost = c("blue", "green", "orange", "brown")
)

# change colors associated with the land meta info
elevationGraph@meta$colors <- colors

plot(elevationGraph, reset = TRUE)
```

![](grids_files/figure-html/reclassifying%20nodes-1.png)

Now if we now set the cost for rugged and very rugged cells to be higher
than for land cells, we can see that the least cost paths between
different land areas tend to avoid not only sea cells but also rugged
and very rugged cells. This can be shown in the same toy example as
above.

``` r

# set the costs for sea cells to 10, land cells to 1, rugged cells to 3 and very rugged cells to 10
sea.cost <- 10
land.cost <- 1
rugged.cost <- 3
very.rugged.cost <- 10
elevationGraph@meta$costs <- data.frame(
  habitat = c("sea", "land", "rugged", "very_rugged"),
  cost = c(sea.cost, land.cost, rugged.cost, very.rugged.cost)
)
elevationGraph <- setCosts(elevationGraph, method = "mean", attr.name = "habitat")
point1 <- c(-3.7038, 40.4168) # Madrid
point2 <- c(14.2681, 40.8522) # Naples
cities.dat <- rbind.data.frame(point1, point2)
colnames(cities.dat) <- c("lon", "lat")
row.names(cities.dat) <- c("Madrid", "Naples")
cities <- new("gData", coords = cities.dat[, 1:2], gGraph.name = "elevationGraph")
path <- dijkstraBetween(cities)
plot(elevationGraph, reset = TRUE)
plot(path, col = "red", lwd = 2)
```

![](grids_files/figure-html/least%20cost%20path%20with%20ruggedness-1.png)

#### Adding coastal cells

Now in a last step we can add a ‘coast’ category to the habitat
classification for cells that are adjacent to any land based cells. This
is done by checking the neighbors of each sea cell and reclassifying
those that have at least one land or mountain neighbor as ‘coast’. We
can then visualize the resulting graph with the new habitat
classifications, including the coastal cells.

``` r

# get the neighbor list from the graph
neigh.list <- elevationGraph@graph@edgeL
node.attr <- getNodesAttr(elevationGraph)

# get all nodes adjacent to land or mountain cells and reclassify them as "coast"
for (i_node in seq_len(nrow(node.attr))) {
  if (node.attr$habitat[i_node] == "sea") {
    neighbour.indices <- neigh.list[[i_node]]$edges # check structure!
    neighbour.land.values <- node.attr$habitat[neighbour.indices]
    if (any(neighbour.land.values %in% c("land", "rugged", "very_rugged"))) {
      # at least one land or mountain neighbour
      node.attr$habitat[i_node] <- "coast"
    }
  }
}

new.attribute <- node.attr

# create the fullGraph
fullGraph <- elevationGraph

# set the new node attributes
fullGraph@nodes.attr <- new.attribute


colors <- data.frame(
  habitat = c("sea", "land", "rugged", "very_rugged", "coast"),
  cost = c("blue", "green", "orange", "brown", "lightblue")
)

# change the costs and colors associated with the land meta info
fullGraph@meta$colors <- colors
plot(fullGraph, reset = TRUE)
```

![](grids_files/figure-html/adding%20coastal%20cells-1.png)

Now if we set the cost for coastal cells to be the same as for land
cells, we can see that the least cost paths between different land areas
tend to prefer crossing coastal cells over crossing rugged areas.

``` r

# set the costs for sea cells to 10, land cells to 1, rugged cells to 3, very rugged cells to 10 and coastal cells to 1
sea.cost <- 10
land.cost <- 1
rugged.cost <- 3
very.rugged.cost <- 10
coastal.cost <- 1
fullGraph@meta$costs <- data.frame(
  habitat = c("sea", "land", "rugged", "very_rugged", "coast"),
  cost = c(sea.cost, land.cost, rugged.cost, very.rugged.cost, coastal.cost)
)
fullGraph <- setCosts(fullGraph, method = "mean", attr.name = "habitat")

point1 <- c(-3.7038, 40.4168) # Madrid
point2 <- c(14.2681, 40.8522) # Naples
cities.dat <- rbind.data.frame(point1, point2)
colnames(cities.dat) <- c("lon", "lat")
row.names(cities.dat) <- c("Madrid", "Naples")
cities <- new("gData", coords = cities.dat[, 1:2], gGraph.name = "fullGraph")
path <- dijkstraBetween(cities)
plot(fullGraph, reset = TRUE)
plot(path, col = "red", lwd = 2)
```

![](grids_files/figure-html/least%20cost%20path%20with%20coastal%20cells-1.png)

#### Using different methods to calculate costs

So far the costs for moving through different habitat types were defined
by the mean of the costs associated with the different habitat types.
However, the `setCosts` function allows to use different methods to
calculate the costs for each edge based on the node attributes. For
example, instead of using the mean, we could specify a custom function
that takes the maximum cost of the two nodes connected by an edge, which
would mean that the cost of moving through an edge is determined by the
more difficult habitat type of the two nodes. This can be done by
defining a custom function `max.cost` and then using it in the
`setCosts` function with the method set to “function”. The resulting
graph will have costs for each edge that reflect the maximum cost of the
two nodes it connects.

``` r

max.cost <- function(x1, x2) {
  pmax(x1, x2)
}

fullGraph <- setCosts(
  fullGraph,
  attr.name = "habitat",
  method = "function",
  FUN = max.cost
)

plot(fullGraph, reset = TRUE)
plotEdges(fullGraph)
```

![](grids_files/figure-html/setting%20costs-1.png)

#### Combining costs

In many ecological applications, connectivity across landscapes depends
on multiple environmental constraints rather than a single variable. In
the previous sections we constructed terrain-based costs that capture
differences in movement through sea, land, rugged, and very rugged
areas. However, climatic factors can also influence movement and
dispersal. To illustrate how multiple environmental layers can be
incorporated into a connectivity model, we now add an additional
environmental gradient: `temperature`, which in this example is a random
variable that we assign to the nodes of the graph. The movement cost
between two nodes can then be defined as a function of the difference in
temperature between the two nodes, with a cost that increases as the
temperature difference increases. The `setCosts` function is then used
to apply this cost function to the graph based on the temperature values
assigned to each node.

``` r

exp.cost <-
  exp.cost <- function(x1, x2, cost.coeff) {
    exp(-abs(x1 - x2) * cost.coeff)
  }

# create a set of node costs for temperature
landGraph <- dropDeadEdges(fullGraph, thres = 9)
landGraph@nodes.attr$temp <- runif(n = 25821)
temperatureGraph <-
  setCosts(
    landGraph,
    node.values = landGraph@nodes.attr$temp,
    method = "function",
    FUN = exp.cost,
    cost.coeff = 1
  )

plot(temperatureGraph, edge = TRUE)
plotEdges(temperatureGraph)
```

![](grids_files/figure-html/unnamed-chunk-2-1.png)

Now we want a combined cost that both captures the temperature as well
as the terrain. For this we can use the `combineCosts` function, which
allows to combine costs from two different graphs using a specified
method (sum, product, or a custom function). Here we will use the `prod`
method to combine the costs from the `temperatureGraph` and the
`landGraph`, which multiplies the costs from both graphs to create a new
cost that reflects both the temperature and terrain constraints on
movement.

``` r

combineCostsGraph <- combineCosts(temperatureGraph, landGraph, method = "prod")

plot(combineCostsGraph, edge = TRUE)
plotEdges(combineCostsGraph)
title("Combined costs graph")
```

![](grids_files/figure-html/unnamed-chunk-3-1.png)

### Extracting information from GIS shapefiles

In the previous sections, we extracted environmental information from
raster layers and assigned it to graph nodes using `assignRasterPoints.`
Another way *geoGraph* can serve as an interface between geographic
information system (GIS) layers and geographic data is the function
`extractFromLayer`. *geoGraph* uses `sf` objects to represent geographic
objects such as points and polygons. By default, *geoGraph* uses the
package *rnaturalearth* to provide continent and country outlines, but
it is possible also to load custom GIS shapefiles with
[`sf::st_read()`](https://r-spatial.github.io/sf/reference/st_read.html).

We start by loading country outlines for the whole world. Note that we
turn off spherical trigonometry functions with `sf::sf_use_s2(FALSE)`,
as the `naturalearth` dataset is not compatible with that functionality.

``` r

library(sf)
sf_use_s2(FALSE)
world.countries <- rnaturalearth::ne_countries(
  scale = "medium",
  returnclass = "sf"
)
```

Let us assume that we are interested in add continent and country
information to the `fullGraph` object.

``` r

newGraph <- extractFromLayer(fullGraph,
  layer = world.countries,
  attr = c("continent", "name")
)
summary(getNodesAttr(newGraph))
```

    ##      water             habitat        elevation           continent    
    ##  Min.   :0.0000   Length   :25821   Min.   :   0.00   Length   :25821  
    ##  1st Qu.:0.0000   N.unique :    5   1st Qu.:  31.60   N.unique :    2  
    ##  Median :0.0000   N.blank  :    0   Median :  78.48   N.blank  :    0  
    ##  Mean   :0.3725   Min.nchar:    3   Mean   : 115.07   Min.nchar:    6  
    ##  3rd Qu.:1.0000   Max.nchar:   11   3rd Qu.: 154.74   Max.nchar:    6  
    ##  Max.   :1.0000                     Max.   :1298.22   NAs      :15694  
    ##  NAs    :45                         NAs    :83                         
    ##         name      
    ##  Length   :25821  
    ##  N.unique :   32  
    ##  N.blank  :    0  
    ##  Min.nchar:    5  
    ##  Max.nchar:   16  
    ##  NAs      :15694  
    ## 

The new object `newGraph` is a `gGraph` which now includes, for each
node of the grid, the corresponding continent and country retrieved from
the GIS layer. Note that `extractFromLayer` can extract information to
other types of objects than `gGraph` (see
[`?extractFromLayer`](https://evolecolgroup.github.io/geograph/dev/reference/extractFromLayer.md))

We can use the newly acquired information for plotting `newGraph`, by
defining new color rules:

``` r

temp <- unique(getNodesAttr(newGraph)$"name")
col <- c("transparent", rainbow(length(temp) - 1))
colMat <- data.frame(name = temp, color = col)
head(colMat)
```

    ##       name       color
    ## 1     <NA> transparent
    ## 2   France     #FF0000
    ## 3   Jersey     #FF3000
    ## 4    Spain     #FF6000
    ## 5 Portugal     #FF8F00
    ## 6  Morocco     #FFBF00

``` r

tail(colMat)
```

    ##               name   color
    ## 28          Kosovo #DF00FF
    ## 29           Malta #FF00EF
    ## 30        Bulgaria #FF00BF
    ## 31 North Macedonia #FF008F
    ## 32          Greece #FF0060
    ## 33         Ukraine #FF0030

``` r

plot(newGraph, col.rules = colMat, reset = TRUE)
```

![](grids_files/figure-html/unnamed-chunk-6-1.png)

This information could in turn be used to define costs for traveling on
the grid. For instance, one could import habitat descriptors from a GIS,
use these values to formulate a habitat model, and derive costs for
dispersal on the grid.

#### Look at connectivity between two polygons

``` r

test <- polygonBetween(newGraph, layer = "name", "Andorra", "Portugal", outline = FALSE)
plot(newGraph, col = NA, reset = TRUE)
plot(test)
```

![](grids_files/figure-html/unnamed-chunk-7-1.png)
