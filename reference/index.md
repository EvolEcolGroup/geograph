# Package index

## Classes

Classes included in the package.

- [`gGraph-class`](https://evolecolgroup.github.io/geograph/reference/gGraph-class.md)
  [`gGraph`](https://evolecolgroup.github.io/geograph/reference/gGraph-class.md)
  : Formal class "gGraph"
- [`gData-class`](https://evolecolgroup.github.io/geograph/reference/gData-class.md)
  [`gData`](https://evolecolgroup.github.io/geograph/reference/gData-class.md)
  : Formal class "gData"
- [`gPath-class`](https://evolecolgroup.github.io/geograph/reference/gPath-class.md)
  [`gPath`](https://evolecolgroup.github.io/geograph/reference/gPath-class.md)
  : Formal class "gPath"

## Graph construction

Functions for creating new custom `gGraph` objects.

- [`makeHexGrid()`](https://evolecolgroup.github.io/geograph/reference/makeHexGrid.md)
  : Make a new gGraph object from a custom hexagonal grid
- [`makeSquareGrid()`](https://evolecolgroup.github.io/geograph/reference/makeSquareGrid.md)
  : Make a new gGraph object from a custom square grid

## Graph modification

Functions for modifying `gGraph` objects.

- [`assignByRaster()`](https://evolecolgroup.github.io/geograph/reference/assignByRaster.md)
  : Assign raster values to graph nodes
- [`assignByPolygon()`](https://evolecolgroup.github.io/geograph/reference/assignByPolygon.md)
  : Assign node attributes from a polygon layer
- [`setEdges()`](https://evolecolgroup.github.io/geograph/reference/setEdges.md)
  : Add and remove edges from a gGraph object
- [`setNodesAttr()`](https://evolecolgroup.github.io/geograph/reference/setNodesAttr.md)
  : Set node attributes in a gGraph object

## Graph modification (interactive)

Functions for interactively modifying `gGraph` objects.

- [`geo.add.edges()`](https://evolecolgroup.github.io/geograph/reference/geo.add.edges.md)
  : Add and remove edges from a gGraph object
- [`geo.change.attr()`](https://evolecolgroup.github.io/geograph/reference/geo.change.attr.md)
  : Change values of a node attribute

## Accessors

Functions for accessing slots of a `gGraph` or `gData` object.

- [`getColors()`](https://evolecolgroup.github.io/geograph/reference/getColors.md)
  : Get colors associated to nodes of a gGraph object
- [`getCoords()`](https://evolecolgroup.github.io/geograph/reference/getCoords.md)
  : Get coordinates of nodes in a gGraph or gData object
- [`getCosts()`](https://evolecolgroup.github.io/geograph/reference/getCosts.md)
  [`getNodeCosts()`](https://evolecolgroup.github.io/geograph/reference/getCosts.md)
  : Get costs associated to edges of a gGraph object
- [`getData()`](https://evolecolgroup.github.io/geograph/reference/getData.md)
  : Get the data component of a gData object
- [`getEdges()`](https://evolecolgroup.github.io/geograph/reference/getEdges.md)
  : Get edges from a gGraph object
- [`getGraph()`](https://evolecolgroup.github.io/geograph/reference/getGraph.md)
  : Get the graph component of a gGraph or gData object
- [`getNodes()`](https://evolecolgroup.github.io/geograph/reference/getNodes.md)
  : Get nodes of a gGraph or gData object
- [`getNodesAttr()`](https://evolecolgroup.github.io/geograph/reference/getNodesAttr.md)
  : Get nodes attributes from gGraph/gData object
- [`setColors()`](https://evolecolgroup.github.io/geograph/reference/setColors.md)
  : Set color rules for a gGraph object
- [`setGraph()`](https://evolecolgroup.github.io/geograph/reference/setGraph.md)
  : Set the linked gGraph for a gData object

## Connectivity functions

Functions for finding nodes and querying spatial relationships.

- [`areConnected()`](https://evolecolgroup.github.io/geograph/reference/areConnected.md)
  : Test if a set of nodes form a connected set
- [`areNeighbours()`](https://evolecolgroup.github.io/geograph/reference/areNeighbours.md)
  : Tests connectivity between pairs of nodes
- [`isConnected(`*`<gData>`*`)`](https://evolecolgroup.github.io/geograph/reference/isConnected.md)
  [`isConnected(`*`<gGraph>`*`)`](https://evolecolgroup.github.io/geograph/reference/isConnected.md)
  : Test if a set of nodes form a connected set
- [`isReachable()`](https://evolecolgroup.github.io/geograph/reference/isReachable.md)
  : Tests if location reachable from nodes
- [`closestNode()`](https://evolecolgroup.github.io/geograph/reference/closestNode.md)
  : Find the closest node to a given location
- [`isInArea()`](https://evolecolgroup.github.io/geograph/reference/isInArea.md)
  : Find which nodes fall in a given area
- [`findLand()`](https://evolecolgroup.github.io/geograph/reference/findLand.md)
  : Find which nodes are on land
- [`dropDeadEdges()`](https://evolecolgroup.github.io/geograph/reference/dropDeadEdges.md)
  : Get rid of some 'dead' edges or nodes
- [`keepMaxConnectedSet()`](https://evolecolgroup.github.io/geograph/reference/keepMaxConnectedSet.md)
  : Keep only the largest connected set

## Costs

Functions adjusting the costs of a `gGraph` object.

- [`combineCosts()`](https://evolecolgroup.github.io/geograph/reference/combineCosts.md)
  : Combine the costs of two gGraph objects
- [`dropCosts()`](https://evolecolgroup.github.io/geograph/reference/dropCosts.md)
  : Remove all costs from a gGraph object
- [`getCosts()`](https://evolecolgroup.github.io/geograph/reference/getCosts.md)
  [`getNodeCosts()`](https://evolecolgroup.github.io/geograph/reference/getCosts.md)
  : Get costs associated to edges of a gGraph object
- [`hasCosts()`](https://evolecolgroup.github.io/geograph/reference/hasCosts.md)
  : Check if a gGraph has costs
- [`setCosts()`](https://evolecolgroup.github.io/geograph/reference/setCosts.md)
  : Set friction in a gGraph object
- [`setDistCosts()`](https://evolecolgroup.github.io/geograph/reference/setDistCosts.md)
  : Set costs associated to edges based on geographic distances

## Shortest paths

Functions for computing shortest paths and distances.

- [`dijkstraBetween()`](https://evolecolgroup.github.io/geograph/reference/dijkstraBetween.md)
  : Find the shortest path between nodes in a graph
- [`dijkstraBuffer()`](https://evolecolgroup.github.io/geograph/reference/dijkstraBuffer.md)
  : Find nodes reachable within a cost threshold
- [`dijkstraFrom()`](https://evolecolgroup.github.io/geograph/reference/dijkstraFrom.md)
  : Find the minimum cost path
- [`gPath2dist()`](https://evolecolgroup.github.io/geograph/reference/gPath2dist.md)
  : Extract distances from a gPath object
- [`polygonBetween()`](https://evolecolgroup.github.io/geograph/reference/polygonBetween.md)
  : Least-cost paths between two polygons
- [`buffer(`*`<gGraph>`*`)`](https://evolecolgroup.github.io/geograph/reference/buffer.md)
  [`buffer(`*`<gData>`*`)`](https://evolecolgroup.github.io/geograph/reference/buffer.md)
  : Compute buffers around locations for gGraph and gData objects

## Basic plotting

Basic functions for visualising `gGraph` and `gData` objects.

- [`plot(`*`<gData>`*`,`*`<missing>`*`)`](https://evolecolgroup.github.io/geograph/reference/plot-gData.md)
  [`points(`*`<gData>`*`)`](https://evolecolgroup.github.io/geograph/reference/plot-gData.md)
  : Plot a gData object.
- [`plot(`*`<gGraph>`*`,`*`<missing>`*`)`](https://evolecolgroup.github.io/geograph/reference/plot-gGraph.md)
  [`points(`*`<gGraph>`*`)`](https://evolecolgroup.github.io/geograph/reference/plot-gGraph.md)
  [`plotEdges()`](https://evolecolgroup.github.io/geograph/reference/plot-gGraph.md)
  : Plot a gGraph object.
- [`plot(`*`<gPath>`*`)`](https://evolecolgroup.github.io/geograph/reference/plot.gPath.md)
  [`print(`*`<gPath>`*`)`](https://evolecolgroup.github.io/geograph/reference/plot.gPath.md)
  : Plot a gPath object
- [`connectivityPlot()`](https://evolecolgroup.github.io/geograph/reference/connectivityPlot.md)
  : Plot connected sets of a gGraph object
- [`geo.segments()`](https://evolecolgroup.github.io/geograph/reference/geo.segments.md)
  : Plot segments correctly when crossing the antimeridian
- [`zoom`](https://evolecolgroup.github.io/geograph/reference/zoom.md)
  [`geo.zoomin`](https://evolecolgroup.github.io/geograph/reference/zoom.md)
  [`geo.zoomout`](https://evolecolgroup.github.io/geograph/reference/zoom.md)
  [`geo.slide`](https://evolecolgroup.github.io/geograph/reference/zoom.md)
  [`geo.back`](https://evolecolgroup.github.io/geograph/reference/zoom.md)
  [`geo.bookmark`](https://evolecolgroup.github.io/geograph/reference/zoom.md)
  [`geo.goto`](https://evolecolgroup.github.io/geograph/reference/zoom.md)
  [`.zoomlog.up`](https://evolecolgroup.github.io/geograph/reference/zoom.md)
  : Navigate in the plot of a gGraph object

## Advanced plotting

Plotting functions based on ggplot2.

- [`autoplot(`*`<gData>`*`)`](https://evolecolgroup.github.io/geograph/reference/autoplot.gData.md)
  :

  Default ggplot for a `gData`

- [`autoplot(`*`<gGraph>`*`)`](https://evolecolgroup.github.io/geograph/reference/autoplot.gGraph.md)
  :

  Default ggplot for a `gGraph`

- [`geom_gdata()`](https://evolecolgroup.github.io/geograph/reference/geom_gdata.md)
  :

  ggplot layer for a `gData`

- [`geom_ggraph()`](https://evolecolgroup.github.io/geograph/reference/geom_ggraph.md)
  :

  ggplot layer for a `gGraph`

- [`geom_gpath()`](https://evolecolgroup.github.io/geograph/reference/geom_gpath.md)
  : ggplot layer for a gPath

## Example data

Datasets included in the package.

- [`hgdp`](https://evolecolgroup.github.io/geograph/reference/hgdp.md)
  [`hgdpPlus`](https://evolecolgroup.github.io/geograph/reference/hgdp.md)
  : Human genome diversity panel - georeferenced data
- [`worldgraph`](https://evolecolgroup.github.io/geograph/reference/worldgraph.md)
  [`rawgraph.10k`](https://evolecolgroup.github.io/geograph/reference/worldgraph.md)
  [`rawgraph.40k`](https://evolecolgroup.github.io/geograph/reference/worldgraph.md)
  [`worldgraph.10k`](https://evolecolgroup.github.io/geograph/reference/worldgraph.md)
  [`worldgraph.40k`](https://evolecolgroup.github.io/geograph/reference/worldgraph.md)
  : Worldwide geographic graphs
- [`fst_hgdp`](https://evolecolgroup.github.io/geograph/reference/fst_hgdp.md)
  : Pairwise FST matrix for HGDP populations

## Basic Methods

Basic methods to subset and show gGraph and gData objects

- [`subset-gData`](https://evolecolgroup.github.io/geograph/reference/subset-gData.md)
  [`[,gData-method`](https://evolecolgroup.github.io/geograph/reference/subset-gData.md)
  [`[,gData,ANY,ANY-method`](https://evolecolgroup.github.io/geograph/reference/subset-gData.md)
  [`[,gData,ANY,ANY,ANY-method`](https://evolecolgroup.github.io/geograph/reference/subset-gData.md)
  : Subset a gData object
- [`subset-gGraph`](https://evolecolgroup.github.io/geograph/reference/subset-gGraph.md)
  [`[,gGraph-method`](https://evolecolgroup.github.io/geograph/reference/subset-gGraph.md)
  [`[,gGraph,ANY,ANY-method`](https://evolecolgroup.github.io/geograph/reference/subset-gGraph.md)
  [`[,gGraph,ANY,ANY,ANY-method`](https://evolecolgroup.github.io/geograph/reference/subset-gGraph.md)
  : Subset a gGraph object
