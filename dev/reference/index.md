# Package index

## Graph construction

Functions for creating new custom `gGraph` objects.

- [`createNewGraph()`](https://evolecolgroup.github.io/geograph/dev/reference/createNewGraph.md)
  : Make a new gGraph object from a custom discrete global grid
- [`makeGrid()`](https://evolecolgroup.github.io/geograph/dev/reference/makeGrid.md)
  : Build a regular grid gGraph

## Graph modification

Functions for modifying `gGraph` objects.

- [`assignRasterPoints()`](https://evolecolgroup.github.io/geograph/dev/reference/assignRasterPoints.md)
  : Assign raster points to graph nodes
- [`extractFromLayer()`](https://evolecolgroup.github.io/geograph/dev/reference/extractFromLayer.md)
  : Retrieves node attributes from a layer
- [`collapseNodeAttribute()`](https://evolecolgroup.github.io/geograph/dev/reference/collapseNodeAttribute.md)
  : Collapse a list-based node attribute into a scalar node attribute
- [`setEdges()`](https://evolecolgroup.github.io/geograph/dev/reference/setEdges.md)
  : Add and remove edges from a gGraph object
- [`setNodesAttr()`](https://evolecolgroup.github.io/geograph/dev/reference/setNodesAttr.md)
  : Set node attributes in a gGraph object

## Graph modification (interactive)

Functions for interactively modifying `gGraph` objects.

- [`geo.add.edges()`](https://evolecolgroup.github.io/geograph/dev/reference/geo.add.edges.md)
  : Add and remove edges from a gGraph object
- [`geo.change.attr()`](https://evolecolgroup.github.io/geograph/dev/reference/geo.change.attr.md)
  : Change values of a node attribute

## Accessors

Functions for accessing slots of a `gGraph` or `gData` object.

- [`getColors()`](https://evolecolgroup.github.io/geograph/dev/reference/getColors.md)
  : Get colors associated to nodes of a gGraph object
- [`getCoords()`](https://evolecolgroup.github.io/geograph/dev/reference/getCoords.md)
  : Get coordinates of nodes in a gGraph or gData object
- [`getCosts()`](https://evolecolgroup.github.io/geograph/dev/reference/getCosts.md)
  [`getNodeCosts()`](https://evolecolgroup.github.io/geograph/dev/reference/getCosts.md)
  : Get costs associated to edges of a gGraph object
- [`getData()`](https://evolecolgroup.github.io/geograph/dev/reference/getData.md)
  : Get the data component of a gData object
- [`getEdges()`](https://evolecolgroup.github.io/geograph/dev/reference/getEdges.md)
  : Get edges from a gGraph object
- [`getGraph()`](https://evolecolgroup.github.io/geograph/dev/reference/getGraph.md)
  : Get the graph component of a gGraph or gData object
- [`getNodes()`](https://evolecolgroup.github.io/geograph/dev/reference/getNodes.md)
  : Get nodes of a gGraph or gData object
- [`getNodesAttr()`](https://evolecolgroup.github.io/geograph/dev/reference/getNodesAttr.md)
  : Get nodes attributes from gGraph/gData object

## Connectivity functions

Functions for finding nodes and querying spatial relationships.

- [`areConnected()`](https://evolecolgroup.github.io/geograph/dev/reference/areConnected.md)
  : Test if a set of nodes form a connected set
- [`areNeighbours()`](https://evolecolgroup.github.io/geograph/dev/reference/areNeighbours.md)
  : Tests connectivity between pairs of nodes
- [`isConnected(`*`<gData>`*`)`](https://evolecolgroup.github.io/geograph/dev/reference/isConnected.md)
  [`isConnected(`*`<gGraph>`*`)`](https://evolecolgroup.github.io/geograph/dev/reference/isConnected.md)
  : Test if a set of nodes form a connected set
- [`isReachable()`](https://evolecolgroup.github.io/geograph/dev/reference/isReachable.md)
  : Tests if location reachable from nodes
- [`closestNode()`](https://evolecolgroup.github.io/geograph/dev/reference/closestNode.md)
  : Find the closest node to a given location
- [`isInArea()`](https://evolecolgroup.github.io/geograph/dev/reference/isInArea.md)
  : Find which nodes fall in a given area
- [`findLand()`](https://evolecolgroup.github.io/geograph/dev/reference/findLand.md)
  : Find which nodes are on land
- [`dropDeadEdges()`](https://evolecolgroup.github.io/geograph/dev/reference/dropDeadEdges.md)
  : Get rid of some 'dead' edges or nodes
- [`keepMaxConnectedSet()`](https://evolecolgroup.github.io/geograph/dev/reference/keepMaxConnectedSet.md)
  : Keep only the largest connected set

## Costs

Functions adjusting the costs of a `gGraph` object.

- [`hasCosts()`](https://evolecolgroup.github.io/geograph/dev/reference/hasCosts.md)
  : Check if a gGraph has costs
- [`setCosts()`](https://evolecolgroup.github.io/geograph/dev/reference/setCosts.md)
  : Set friction in a gGraph object
- [`setDistCosts()`](https://evolecolgroup.github.io/geograph/dev/reference/setDistCosts.md)
  : Set costs associated to edges based on geographic distances
- [`combineCosts()`](https://evolecolgroup.github.io/geograph/dev/reference/combineCosts.md)
  : Combine the costs of two gGraph objects
- [`dropCosts()`](https://evolecolgroup.github.io/geograph/dev/reference/dropCosts.md)
  : Remove all costs from a gGraph object

## Shortest paths

Functions for computing shortest paths and distances.

- [`plot(`*`<gPath>`*`)`](https://evolecolgroup.github.io/geograph/dev/reference/dijkstra-methods.md)
  [`dijkstraFrom(`*`<gGraph>`*`)`](https://evolecolgroup.github.io/geograph/dev/reference/dijkstra-methods.md)
  [`dijkstraFrom(`*`<gData>`*`)`](https://evolecolgroup.github.io/geograph/dev/reference/dijkstra-methods.md)
  : Shortest path using Dijkstra algorithm
- [`dijkstraBetween()`](https://evolecolgroup.github.io/geograph/dev/reference/dijkstraBetween.md)
  : Find the shortest path between nodes in a graph
- [`dijkstraFrom()`](https://evolecolgroup.github.io/geograph/dev/reference/dijkstraFrom.md)
  : Find the minimum cost path
- [`gPath2dist()`](https://evolecolgroup.github.io/geograph/dev/reference/gPath2dist.md)
  : Extract distances from a gPath object
- [`polygonBetween()`](https://evolecolgroup.github.io/geograph/dev/reference/polygonBetween.md)
  : Least-cost paths between two polygons
- [`buffer(`*`<gGraph>`*`)`](https://evolecolgroup.github.io/geograph/dev/reference/buffer.md)
  [`buffer(`*`<gData>`*`)`](https://evolecolgroup.github.io/geograph/dev/reference/buffer.md)
  : Compute buffers around locations for gGraph and gData objects
- [`nodeBuffer()`](https://evolecolgroup.github.io/geograph/dev/reference/nodeBuffer.md)
  : node buffer

## Plotting

Functions for visualising `gGraph` and `gData` objects.

- [`plot(`*`<gGraph>`*`,`*`<missing>`*`)`](https://evolecolgroup.github.io/geograph/dev/reference/plot-gGraph.md)
  [`points(`*`<gGraph>`*`)`](https://evolecolgroup.github.io/geograph/dev/reference/plot-gGraph.md)
  [`plotEdges()`](https://evolecolgroup.github.io/geograph/dev/reference/plot-gGraph.md)
  : Plot a gGraph object.
- [`plot(`*`<gData>`*`,`*`<missing>`*`)`](https://evolecolgroup.github.io/geograph/dev/reference/plot-gData.md)
  [`points(`*`<gData>`*`)`](https://evolecolgroup.github.io/geograph/dev/reference/plot-gData.md)
  : Plot a gData object.
- [`connectivityPlot()`](https://evolecolgroup.github.io/geograph/dev/reference/connectivityPlot.md)
  : Plot connected sets of a gGraph object
- [`geo.segments()`](https://evolecolgroup.github.io/geograph/dev/reference/geo.segments.md)
  : Plot segments correctly when crossing the antimeridian
- [`zoom`](https://evolecolgroup.github.io/geograph/dev/reference/zoom.md)
  [`geo.zoomin`](https://evolecolgroup.github.io/geograph/dev/reference/zoom.md)
  [`geo.zoomout`](https://evolecolgroup.github.io/geograph/dev/reference/zoom.md)
  [`geo.slide`](https://evolecolgroup.github.io/geograph/dev/reference/zoom.md)
  [`geo.back`](https://evolecolgroup.github.io/geograph/dev/reference/zoom.md)
  [`geo.bookmark`](https://evolecolgroup.github.io/geograph/dev/reference/zoom.md)
  [`geo.goto`](https://evolecolgroup.github.io/geograph/dev/reference/zoom.md)
  [`.zoomlog.up`](https://evolecolgroup.github.io/geograph/dev/reference/zoom.md)
  : Navigate in the plot of a gGraph object

## Example data

Datasets included in the package.

- [`hgdp`](https://evolecolgroup.github.io/geograph/dev/reference/hgdp.md)
  [`hgdpPlus`](https://evolecolgroup.github.io/geograph/dev/reference/hgdp.md)
  : Human genome diversity panel - georeferenced data
- [`worldgraph`](https://evolecolgroup.github.io/geograph/dev/reference/worldgraph.md)
  [`rawgraph.10k`](https://evolecolgroup.github.io/geograph/dev/reference/worldgraph.md)
  [`rawgraph.40k`](https://evolecolgroup.github.io/geograph/dev/reference/worldgraph.md)
  [`worldgraph.10k`](https://evolecolgroup.github.io/geograph/dev/reference/worldgraph.md)
  [`worldgraph.40k`](https://evolecolgroup.github.io/geograph/dev/reference/worldgraph.md)
  : Worldwide geographic graphs

## Validation

Functions for checking object validity.

- [`gGraph-class`](https://evolecolgroup.github.io/geograph/dev/reference/gGraph-class.md)
  [`gGraph`](https://evolecolgroup.github.io/geograph/dev/reference/gGraph-class.md)
  [`[,gGraph-method`](https://evolecolgroup.github.io/geograph/dev/reference/gGraph-class.md)
  [`[,gGraph,ANY,ANY-method`](https://evolecolgroup.github.io/geograph/dev/reference/gGraph-class.md)
  [`[,gGraph,ANY,ANY,ANY-method`](https://evolecolgroup.github.io/geograph/dev/reference/gGraph-class.md)
  [`initialize,gGraph-method`](https://evolecolgroup.github.io/geograph/dev/reference/gGraph-class.md)
  [`show,gGraph-method`](https://evolecolgroup.github.io/geograph/dev/reference/gGraph-class.md)
  [`is.gGraph`](https://evolecolgroup.github.io/geograph/dev/reference/gGraph-class.md)
  : Formal class "gGraph"
- [`gData-class`](https://evolecolgroup.github.io/geograph/dev/reference/gData-class.md)
  [`gData`](https://evolecolgroup.github.io/geograph/dev/reference/gData-class.md)
  [`[,gData-method`](https://evolecolgroup.github.io/geograph/dev/reference/gData-class.md)
  [`[,gData,ANY,ANY-method`](https://evolecolgroup.github.io/geograph/dev/reference/gData-class.md)
  [`[,gData,ANY,ANY,ANY-method`](https://evolecolgroup.github.io/geograph/dev/reference/gData-class.md)
  [`initialize,gData-method`](https://evolecolgroup.github.io/geograph/dev/reference/gData-class.md)
  [`show,gData-method`](https://evolecolgroup.github.io/geograph/dev/reference/gData-class.md)
  [`is.gData`](https://evolecolgroup.github.io/geograph/dev/reference/gData-class.md)
  : Formal class "gData"
