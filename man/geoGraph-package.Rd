% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geograph.R
\docType{package}
\name{geoGraph-package}
\alias{geoGraph-package}
\alias{geoGraph}
\title{The geoGraph package}
\description{
This package implements classes and methods for large-scale georeferenced
data handled through spatial graphs.\cr
}
\details{
Main functionalities of \code{geoGraph} are summarized below.\cr

=== DATA HANDLING ===\cr In \code{geoGraph}, data are stored as a particular
formal class named \linkS4class{gGraph}. This class contains spatial
coordinates of a set of nodes (@coords), attributes for these nodes
(@nodes.attr), meta-information about nodes attributes (@meta), and a graph
of connections between nodes of class graphNEL (@graph).\cr

Several functions are available for handling gGraph data:
\itemize{
\item some accessors allow to access slots of an object, sometimes with
additional treatment of information: \code{\link{getGraph}},
\code{\link{getNodesAttr}}, \code{\link{getCoords}}, \code{\link{getNodes}},
\code{\link{getEdges}}, \code{\link{getCosts}}.\cr
\item \code{\link{setEdges}}: add/remove edges specified edges.\cr
\item \code{\link{setCosts}}: set costs of edges.\cr
\item \code{\link{hasCosts}}: tests if the graph is weighted (i.e., has
non-uniform costs).\cr
\item \code{\link{isInArea}}: finds which nodes are in the currently plotted
area.\cr
\item \code{\link{areConnected}}: tests if nodes are directly connected.\cr
\item \code{\link{connectivityPlot}}: plot connected components with different
colors.\cr
\item \code{\link{dropDeadEdges}}: suppress edges whose weight is null.\cr
\item \code{\link{closestNode}}: given a longitude and a latitude, finds the
closest node; specific values of node attribute can be provided, for
instance, to find the closest node on land.\cr
\item \code{\link{show}}: printing of gGraph objects.\cr
\item \code{\link{extractFromLayer}}: extract information from GIS layers.\cr
\item \code{\link{findLand}}: checks which nodes are on land.\cr
\item \code{\link{setCosts}}: define edges weights accoring to rules specified
in the @meta slot.\cr
\item \code{\link{geo.add.edges}}, \code{\link{geo.remove.edges}}: graphical
functions for adding or removing edges.\cr
\item \code{\link{geo.change.attr}}: graphical functions for changing attributes
of nodes.\cr
}

=== GRAPHICS ===\cr \code{geoGraph} aims at providing advanced graphical
facilities, such as zooming in or out particular area, moving the plotted
area, or visualizing connectivity between nodes. \cr
\itemize{
\item \code{\link{plot}}: plot method with various options, allowing to display
a shapefile (by default, the map of the world), using color according to
attributes, showing connectivity between nodes, etc.\cr
\item \code{\link{points}}: similar to plot method, except that a new plot is
not created.\cr
\item \code{\link{plotEdges}}: the specific function plotting edges. It detects
if the object is a weighted graph, and plots edges accordingly.\cr
\item \code{\link{geo.zoomin}}, \code{\link{geo.zoomout}}: zoom in and out a
plot.\cr
\item \code{\link{geo.back}}: replot the previous screens.\cr
\item \code{\link{geo.slide}}: slide the plotted area toward the indicated
direction.\cr
\item \code{\link{geo.bookmark}}, \code{\link{geo.goto}}: set and goto a
bookmarked area.\cr
}

=== DATASETS ===\cr Datasets occupy a central place in \code{geoGraph},
since they provide the spatial models used in later operations.

Two main datasets are proposed, each being a \linkS4class{gGraph} resulting
from the spliting of the earth into cells of (allmost perfectly) equal
sizes. Two different resolutions are provided:\cr -
\code{\link{worldgraph.10k}}: coverage using about 10,000 nodes\cr -
\code{\link{worldgraph.40k}}: coverage using about 40,000 nodes\cr

Other datasets are:\cr - \code{\link{worldshape}}: shapefile containing
world countries.\cr

To cite geoGraph, please use the reference given by
\code{citation("geoGraph")}.
}
\examples{

## the class gGraph
worldgraph.10k

## plotting the object
plot(worldgraph.10k, reset = TRUE)

## zooming in
geo.zoomin(list(x = c(-6, 38), y = c(35, 73)))
title("Europe")

## to play interactively with graphics, use:
# geo.zoomin()
# geo.zoomout()
# geo.slide()
# geo.back()

## defining a new object restrained to visible nodes
x <- worldgraph.10k[isInArea(worldgraph.10k)]
plot(x, reset = TRUE, edges = TRUE)
title("x does just contain these visible nodes.")

## define weights for edges
x <- setCosts(x, attr.name = "habitat", method = "prod")
plot(x, edges = TRUE)
title("connectivity defined by habitat (land/land=1, other=0)")

## drop 'dead edges' (i.e. with weight 0)
x <- dropDeadEdges(x)
plot(x, edges = TRUE)
title("after droping edges with null weight")

}
\keyword{manip}
\keyword{spatial}
