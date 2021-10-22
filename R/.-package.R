

#' Auxiliary methods for geoGraph
#' 
#' These methods are low-level functions called by other procedures of
#' \code{geoGraph}. Some can, however, be useful in themselves. Note that
#' unlike other functions in \code{geoGraph}, these functions do not generally
#' test for the validity of the provided arguments (for speed purposes).\cr
#' 
#' - \code{hasCosts}: tests whether a \linkS4class{gGraph} has costs associated
#' to its edges.\cr
#' 
#' - \code{geo.segments}: a substitute to \code{segments} which correctly draws
#' segments between locations distant by more than 90 degrees of longitude.\cr
#' 
#' - \code{rebuild}: in development.
#' 
#' 
#' @aliases hasCosts rebuild geo.segments
#' @param x a valid \linkS4class{gGraph}.
#' @param x0,y0 coordinates of points *from* which to draw.
#' @param x1,y1 coordinates of points *to* which to draw.
#' @param col a character string or an integer indicating the color of the
#' segments.
#' @param lty a character string or an integer indicating the type of line.
#' @param lwd an integer indicating the line width.
#' @param \dots further graphical parameters (from 'par') passed to the
#' \code{segments} function.
#' @return For \code{hasCost}, a logical value is returned. \code{geo.segments}
#' returns NULL.
#' @author Thibaut Jombart (\email{t.jombart@@imperial.ac.uk})
#' @keywords utilities methods
#' @examples
#' 
#' hasCosts(worldgraph.10k)
#' 
NULL





#' Compute buffers around locations for gGraph and gData objects
#' 
#' The generic function \code{buffer} finds buffers around specified locations
#' of a \linkS4class{gGraph} or a \linkS4class{gData} object. Different format
#' for the output are available.
#' 
#' The computed buffers are sets of nodes lying within a given distance of
#' specified locations. All nodes of a buffer need to be connected to the
#' location they surround.
#' 
#' 
#' @aliases buffer buffer-methods buffer,gGraph-method buffer,gData-method
#' @param x a valid \linkS4class{gGraph} or \linkS4class{gData} object.
#' @param \dots further arguments passed to specific methods.
#' @param nodes a character vector identifying the nodes aournd which buffers
#' should be computed.
#' @param d the radius of the buffer, in km.
#' @param res.type the type of result that should be returned (see section
#' \code{value}.
#' @return The output depends on the value of the argument \code{res.type}:\cr
#' - \code{nodes}: a vector of characters identifying the nodes of the
#' buffers.\cr
#' 
#' - \code{gGraph}: a \linkS4class{gGraph} object with a new attribute "buffer"
#' (TRUE: within buffers; FALSE: outside buffers), and new color rules for this
#' attribute in \code{@meta$buf.colors}.\cr
#' 
#' - \code{gData}: a \linkS4class{gData} object including all the nodes of the
#' buffers.\cr
#' @author Thibaut Jombart (\email{t.jombart@@imperial.ac.uk})
#' @keywords utilities methods
#' @examples
#' 
#' #### gGraph example ####
#' ## zoom in to an area
#' plot(worldgraph.10k, reset=TRUE)
#' geo.zoomin(list(x=c(-6,38), y=c(35,73)))
#' 
#' ## identify one node
#' oneNodeXY <- c(getCoords(worldgraph.10k)[9299,1],getCoords(worldgraph.10k)[9299,2])
#' points(oneNodeXY[1],  oneNodeXY[2], col="red")
#' 
#' ## find some buffers
#' buffer(worldgraph.10k, "9299", 100) # nothing around 100km
#' buffer(worldgraph.10k, "9299", 500)
#' buf500km <- buffer(worldgraph.10k, "9299", 500, res="gGraph")
#' plot(buf500km, col.rules=buf500km@meta$buf.colors)
#' buf1000km <- buffer(worldgraph.10k, "9299", 1000, res="gGraph")
#' plot(buf1000km, col.rules=buf1000km@meta$buf.colors)
#' 
#' 
#' #### gData example ####
#' x <- hgdp[27:30] # retain a subset of hgdp 
#' plot(x, reset=TRUE, col.g="lightgrey", pch.node=20)
#' buf.200 <- buffer(x, 200, res="gData")
#' buf.400 <- buffer(x, 400, res="gData")
#' buf.600 <- buffer(x, 600, res="gData")
#' buf.1000 <- buffer(x, 1000, res="gData")
#' points(buf.1000, col.node="black")
#' points(buf.600, col.node="yellow")
#' points(buf.400, col.node="gold")
#' points(buf.200, col.node="orange")
#' title("Different buffers for a gData \n(100km, 200km, 500km)")
#' 
NULL





#' Find the closest node to a given location
#' 
#' The function \code{closestNode} searches for the closest node in a
#' \linkS4class{gGraph} or a \linkS4class{gData} object to a given location. It
#' is possible to restrain the research to given values of a node attribute.
#' For instance, one can search the closest node on land to a given
#' location.\cr
#' 
#' This function is also used to match locations of a \linkS4class{gData}
#' object with nodes of the \code{gGraph} object to which it is linked.
#' 
#' When creating a \linkS4class{gData} object, if the \code{gGraph.name}
#' argument is provided, then locations are matched with the \code{gGraph}
#' object automatically, by an internal call to closestNode. Note, however,
#' that it is not possible to specify node attributes (\code{attr.names} and
#' \code{attr.values}) this way.
#' 
#' @aliases closestNode closestNode-methods closestNode,gGraph-method
#' closestNode,gData-method
#' @param x a valid \linkS4class{gGraph} or \linkS4class{gData} object. In the
#' latter case, the \linkS4class{gGraph} to which the \linkS4class{gData} is
#' linked has to be in the current environment.
#' @param \dots further arguments passed to specific methods.
#' @param loc locations, specified as a list with two components indicating
#' longitude and latitude of locations. Alternatively, this can be a data.frame
#' or a matrix with longitude and latitude in columns, in this order. Note that
#' \code{locator()} can be used to specify interactively the locations.
#' @param zoneSize a numeric value indicating the size of the zone (in
#' latitude/longitude units) where the closest node is searched for. Note that
#' this only matters for speed purpose: if no closest node is found inside a
#' given zone, the zone is expanded until nodes are found.
#' @param attr.name the optional name of a node attribute. See details.
#' @param attr.values an optional vector giving values for \code{attr.names}.
#' See details.
#' @return If \code{x} is a \linkS4class{gGraph} object: a vector of node
#' names.\cr
#' 
#' If \code{x} is a \linkS4class{gData} object: a \linkS4class{gData} object
#' with matching nodes stored in the \code{@nodes.id} slot. Note that previous
#' content of \code{@nodes.id} will be erased.\cr
#' @author Thibaut Jombart (\email{t.jombart@@imperial.ac.uk})
#' @seealso \code{\link{geo.add.edges}} and \code{\link{geo.remove.edges}} to
#' interactively add or remove edges in a \linkS4class{gGraph} object.
#' @keywords utilities methods
#' @examples
#' 
#' \dontrun{
#' ## interactive example ##
#' plot(worldgraph.10k, reset=TRUE)
#' 
#' ## zooming in
#' geo.zoomin(list(x=c(-6,38), y=c(35,73)))
#' title("Europe")
#' 
#' ## click some locations
#' myNodes <- closestNode(worldgraph.10k,locator(), attr.name="habitat", attr.value="land")
#' myNodes
#' 
#' ## here are the closestNodes
#' points(getCoords(worldgraph.10k)[myNodes,], col="red")
#' }
#' 
#' ## example with a gData object ##
#' myLoc <- list(x=c(3, -8, 11, 28), y=c(50, 57, 71, 67)) # some locations
#' obj <- new("gData", coords=myLoc) # new gData object
#' obj
#' 
#' obj@gGraph.name <- "worldgraph.10k" # this could be done when creating obj
#' obj <- closestNode(obj, attr.name="habitat", attr.value="land")
#' 
#' ## plot the result (original location -> assigned node)
#' plot(obj, method="both", reset=TRUE)
#' title("'x'=location, 'o'=assigned node")
#' 
#' 
NULL





#' Check connectivity of a gGraph object
#' 
#' The functions \code{areNeighbours}, \code{areConnected} and the method
#' \code{isConnected} test connectivity in different ways.\cr
#' 
#' - \code{areNeighbours}: tests connectivity between couples of nodes on an
#' object inheriting \code{graph} class (like a \linkS4class{graphNEL}
#' object).\cr
#' 
#' - \code{areConnected}: tests if a set of nodes form a connected set on a
#' \linkS4class{gGraph} object.\cr
#' 
#' - \code{isConnected}: tests if the nodes of a \linkS4class{gData} object
#' form a connected set. Note that this is a method for \linkS4class{gData},
#' the generic being defined in the \code{graph} package.\cr
#' 
#' - \code{isReachable}: tests if one location (actually, the closest node to
#' it) is reachable from the set of nodes of a \linkS4class{gData} object.\cr
#' 
#' - \code{connectivityPlot}: plots connected sets of a \linkS4class{gGraph} or
#' a \linkS4class{gData} object with different colors.\cr
#' 
#' In \code{connectivityPlot}, isolated nodes (i.e. belonging to no connected
#' set of size > 1) are plotted in light grey.
#' 
#' @aliases areNeighbours areConnected isConnected,gData-method isReachable
#' connectivityPlot connectivityPlot-methods connectivityPlot,gGraph-method
#' connectivityPlot,gData-method
#' @param V1 a vector of node names
#' @param V2 a vector of node names
#' @param graph a valid \linkS4class{graphNEL} object.
#' @param x a valid \linkS4class{gGraph} object.
#' @param nodes a vector of node names
#' @param object a valid \linkS4class{gData} object.
#' @param \dots other arguments passed to other methods.
#' @param loc location, specified as a list of two components giving
#' respectively the longitude and the latitude. Alternatively, it can be a
#' matrix-like object with one row and two columns.
#' @param seed an optional integer giving the seed to be used when randomizing
#' colors. One given seed will always give the same set of colors. NULL by
#' default, meaning colors are randomized each time a plot is drawn.
#' @param col.gGraph a character string or a number indicating the color of the
#' nodes to be used when plotting the \linkS4class{gGraph} object. Defaults to
#' '0', meaning that nodes are invisible.
#' @return - \code{areNeighbours}: a vector of logical, having one value for
#' each couple of nodes.\cr
#' 
#' - \code{areConnected}: a single logical value, being TRUE if nodes form a
#' connected set.\cr
#' 
#' - \code{isConnected}: a single logical value, being TRUE if nodes of the
#' object form a connected set.\cr
#' @author Thibaut Jombart (\email{t.jombart@@imperial.ac.uk})
#' @keywords utilities methods
#' @examples
#' 
#' connectivityPlot(rawgraph.10k)
#' connectivityPlot(worldgraph.10k)
#' 
NULL





#' Shortest path using Dijkstra algorithm
#' 
#' The methods \code{dijkstraFrom} and \code{dijkstraBetween} are wrappers of
#' procedures implemented in RBGL package, designed for \linkS4class{gGraph}
#' and \linkS4class{gData} object.\cr
#' 
#' \code{dijkstraFrom} finds minimum costs paths to nodes from a given 'source'
#' node.\cr
#' 
#' \code{dijkstraBetween} finds minimum costs paths between all possible pairs
#' of nodes given two sets of nodes.\cr
#' 
#' All these functions return objects with S3 class "gPath". These objects can
#' be plotted using \code{plot.gPath}.
#' 
#' \code{gPath2dist} extracts the pairwise distances from the \code{gPath}
#' returned by \code{dijkstraBetween} and returns a \code{dist} object. Note
#' that if the \code{gPath} does not contain pairwise information, a warning
#' will be issued, but the resulting output will likely be meaningless.\cr
#' 
#' In 'dijkstraBetween', paths are seeked all possible pairs of nodes between
#' 'from' and 'to'.
#' 
#' @name dijkstra-methods
#' @aliases dijkstraFrom dijkstraFrom-methods dijkstraFrom,gData-method
#' dijkstraFrom,gGraph-method dijkstraBetween dijkstraBetween-methods
#' dijkstraBetween,gData-method dijkstraBetween,gGraph-method gPath2dist gPath
#' plot.gPath
#' @docType methods
#' @param x a \linkS4class{gGraph} or a \linkS4class{gData} object. For
#' plotting method of \code{gPath} objects, a \code{gPath} object.
#' @param start a character string naming the 'source' node.
#' @param from a vector of character strings giving node names.
#' @param to a vector of character strings giving node names.
#' @param col a character string indicating a color or a palette of colors to
#' be used for plotting edges.
#' @param lwd a numeric value indicating the width of edges.
#' @param m a \code{gPath} object obtained by \code{dijkstraBetween}.
#' @param diag,upper unused parameters added for consistency with
#' \code{as.dist}.
#' @param res.type a character string indicating what type of result should be
#' returned: a \code{dist} object ('dist'), or a vector of distances
#' ('vector'). Note that 'dist' should only be required for pairwise data, as
#' output by dijkstraBetween (as opposed to dijkstraFrom).
#' @param \dots further arguments passed to the \code{segments} method.
#' @return A "gPath" object. These are basically the outputs of RBGL's
#' \code{sp.between} function (see \code{?sp.between}), with a class attribute
#' set to "gPath", and an additional slot 'xy' containing geographic
#' coordinates of the nodes involved in the paths.\cr
#' @author Thibaut Jombart (\email{t.jombart@@imperial.ac.uk})
#' @keywords methods spatial
#' @examples
#' 
#' \dontrun{
#' 
#' ## plotting
#' world <- worldgraph.40k
#' par(mar=rep(.1,4))
#' plot(world, reset=TRUE)
#' 
#' ## check connectivity
#' isConnected(hgdp) # must be ok
#' 
#' ## Lowest cost path from an hypothetical origin
#' ori.coord <- list(33,10) # one given location long/lat
#' points(data.frame(ori.coord), pch="x", col="black", cex=3) # an 'x' shows the putative origin
#' ori <- closestNode(world, ori.coord) # assign it the closest node
#' 
#' myPath <- dijkstraFrom(hgdp, ori) # compute shortest path
#' 
#' ## plotting
#' plot(world,pch="") # plot the world
#' points(hgdp, lwd=3) # plot populations
#' points(data.frame(ori.coord), pch="x", col="black", cex=3) # add origin
#' plot(myPath) # plot the path
#' }
#' 
NULL





#' Retrieves node attributes from a layer
#' 
#' The generic function \code{extractFromLayer} uses information from a GIS
#' shapefile to define node attributes. For each node, information is retrieved
#' from the layer and assigned to that node.\cr
#' 
#' Nodes can be specified in different ways, including by providing a
#' \linkS4class{gGraph} or a \linkS4class{gData} object. Outputs match the
#' input formats.
#' 
#' 
#' @aliases extractFromLayer extractFromLayer-methods
#' extractFromLayer,matrix-method extractFromLayer,data.frame-method
#' extractFromLayer,list-method extractFromLayer,gGraph-method
#' extractFromLayer,gData-method
#' @param x a matrix, a data.frame, a list, a valid \linkS4class{gGraph}, or a
#' valid \linkS4class{gData} object. For matrix and data.frame, input must have
#' two columns giving longitudes and latitudes of locations being considered.
#' For list, input must have two components being vectors giving longitudes and
#' latitudes of locations.
#' @param layer a shapefile of the class \code{SpatialPolygonsDataFrame} (see
#' \code{readShapePoly} in maptools package to import such data from a GIS
#' shapefile). Alternatively, a character string indicating one shapefile
#' released with geoGraph; currently, only 'world' is available (see
#' \code{?data(worldshape)}).
#' @param attr a character vector giving names of the variables to be extracted
#' from the layer. If 'all', all available variables are extracted. In case of
#' problem, available names are displayed with the error message. Available
#' data are also stored in \code{layer@data}.
#' @param \dots further arguments to be passed to other methds. Currently not
#' used.
#' @return The output depends on the nature of the input:\cr - \code{matrix,
#' data.frame, list}: a data.frame with one row per location, and as many
#' columns as requested variables ('attributes').\cr
#' 
#' - \code{gGraph}: a \linkS4class{gGraph} object with new node attributes
#' (\code{@nodes.attr} slot). If nodes attributes already existed, new
#' attributes are added as new columns.\cr
#' 
#' - \code{gData}: a \linkS4class{gData} object with new data associated to
#' locations (\code{@data} slot). New information is merge to older information
#' according to the type of data being stored. \cr
#' @author Thibaut Jombart (\email{t.jombart@@imperial.ac.uk})
#' @seealso \code{\link{findLand}}, to find which locations are on land.
#' @keywords utilities methods
#' @examples
#' 
#' \dontrun{
#' 
#' plot(worldgraph.10k, reset=TRUE)
#' 
#' 
#' ## see what info is available
#' names(worldshape@data)
#' unique(worldshape@data$CONTINENT)
#' 
#' 
#' ## retrieve continent info for all nodes
#' ## (might take a few seconds)
#' x <- extractFromLayer(worldgraph.10k, layer=worldshape, attr="CONTINENT") 
#' x
#' table(getNodesAttr(x, attr.name="CONTINENT"))
#' 
#' 
#' ## subset Africa
#' temp <- getNodesAttr(x, attr.name="CONTINENT")=="Africa"
#' temp[is.na(temp)] <- FALSE
#' x <- x[temp]
#' plot(x, reset=TRUE)
#' 
#' }
#' 
NULL





#' Find which nodes are on land
#' 
#' The generic function \code{findLand} uses information from a GIS shapefile
#' to define which nodes are on land, and which are not. Strickly speaking,
#' being 'on land' is in fact being inside a polygon of the shapefile.
#' 
#' Nodes can be specified either as a matrix of geographic coordinates, or as a
#' \linkS4class{gGraph} object.
#' 
#' 
#' @aliases findLand findLand-methods findLand,matrix-method
#' findLand,data.frame-method findLand,gGraph-method
#' @param x a matrix, a data.frame, or a valid \linkS4class{gGraph} object. For
#' matrix and data.frame, input must have two columns giving longitudes and
#' latitudes of locations being considered.
#' @param shape a shapefile of the class \code{SpatialPolygonsDataFrame} (see
#' \code{readShapePoly} in maptools package to import such data from a GIS
#' shapefile). Alternatively, a character string indicating one shapefile
#' released with geoGraph; currently, only 'world' is available (see
#' \code{?data(worldshape)}).
#' @param \dots further arguments to be passed to other methods. Currently not
#' used.
#' @param attr.name a character string giving the name of the node attribute in
#' which the output is to be stored.
#' @return The output depends on the nature of the input:\cr - \code{matrix,
#' data.frame}: a factor with two levels being 'land' and 'sea'.\cr
#' 
#' - \code{gGraph}: a \linkS4class{gGraph} object with a new node attribute,
#' possibly added to previously existing node attributes (\code{@nodes.attr}
#' slot).\cr
#' @author Thibaut Jombart (\email{t.jombart@@imperial.ac.uk})
#' @seealso \code{\link{extractFromLayer}}, to retrieve any information from a
#' GIS shapefile.
#' @keywords utilities methods
#' @examples
#' 
#' 
#' ## create a new gGraph with random coordinates
#' myCoords <- data.frame(long=runif(1000,-180,180), lat=runif(1000,-90,90))
#' obj <- new("gGraph", coords=myCoords)
#' obj # note: no node attribute 
#' plot(obj)
#' 
#' ## find which points are on land
#' obj <- findLand(obj)
#' obj # note: new node attribute
#' 
#' ## define rules for colors
#' temp <- data.frame(habitat=c("land","sea"), color=c("green","blue"))
#' temp
#' obj@meta$color <- temp
#' 
#' ## plot object with new colors
#' plot(obj)
#' 
#' 
NULL





#' Formal class "gData"
#' 
#' The class \code{gData} is a formal (S4) class storing georeferenced data,
#' consisting in a set of locations (longitude and latitude) where one or
#' several variables have been measured. These data are designed to be matched
#' against a \linkS4class{gGraph} object, each location being assigned to the
#' closest node of the \linkS4class{gGraph} object.\cr
#' 
#' Note that for several operations on a \code{gData} object, the
#' \linkS4class{gGraph} object to which it is linked will have to be present in
#' the same environment.
#' 
#' 
#' @name gData-class
#' @aliases gData gData-class [,gData-method [,gData,ANY,ANY-method
#' [,gData,ANY,ANY,ANY-method getCoords,gData-method getData-methods
#' getData,gData-method getData getNodes,gData-method initialize,gData-method
#' show,gData-method is.gData getGraph,gData-method
#' @docType class
#' @section Objects from the class gData: \code{gData} objects can be created
#' by calls to \code{new("gData", ...)}, where '...' can be the following
#' arguments:
#' 
#' \describe{ \item{list("coords")}{a matrix of spatial coordinates with two
#' columns, being respectively longitude (from -180 to 180) and latitude.
#' Positive numbers are intended as 'east' and 'north', respectively.}
#' \item{list("nodes.id")}{a vector of character strings giving the name of the
#' nodes (of the \linkS4class{gGraph} object) associated to the locations.}
#' \item{list("data")}{any kind of data associated to the locations in coords.
#' For matrix-like objects, rows should correspond to locations.}
#' \item{list("gGraph.name")}{a character string the name of the
#' \linkS4class{gGraph} object against which the object is matched.} } Note
#' that none of these is mandatory: \code{new("gData")} would work, and create
#' an empty \code{gGraph} object. Also note that a finer matching of locations
#' against the nodes of a \code{gGraph} object can be achieved after creating
#' the object, for instance using the \code{closestNode} method.
#' @author Thibaut Jombart (\email{t.jombart@@imperial.ac.uk})
#' @seealso Related class:\cr - \code{\linkS4class{gGraph}}\cr
#' @keywords classes spatial
#' @examples
#' 
#' hgdp
#' 
#' ## plot data
#' plot(worldgraph.40k, pch="")
#' points(hgdp)
#' 
#' ## subset and plot data
#' onlyNorth <- hgdp[hgdp@data$Latitude >0] # only northern populations
#' 
#' plot(worldgraph.40k, reset=TRUE)
#' abline(h=0) # equator
#' points(onlyNorth, pch.node=20, cex=2, col.node="purple")
#' 
#' 
NULL





#' The geoGraph package
#' 
#' This package implements classes and methods for large-scale georeferenced
#' data handled through spatial graphs.\cr
#' 
#' Main functionalities of \code{geoGraph} are summarized below.\cr
#' 
#' === DATA HANDLING ===\cr In \code{geoGraph}, data are stored as a particular
#' formal class named \linkS4class{gGraph}. This class contains spatial
#' coordinates of a set of nodes (@coords), attributes for these nodes
#' (@nodes.attr), meta-information about nodes attributes (@meta), and a graph
#' of connections between nodes of class graphNEL (@graph).\cr
#' 
#' Several functions are available for handling gGraph data:
#' 
#' - some accessors allow to access slots of an object, sometimes with
#' additional treatment of information: \code{\link{getGraph}},
#' \code{\link{getNodesAttr}}, \code{\link{getCoords}}, \code{\link{getNodes}},
#' \code{\link{getEdges}}, \code{\link{getCosts}}.\cr
#' 
#' - \code{\link{setEdges}}: add/remove edges specified edges.\cr
#' 
#' - \code{\link{setCosts}}: set costs of edges.\cr
#' 
#' - \code{\link{hasCosts}}: tests if the graph is weighted (i.e., has
#' non-uniform costs).\cr
#' 
#' - \code{\link{isInArea}}: finds which nodes are in the currently plotted
#' area.\cr
#' 
#' - \code{\link{areConnected}}: tests if nodes are directly connected.\cr
#' 
#' - \code{\link{connectivityPlot}}: plot connected components with different
#' colors.\cr
#' 
#' - \code{\link{dropDeadEdges}}: suppress edges whose weight is null.\cr
#' 
#' - \code{\link{closestNode}}: given a longitude and a latitude, finds the
#' closest node; specific values of node attribute can be provided, for
#' instance, to find the closest node on land.\cr
#' 
#' - \code{\link{show}}: printing of gGraph objects.\cr
#' 
#' - \code{\link{extractFromLayer}}: extract information from GIS layers.\cr
#' 
#' - \code{\link{findLand}}: checks which nodes are on land.\cr
#' 
#' - \code{\link{setCosts}}: define edges weights accoring to rules specified
#' in the @meta slot.\cr
#' 
#' - \code{\link{geo.add.edges}}, \code{\link{geo.remove.edges}}: graphical
#' functions for adding or removing edges.\cr
#' 
#' - \code{\link{geo.change.attr}}: graphical functions for changing attributes
#' of nodes.\cr
#' 
#' === GRAPHICS ===\cr \code{geoGraph} aims at providing advanced graphical
#' facilities, such as zooming in or out particular area, moving the plotted
#' area, or visualizing connectivity between nodes. \cr
#' 
#' - \code{\link{plot}}: plot method with various options, allowing to display
#' a shapefile (by default, the map of the world), using color according to
#' attributes, showing connectivity between nodes, etc.\cr
#' 
#' - \code{\link{points}}: similar to plot method, except that a new plot is
#' not created.\cr
#' 
#' - \code{\link{plotEdges}}: the specific function plotting edges. It detects
#' if the object is a weighted graph, and plots edges accordingly.\cr
#' 
#' - \code{\link{geo.zoomin}}, \code{\link{geo.zoomout}}: zoom in and out a
#' plot.\cr
#' 
#' - \code{\link{geo.back}}: replot the previous screens.\cr
#' 
#' - \code{\link{geo.slide}}: slide the plotted area toward the indicated
#' direction.\cr
#' 
#' - \code{\link{geo.bookmark}}, \code{\link{geo.goto}}: set and goto a
#' bookmarked area.\cr
#' 
#' === DATASETS ===\cr Datasets occupy a central place in \code{geoGraph},
#' since they provide the spatial models used in later operations.
#' 
#' Two main datasets are proposed, each being a \linkS4class{gGraph} resulting
#' from the spliting of the earth into cells of (allmost perfectly) equal
#' sizes. Two different resolutions are provided:\cr -
#' \code{\link{worldgraph.10k}}: coverage using about 10,000 nodes\cr -
#' \code{\link{worldgraph.40k}}: coverage using about 40,000 nodes\cr
#' 
#' Other datasets are:\cr - \code{\link{worldshape}}: shapefile containing
#' world countries.\cr - \code{\link{globalcoord.10k}}: spatial coordinates
#' used in \code{\link{worldgraph.10k}}.\cr - \code{\link{globalcoord.40k}}:
#' spatial coordinates used in \code{\link{worldgraph.40k}}.\cr
#' 
#' To cite geoGraph, please use the reference given by
#' \code{citation("geoGraph")}.
#' 
#' \tabular{ll}{ Package: \tab geoGraph\cr Type: \tab Package\cr Version: \tab
#' 1.0-0\cr Date: \tab 2010-07-01 \cr License: \tab GPL (>=2) }
#' 
#' @name geoGraph-package
#' @aliases geoGraph-package geoGraph
#' @docType package
#' @author Thibaut Jombart <t.jombart@@imperial.ac.uk> (maintainer)\cr François
#' Balloux \cr Andrea Manica \cr
#' @keywords manip spatial
#' @examples
#' 
#' ## the class gGraph
#' worldgraph.10k
#' 
#' ## plotting the object
#' plot(worldgraph.10k, reset=TRUE)
#' 
#' ## zooming in
#' geo.zoomin(list(x=c(-6,38), y=c(35,73)))
#' title("Europe")
#' 
#' ## to play interactively with graphics, use:
#' # geo.zoomin()
#' # geo.zoomout()
#' # geo.slide()
#' # geo.back()
#' 
#' ## defining a new object restrained to visible nodes
#' x <- worldgraph.10k[isInArea(worldgraph.10k)]
#' plot(x,reset=TRUE, edges=TRUE)
#' title("x does just contain these visible nodes.")
#' 
#' ## define weights for edges
#' x <- setCosts(x, attr.name="habitat", method="prod")
#' plot(x,edges=TRUE)
#' title("connectivity defined by habitat (land/land=1, other=0)")
#' 
#' ## drop 'dead edges' (i.e. with weight 0)
#' x <- dropDeadEdges(x)
#' plot(x,edges=TRUE)
#' title("after droping edges with null weight")
#' 
#' 
NULL





#' Get colors associated to edges of a gGraph object
#' 
#' The function \code{getColors} returns the colors associated to the nodes of
#' a \linkS4class{gGraph} object, based on a specified node attribute.
#' 
#' Colors are based on a node attribute, that is, on a column of the
#' \code{nodes.attr} data.frame. This attribute should have a finite number of
#' values, and would most likely be a factor. Correspondence between values of
#' this variable and colors must be provided in the \code{@meta\$color} slot,
#' or as \code{col.rules} argument. Color rules mus be provided as a two-column
#' matrix; the first column contains values of a node attribute, and is named
#' after this attribute; the second must be named "color", and contain valid
#' colors.
#' 
#' See example section to know how this slot should be designed.
#' 
#' @aliases getColors getColors-methods getColors,gGraph-method
#' @param x a valid \linkS4class{gGraph}.
#' @param nodes a vector of character strings or of integers identifying nodes
#' by their name or their index. Can be "all", in which case all nodes are
#' considered.
#' @param attr.name a character string indicating the name of node attribute to
#' be used to define colors.
#' @param col.rules a matrix giving the rules for plotting attribute values
#' with different colors. See details.
#' @param \dots other arguments passed to other methods.
#' @return A vector of characters being valid colors.\cr
#' @author Thibaut Jombart (\email{t.jombart@@imperial.ac.uk})
#' @keywords utilities methods
#' @examples
#' 
#' worldgraph.10k # there is a node attribute 'habitat'
#' worldgraph.10k@meta$color
#' 
#' head(getNodes(worldgraph.10k))
#' head(getColors(worldgraph.10k,res.type="vector", attr.name="habitat"))
#' 
#' 
NULL





#' Get costs associated to edges of a gGraph object
#' 
#' The function \code{getCosts} returns the costs associated to the edges of a
#' \linkS4class{gGraph} object using different possible outputs. These outputs
#' are designed to match possible outputs of \code{\link{getEdges}} function.
#' 
#' \code{getNodeCosts} returns the costs associated to nodes based on one node
#' attribute.
#' 
#' The notion of 'costs' in the context of \linkS4class{gGraph} objects is
#' identical to the concept of 'weights' in \linkS4class{graph} (and thus
#' \linkS4class{graphNEL}) objects. The larger it is for an edge, the less
#' connectivity there is between the couple of concerned nodes.
#' 
#' @aliases getCosts getCosts-methods getCosts,gGraph-method getNodeCosts
#' getNodeCosts-methods getNodeCosts,gGraph-method
#' @param x a valid \linkS4class{gGraph}.
#' @param res.type a character string indicating which kind of output should be
#' used. See value.
#' @param unique a logical indicating whether the costs should be returned for
#' unique edges (TRUE), or if duplicate edges should be considered as well
#' (TRUE, default).
#' @param attr.name the name of the node attribute used to define node costs.
#' @param \dots other arguments passed to other methods (currently unused).
#' @return The output depends on the value of the argument \code{res.type}:\cr
#' - \code{asIs}: output is a named list of weights, each slot containing
#' weights associated to the edges stemming from one given node. This format is
#' that of the \code{weights} accessor for \linkS4class{graphNEL} objects.\cr
#' 
#' - \code{vector}: a vector of weights; this output matches matrix outputs of
#' \code{\link{getEdges}}.\cr
#' @author Thibaut Jombart (\email{t.jombart@@imperial.ac.uk})
#' @seealso Most other accessors are documented in \linkS4class{gGraph}
#' manpage.\cr
#' @keywords utilities methods
#' @examples
#' 
#' head(getEdges(worldgraph.10k, res.type="matNames",unique=TRUE))
#' head(getCosts(worldgraph.10k,res.type="vector",unique=TRUE))
#' 
#' 
NULL





#' Get edges from a gGraph object
#' 
#' The function \code{getEdges} returns the edges of a \linkS4class{gGraph}
#' object using different possible outputs.
#' 
#' 
#' @aliases getEdges getEdges-methods getEdges,gGraph-method
#' @param x a valid \linkS4class{gGraph}.
#' @param res.type a character string indicating which kind of output should be
#' used. See value.
#' @param unique a logical indicating whether all returned edges should be
#' unique (TRUE) or if duplicated edges should be allowed (TRUE, default).
#' @param \dots other arguments passed to other methods (currently unused).
#' @return The output depends on the value of the argument \code{res.type}:\cr
#' - \code{asIs}: output is a named list of nodes, each slot containing nodes
#' forming an edge with one given node. This format is that of the \code{edges}
#' accessor for \linkS4class{graphNEL} objects.\cr
#' 
#' - \code{matNames}: a matrix with two columns giving couples of node names
#' forming edges.\cr
#' 
#' - \code{matId}: a matrix with two columns giving couples of node indices
#' forming edges.\cr
#' @author Thibaut Jombart (\email{t.jombart@@imperial.ac.uk})
#' @seealso Most other accessors are documented in \linkS4class{gGraph}
#' manpage.\cr
#' 
#' See \code{\link{setEdges}} to add/remove edges, or
#' \code{\link{geo.add.edges}} and \code{\link{geo.remove.edges}} for
#' interactive versions.
#' @keywords utilities methods
#' @examples
#' 
#' example(gGraph)
#' 
#' getEdges(x)
#' getEdges(x,res.type="matNames")
#' getEdges(x,res.type="matId")
#' 
NULL





#' Get nodes attributes from gGraph/gData object
#' 
#' The function \code{getNodesAttr} returns the values of a set of variables
#' associated to the nodes (i.e. node attributes) of a \linkS4class{gGraph} or
#' \linkS4class{gData} object.
#' 
#' 
#' @aliases getNodesAttr getNodesAttr-methods getNodesAttr,gGraph-method
#' getNodesAttr,gData-method
#' @param x a valid \linkS4class{gGraph} or \linkS4class{gData} object.
#' @param nodes an optional integer, logical, or character string indicating
#' the subset of nodes to be used. If NULL, all nodes are used.
#' @param attr.name an optional character string indicating which node
#' attributes should be returned. If provided, it must match at least one of
#' the columns of \code{x@nodes.attr}.
#' @param \dots other arguments passed to other methods (currently unused).
#' @return A data.frame with the requested nodes attributes. Nodes are
#' displayed in rows, variables in columns.
#' @author Thibaut Jombart (\email{t.jombart@@imperial.ac.uk})
#' @seealso Most other accessors are documented in \linkS4class{gGraph} and
#' \linkS4class{gData} manpages.\cr
#' @keywords utilities methods
#' @examples
#' 
#' ## gGraph method
#' head(getNodesAttr(worldgraph.40k))
#' 
#' 
#' ## gData method
#' getNodesAttr(hgdp)
#' 
#' 
NULL





#' Formal class "gGraph"
#' 
#' The class \code{gGraph} is a formal (S4) class storing geographic data.\cr
#' Such data are composed of a set of geographic coordinates of vertices (or
#' 'nodes'), and a graph describing connectivity between these vertices. Data
#' associated to the nodes can also be stored ('nodes attributes'), as well as
#' meta-information used when plotting the object, or when computing weights
#' associated to the edges based on nodes attributes.\cr % History associated
#' to a \code{gGraph} object is stored in the slot % \code{history}, as an
#' object of the class % \linkS4class{gGraphHistory}.\cr
#' 
#' In all slots, nodes are uniquely identified by their name (reference is
#' taken from the row names of \code{@coords} slot).
#' 
#' 
#' @name gGraph-class
#' @aliases gGraph gGraph-class [,gGraph-method [,gGraph,ANY,ANY-method
#' [,gGraph,ANY,ANY,ANY-method getCoords,gGraph-method getGraph,gGraph-method
#' getNodes,gGraph-method initialize,gGraph-method dropCosts,gGraph-method
#' dropCosts show,gGraph-method is.gGraph getGraph getCoords getNodes
#' @docType class
#' @section Objects from the class gGraph: \code{gGraph} objects can be created
#' by calls to \code{new("gGraph", ...)}, where '...' can be the following
#' arguments:
#' 
#' \describe{ \item{list("coords")}{a matrix of spatial coordinates with two
#' columns, being respectively longitude (from -180 to 180) and latitude.
#' Positive numbers are intended as 'east' and 'north', respectively.}
#' \item{list("nodes.attr")}{a data.frame whose rows are nodes, and whose
#' columns are different variables associated to the nodes.}
#' \item{list("meta")}{a list, most likely containing named data.frames (see
#' Slots).} \item{list("graph")}{an object of the class \linkS4class{graphNEL},
#' from the \code{graph} package (see \code{class?graphNEL}), describing
#' connectivity among nodes.} % \item{\code{history}}{an object of the class %
#' \linkS4class{gGraphHistory}, storing a previous history.} %
#' \item{\code{cmd}}{an unevaluated expression (see \code{?expression}) %
#' passed to the \linkS4class{gGraphHistory} object.} %
#' \item{\code{comments}}{a character string passed to the %
#' \linkS4class{gGraphHistory} object, commenting the object creation.} %
#' \item{\code{}}{} % \item{\code{}}{} % \item{\code{}}{} }
#' 
#' Note that none of these is mandatory: \code{new("gGraph")} would work, and
#' create an empty \code{gGraph} object.
#' @author Thibaut Jombart (\email{t.jombart@@imperial.ac.uk})
#' @seealso Related classes are:\cr % - \code{\linkS4class{gGraphHistory}}:
#' slot \code{@history} in \code{gGraph}.\cr - \code{\linkS4class{graphNEL}}
#' (graph package): slot \code{@graph} in \code{gGraph}.\cr
#' @keywords classes spatial graphs
#' @examples
#' 
#' ## create an empty object
#' new("gGraph")
#' 
#' 
#' ## plotting the object
#' plot(rawgraph.10k, reset=TRUE)
#' 
#' ## zooming in
#' geo.zoomin(list(x=c(-6,38), y=c(35,73)))
#' title("Europe")
#' 
#' ## to play interactively with graphics, use:
#' # geo.zoomin()
#' # geo.zoomout()
#' # geo.slide()
#' # geo.back()
#' 
#' ## defining a new object restrained to visible nodes
#' x <- rawgraph.10k[isInArea(rawgraph.10k)]
#' plot(x,reset=TRUE, edges=TRUE)
#' title("x does just contain these visible nodes.")
#' 
#' ## define weights for edges
#' x <- setCosts(x, attr.name="habitat", method="prod")
#' plot(x,edges=TRUE)
#' title("costs defined by habitat (land/land=1, other=100)")
#' 
#' ## drop 'dead edges' (i.e. with weight 0)
#' x <- dropDeadEdges(x, thres=10)
#' plot(x,edges=TRUE)
#' title("after droping edges with null weight")
#' 
#' 
NULL





#' Disabled functionality
#' 
#' Disabled functionality
#' 
#' 
NULL





#' Human genome diversity panel - georeferenced data
#' 
#' The datasets \code{hgdp} and \code{hgdpPlus} provides genetic diversity
#' several human populations worldwide. Both datasets are \linkS4class{gData}
#' objects, interfaced with the \linkS4class{gGraph} object
#' \code{\link{worldgraph.40k}}.\cr
#' 
#' \code{hgdp} describes 52 populations from the original Human Genome
#' Diversity Panel. \cr
#' 
#' \code{hgdpPlus} describes \code{hgdp} populations plus 24 native American
#' populations. \cr
#' 
#' 
#' @name hgdp
#' @aliases hgdp hgdpPlus
#' @docType data
#' @format \code{hgdp} is a \linkS4class{gGraph} object with the following
#' data: % \describe{ % \item{@nodes.attr$habitat}{habitat corresponding to
#' each % vertice; currently 'land' or 'sea'.} % \item{@meta$color}{a matrix
#' assigning a color for plotting % vertices (second column) to different
#' values of habitat (first % column).} % }
#' @references Authors \emph{Journal}, YEAR, \bold{nb}: pp-pp.
#' @keywords datasets
#' @examples
#' 
#' ## check object
#' hgdp
#' 
#' ## plotting the object
#' plot(hgdp)
#' 
#' 
#' ## results from Handley et al.
#' \dontrun{
#' ## Addis Ababa
#' addis <- list(lon=38.74,lat=9.03)
#' addis <- closestNode(worldgraph.40k,addis) # this takes a while
#' 
#' ## shortest path from Addis Ababa
#' myPath <- dijkstraFrom(hgdp, addis)
#' 
#' ## plot results
#' plot(worldgraph.40k, col=0)
#' points(hgdp)
#' points(worldgraph.40k[addis], psize=3,pch="x", col="black")
#' plot(myPath)
#' 
#' ## correlations distance/genetic div.
#' geo.dist <- sapply(myPath[-length(myPath)],function(e) e$length)
#' gen.div <- getData(hgdp)[,"Genetic.Div"]
#' plot(gen.div~geo.dist)
#' lm1 <- lm(gen.div~geo.dist)
#' abline(lm1, col="blue") # this regression is wrong
#' summary(lm1)
#' }
#' 
NULL





#' Find which nodes fall in a given area
#' 
#' The generic function \code{isInArea} finds which nodes fall in a given area.
#' Nodes can be specified in different ways, including by providing a
#' \linkS4class{gGraph} or a \linkS4class{gData} object. Different format for
#' the output are also available.
#' 
#' 
#' @aliases isInArea isInArea-methods isInArea,matrix-method
#' isInArea,data.frame-method isInArea,gGraph-method isInArea,gData-method
#' @param x a matrix, a data.frame, a valid \linkS4class{gGraph}, or a valid
#' \linkS4class{gData} object. For matrix and data.frame, input must have two
#' columns giving longitudes and latitudes of locations being considered.
#' @param \dots further arguments passed to specific methods.
#' @param reg a character string or a list indicating the area ('reg' stands
#' for 'region'). Character strings can be "current" (current user window,
#' default) or "zoom" (current zoom). If the argument is a list, is has to have
#' two components, both being numeric vectors of length two, giving x and y
#' limits of the area. Note that such list can be produced by \code{locator},
#' so \code{locator(1)} is a valid value for \code{reg}.
#' @param res.type a character string indicating what kind of output should be
#' produced. See value.
#' @param buffer a numeric value giving a buffer adding extra space aroung the
#' area, as a proportion of current area's dimensions.
#' @return The output depends on the value of the argument \code{res.type}:\cr
#' - \code{logical}: a vector of logicals having one value for each node of the
#' input.\cr
#' 
#' - \code{integer}: a vector of integers corresponding to the indices of nodes
#' falling within the area.\cr
#' 
#' - \code{character}: a vector of characters corresponding to the names of the
#' nodes falling within the area.\cr
#' @author Thibaut Jombart (\email{t.jombart@@imperial.ac.uk})
#' @keywords utilities methods
#' @examples
#' 
#' plot(worldgraph.10k, reset=TRUE)
#' 
#' ## zooming in
#' geo.zoomin(list(x=c(-6,38), y=c(35,73)))
#' title("Europe")
#' 
#' 
#' ## different outputs of isInArea
#' head(isInArea(worldgraph.10k)) # logical
#' length(isInArea(worldgraph.10k))
#' sum(isInArea(worldgraph.10k))
#' head(which(isInArea(worldgraph.10k))) # which nodes are TRUE ?
#' 
#' head(isInArea(worldgraph.10k, res.type="integer")) # node indices
#' 
#' head(isInArea(worldgraph.10k, res.type="character")) # node names
#' 
#' 
#' ## use isInArea to have a subset of visible nodes
#' x <- worldgraph.10k[isInArea(worldgraph.10k)]
#' plot(x, reset=TRUE)
#' 
NULL





#' Plot a gData object.
#' 
#' Various functions to plot a \linkS4class{gData} object: \code{plot} opens a
#' device and plots the object, while \code{points} plots the object on the
#' existing device. Plotting of \linkS4class{gData} object relies on plotting
#' the \linkS4class{gGraph} object to which it is linked, and then represent
#' the locations of the \linkS4class{gData} and/or the associated nodes.
#' 
#' When \code{sticky.points} is set to TRUE, all operations performed on the
#' graphics like zooming or sliding the window can be performed without loosing
#' the \code{gData} plot.\cr
#' 
#' @name plot-gData
#' @aliases plot,gData-method plot,gData,missing-method plot.gData
#' points,gData-method points.gData
#' @docType methods
#' @param x a valid \linkS4class{gData} object. The \linkS4class{gData} object
#' to which it is linked must exist in the global environment.
#' @param type a character string indicating which information should be
#' plotted: original locations ('original'), associated nodes ('nodes',
#' default), or both ('both'). In the latter case, an arrow goes from locations
#' to nodes.
#' @param pch.ori a numeric or a character indicating the type of point for
#' locations.
#' @param pch.nodes a numeric or a character indicating the type of point for
#' nodes.
#' @param col.ori a character string indicating the color to be used for
#' locations.
#' @param col.nodes a character string indicating the color to be used for
#' nodes.
#' @param col.gGraph a (recycled) color vector for the associated
#' \linkS4class{gGraph} object. If NULL, default color is used. Set to
#' \code{NA} or "transparent" to avoid plotting the \linkS4class{gGraph}.
#' @param reset a logical stating whether the plotting area should be reset to
#' fit the \code{gData} object (TRUE), or should conserve previous plotting and
#' settings (FALSE, default).
#' @param sticky.points a logical indicating if added points should be kept
#' when replotting (TRUE, default), or not (FALSE). In any case,
#' \code{reset=TRUE} will prevent points to be redrawn.
#' @param \dots further arguments passed to \code{points}.
#' @author Thibaut Jombart (\email{t.jombart@@imperial.ac.uk})
#' @seealso - Different functions to explore these plots:\cr
#' \code{\link{geo.zoomin}}, \code{\link{geo.zoomout}},
#' \code{\link{geo.slide}}, \code{\link{geo.back}}, \code{\link{geo.bookmark}},
#' \code{\link{geo.goto}}.\cr
#' @keywords methods hplot spatial
#' @examples
#' 
#' 
#' myLoc <- list(x=c(3, -8, 11, 28), y=c(50, 57, 71, 67)) # some locations
#' obj <- new("gData", coords=myLoc) # new gData object
#' obj
#' 
#' obj@gGraph.name <- "worldgraph.10k"
#' obj <- closestNode(obj, attr.name="habitat", attr.value="land")
#' 
#' ## plot the result (original location -> assigned node)
#' plot(obj, type="both",reset=TRUE)
#' title("'x'=location, 'o'=assigned node")
#' 
#' ## using different parameters
#' points(obj, type="both", pch.ori=2, col.ori="red", pch.nodes=20, col.nodes="pink")
#' 
#' ## only nodes, fancy plot
#' plot(obj, col.nodes="red", cex=1, pch.node=20)
#' points(obj, col.nodes="red", cex=2)
#' points(obj, col.nodes="orange", cex=3)
#' points(obj, col.nodes="yellow", cex=4)
#' 
#' 
NULL





#' Plot a gGraph object.
#' 
#' Various functions to plot a \linkS4class{gGraph} object: \code{plot} opens a
#' device and plot the object, while \code{points} plots the object on the
#' existing device. \code{plotEdges} only plots the edges of the graph: it can
#' be called directly, or via arguments passed to \code{plot} and
#' \code{points}.\cr
#' 
#' Plotting of a gGraph object stores some parameters in R; see details for
#' more information.
#' 
#' To be able to zoom in and out, or slide the window, previous plotting
#' information are stored in a particular environment (.geoGraphEnv), which is
#' created when loading \code{geoGraph}. Users should not have to interact
#' directly with objects in this environment.\cr
#' 
#' The resulting plotting behaviour is that when plotting a \code{gGraph}
#' object, last plotting parameters are re-used. To override this behaviour,
#' specify \code{reset=TRUE} as argument to \code{plot}.
#' 
#' @name plot-gGraph
#' @aliases plot,gGraph-method plot,gGraph,missing-method plot.gGraph
#' points,gGraph-method points.gGraph plotEdges
#' @docType methods
#' @param x a \linkS4class{gGraph} object.
#' @param shape a shapefile used as background to the object. Must be of the
#' class \code{SpatialPolygonsDataFrame} (see \code{readShapePoly} in maptools
#' package to import such data from a GIS shapefile). Alternatively, a
#' character string indicating one shapefile released with geoGraph.
#' @param psize a numeric giving the size of points.
#' @param pch a numeric or a character indicating the type of point.
#' @param col a character string indicating the color to be used.
#' @param edges a logical indicating if edges should be plotted (TRUE) or not
#' (FALSE).
#' @param reset a logical indicating if plotting parameters should be reset
#' (TRUE) or not (FALSE).
#' @param bg.col a character string indicating the color of the polygons of the
#' shapefile used as background.
#' @param border.col a character string indicating the color of the polygon
#' borders.
#' @param lwd a numeric indicating the width of line (used for edges).
#' @param useCosts a logical indicating if edge width should be inversely
#' proportionnal to edge cost (TRUE) or not (FALSE).
#' @param maxLwd a numeric indicating the maximum edge width (corresponding to
#' the maximum weight).
#' @param col.rules a data.frame with two named columns, the first one giving
#' values of a node attribute, and the second one stating colors to be used for
#' each value. If not provided, this is seeked from the \code{@meta\$color}
#' slot of the object.
#' @param sticky.points a logical indicating if added points should be kept
#' when replotting (TRUE), or not (FALSE). In any case, \code{reset=TRUE} will
#' prevent points to be redrawn.
#' @param lty the type of line (for the edges).
#' @param pcol a character indicating the color to be used for points.
#' @param sticky.edges a logical indicating whether added edges should be kept
#' when replotting (TRUE), or not (FALSE, default). In any case,
#' \code{reset=TRUE} will prevent points to be redrawn.
#' @param \dots further arguments passed to the generic methods (plot, points,
#' and segments, respectively).
#' @author Thibaut Jombart (\email{t.jombart@@imperial.ac.uk})
#' @seealso - Different functions to explore these plots:\cr
#' \code{\link{geo.zoomin}}, \code{\link{geo.zoomout}},
#' \code{\link{geo.slide}}, \code{\link{geo.back}}.\cr
#' 
#' - \code{\link{isInArea}}, to retain a set of visible data.\cr
#' @keywords methods hplot spatial
#' @examples
#' 
#' 
#' ## just the background
#' plot(worldgraph.10k,reset=TRUE,type="n")
#' 
#' ## basic plot
#' plot(worldgraph.10k)
#' 
#' ## zooming and adding edges
#' geo.zoomin(list(x=c(90,150),y=c(0,-50)))
#' plot(worldgraph.10k, edges=TRUE)
#' 
#' 
#' ## display edges differently
#' plotEdges(worldgraph.10k, col="red", lwd=2)
#' 
#' 
#' ## replot points with different color
#' points(worldgraph.10k, col="orange")
#' 
#' ## mask points in the sea
#' inSea <- unlist(getNodesAttr(worldgraph.10k,attr.name="habitat"))=="sea"
#' head(inSea)
#' points(worldgraph.10k[inSea], col="white", sticky=TRUE) # this will stay
#' 
#' ## but better, only draw those on land, and use a fancy setup
#' par(bg="blue")
#' plot(worldgraph.10k[!inSea], bg.col="darkgreen", col="purple", edges=TRUE)
#' 
#' 
NULL





#' Set costs associated to edges based on geographic distances
#' 
#' The function \code{setDistCosts} sets the costs of a \linkS4class{gGraph}
#' object using the geographic distance. The cost associated to an edge is
#' defined as the great circle distance between the two nodes of this edge.
#' \code{setDistCosts} actually relies on \code{\link[fields]{rdist.earth}} of
#' the \code{fields} package.
#' 
#' The notion of 'costs' in the context of \linkS4class{gGraph} objects is
#' identical to the concept of 'weights' in \linkS4class{graph} (and thus
#' \linkS4class{graphNEL}) objects. The larger it is for an edge, the less
#' connectivity there is between the couple of concerned nodes.
#' 
#' @aliases setDistCosts setDistCosts-methods setDistCosts,gGraph-method
#' @param x a valid \linkS4class{gGraph}.
#' @param \dots other arguments passed to other methods (currently unused).
#' @return For the \linkS4class{gGraph} method, a \linkS4class{gGraph} object
#' with appropriate weights. Note that former weights will be removed from the
#' object.
#' @author Thibaut Jombart (\email{t.jombart@@imperial.ac.uk})
#' @seealso The \code{\link{getCosts}} accessor, returning costs of the edges
#' of a \linkS4class{gGraph} object in different ways.\cr
#' @keywords utilities methods
#' @examples
#' 
#' if(require(fields)){
#' ## load data
#' plot(rawgraph.10k,reset=TRUE)
#' geo.zoomin(list(x=c(110,150),y=c(-10,-40)))
#' plotEdges(rawgraph.10k)
#' 
#' ## compute costs
#' x <- rawgraph.10k[isInArea(rawgraph.10k)]
#' x <- setDistCosts(x)
#' 
#' ## replot edges
#' plotEdges(x) # no big differences can be seen
#' head(getCosts(x))
#' }
#' 
NULL





#' Add and remove edges from a gGraph object
#' 
#' The function \code{setEdges} allows one to add or remove edges in a
#' \linkS4class{gGraph} by directly specifying the relevant nodes, as a list or
#' a data.frame. This low-level function is called by \code{geo.add.edges} and
#' \code{geo.remove.edges}.
#' 
#' 
#' @aliases setEdges setEdges-methods setEdges,gGraph-method
#' @param x a valid \linkS4class{gGraph} object.
#' @param add a list or a dataframe containing node names of edges to be added.
#' The first element of the list (or column of the data.frame) gives starting
#' nodes of edges; the second gives ending nodes. Hence, the nodes of the i-th
#' edge are \code{add[[1]][i]} and \code{add[[2]][i]} if \code{add} is a list,
#' and \code{add[i,]} if \code{add} is a data.frame.
#' @param remove same as \code{add} argument, but edges are removed.
#' @param costs a numeric vector providing costs of the edges to be added.
#' \code{costs[i]} is the weight of the i-th edge.
#' @param \dots other arguments passed to other methods (currently unused).
#' @return A \linkS4class{gGraph} object with newly added or removed edges.
#' @author Thibaut Jombart (\email{t.jombart@@imperial.ac.uk})
#' @seealso \code{\link{geo.add.edges}} and \code{\link{geo.remove.edges}} to
#' interactively add or remove edges in a \linkS4class{gGraph} object. \cr
#' 
#' \code{\link{getEdges}} to retrieve edges in different formats.
#' @keywords utilities methods
NULL





#' Worldwide geographic graphs
#' 
#' The datasets 'rawgraph.10k', 'rawgraph.40k', 'worldgraph.10k', and
#' 'worldgraph.40k' are geographic graphs (\linkS4class{gGraph} objects) of the
#' world, with respective resolutions of 10,242 and 40,962 vertices.\cr
#' 
#' 'rawgraph's are raw graphs as obtained directly from the method provided in
#' references.\cr
#' 
#' 'worldgraph's are 'rawgraph's that have been modified manually to recitify
#' connectivity between edges at some places. The most noticable change is that
#' all edges oversea have been removed.\cr
#' 
#' 'globalcoord.10k' and 'globalcoord.40k' are matrices of geographic
#' coordinates of nodes, used to construct 'worlgraph' objects.\cr
#' 
#' 'worldshape' is a shapefile of contries of the world (snapshot from 1994).
#' 
#' 
#' @name worldgraph
#' @aliases worldgraph rawgraph.10k rawgraph.40k worldgraph.10k worldgraph.40k
#' globalcoord.10k globalcoord.40k worldshape
#' @docType data
#' @format \code{worldgraph.10k} and \code{worldgraph.40k} are
#' \linkS4class{gGraph} objects with the following specificities: \describe{
#' \item{@nodes.attr\$habitat}{habitat corresponding to each vertice; currently
#' 'land' or 'sea'.} \item{@meta\$color}{a matrix assigning a color for
#' plotting vertices (second column) to different values of habitat (first
#' column).} }
#' @references === On the construction of the graph ===\cr Randall, D. A.;
#' Ringler, T. D.; Heikes, R. P.; Jones, P. & Baumgardner, J. Climate Modeling
#' with Spherical Geodesic Grids \emph{Computing in science & engineering},
#' 2002, \bold{4}: 32-41.
#' @source Graph reconstructed by Andrea Manica.
#' @keywords datasets
#' @examples
#' 
#' 
#' worldgraph.10k
#' 
#' ## plotting the object
#' plot(worldgraph.10k, reset=TRUE)
#' title("Hello world")
#' 
#' ## zooming in
#' geo.zoomin(list(x=c(-12,45), y=c(33,75)))
#' title("Europe")
#' geo.zoomin(list(x=c(-12,2), y=c(50,60)))
#' plotEdges(worldgraph.10k)
#' title("United Kingdom")
#' 
#' ## zooming out
#' # geo.zoomout() # needs clicking on device
#' geo.zoomin(list(x=c(-6,38), y=c(35,73)))
#' title("Europe")
#' 
#' ## defining the subset of visible points
#' x <- worldgraph.10k[isInArea(worldgraph.10k)] 
#' plot(x,reset=TRUE, edges=TRUE)
#' title("One subsetted object.")
#' 
#' \dontrun{
#' ## interactive zooming
#' geo.zoomin()
#' }
#' 
#' 
NULL



