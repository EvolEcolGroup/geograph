\name{gData-class}
\docType{class}
\alias{gData}
\alias{gData-class}
\alias{[,gData-method}
\alias{[,gData,ANY,ANY-method}
\alias{[,gData,ANY,ANY,ANY-method}
\alias{getCoords,gData-method}
\alias{getData-methods}
\alias{getData,gData-method}
\alias{getData}
\alias{getNodes,gData-method}
\alias{initialize,gData-method}
\alias{show,gData-method}
\alias{is.gData}
\alias{getGraph,gData-method}
%%%% THESE HAVE THEIR OWN MANPAGE
%\alias{isInArea,gData-method}
%\alias{points,gData-method}
%\alias{isConnected,gData-method}
%\alias{closestNode,gData-method}
%\alias{dijkstraBetween,gData-method}
%\alias{dijkstraFrom,gData-method}
%\alias{extractFromLayer,gData-method}
%%%%
\title{Formal class "gData"}
\description{The class \code{gData} is a formal (S4) class storing
  georeferenced data, consisting in a set of locations (longitude and
  latitude) where one or several variables have been measured. These
  data are designed to be matched against a \linkS4class{gGraph}
  object, each location being assigned to the closest node of the
  \linkS4class{gGraph} object.\cr

  Note that for several operations on a \code{gData} object, the
  \linkS4class{gGraph} object to which it is linked will
  have to be present in the same environment.
}
\section{Objects from the class gData}{
  \code{gData} objects can be created by calls to \code{new("gData",
    ...)}, where '...' can be the following arguments:
  
  \describe{
    \item{\code{coords}}{a matrix of spatial coordinates with two columns,
      being respectively longitude (from -180 to 180) and latitude. Positive
      numbers are intended as 'east' and 'north', respectively.}
    \item{\code{nodes.id}}{a vector of character strings giving the name
      of the nodes (of the \linkS4class{gGraph} object) associated to the locations.}
    \item{\code{data}}{any kind of data associated to the locations in
      coords. For matrix-like objects, rows should correspond to locations.}
    \item{\code{gGraph.name}}{a character string the name of the \linkS4class{gGraph}
      object against which the object is matched.}
  }
  Note that none of these is mandatory: \code{new("gData")} would work, and
  create an empty \code{gGraph} object. Also note that a finer matching
    of locations against the nodes of a \code{gGraph} object can be
    achieved after creating the object, for instance using the
    \code{closestNode} method.
}
\section{Slots}{
	 \describe{
    \item{\code{coords}:}{Object of class \code{"matrix"} storing
      geographic coordinates (see above).}
    \item{\code{nodes.id}:}{Object of class \code{"character"} storing
    names of the nodes of a \code{gGraph} object assigned to the
    locations in \code{@coords}.}
    \item{\code{data}:}{Any object where observations match locations in
    \code{@coords}.}
    \item{\code{gGraph.name}:}{the name of a \code{gGraph} object
    (see above).}
}
}
\section{Methods}{
  Here is a list of methods available for \code{gData} objects. See
  corresponding manpages for further documentation. Specific manpages
  exist for accessors with more than one argument. These are indicated
  by a '*' symbol next to the method's name.
  \describe{
    \item{[}{\code{signature(x = "gData")}: usual method to subset
      objects in R. First indice corresponds to locations (it can be a logical,
      a numeric, or a character); when a character is used, it has to be
      node names matching those in \code{@nodes.id}. Optionnaly, a second indice can be
      provided to subset columns of \code{data} (i.e., variables
      associated to locations) as well.}
    \item{closestNode}{\code{signature(x = "gData")}: find the node
      closest to a given location.}
    \item{dijkstraBetween}{\code{signature(x = "gData")}: least-cost
      path between two sets of nodes.}
    \item{dijkstraFrom}{\code{signature(x = "gData")}: least-cost
      path between a set of nodes and a 'source node'.}
    \item{extractFromLayer}{\code{signature(x = "gData")}: retrieve
      information from a shapefile and add it to \code{nodes.attr}.}
    \item{getCoords}{\code{signature(x = "gData")}: get geographic
      coordinates of the object. The additional argument \code{original}
    is a logical indicating if original coordinates should be returned
    (TRUE, default), or if coordinates of the assigned nodes of the
    \code{gGraph} should be returned.}
    \item{getData}{\code{signature(x = "gData")}: get the data of the
      object (i.e., content of the slot \code{@data}).}
    \item{getNodes}{\code{signature(x = "gData")}: get the names of the
      nodes of the object.}
    \item{initialize}{\code{signature(.Object = "gData")}: internal
      definition of the constructor.}
    \item{isInArea}{\code{signature(x = "gData")}: check which nodes
      are in a given rectangular area.}
    \item{points}{\code{signature(x = "gData")}: add points to an
      existing plot.}
    \item{show}{\code{signature(object = "gData")}: prints the object
      to screen.}
    \item{isConnected}{\code{signature(object = "gData")}: check if
      nodes are a connected set.}
    \item{getGraph}{\code{signature(object = "gData")}: get the
      \linkS4class{graphNEL} object linked to a \code{gData}.}
    \item{getNodesAttr*}{\code{signature(x = "gData")}: get the nodes
      attributes of the object.}
  }
}
\author{Thibaut Jombart (\email{t.jombart@imperial.ac.uk})}
\seealso{
 Related class:\cr
  -  \code{\linkS4class{gGraph}}\cr
}
\examples{
## load data
data(worldgraph.40k)
data(hgdp)
hgdp

## plot data
plot(worldgraph.40k, pch="")
points(hgdp)

## subset and plot data
onlyNorth <- hgdp[hgdp@data$Latitude >0] # only northern populations

plot(worldgraph.40k, reset=TRUE)
abline(h=0) # equator
points(onlyNorth, pch.node=20, cex=2, col.node="purple")

}
\keyword{classes}
\keyword{spatial}
