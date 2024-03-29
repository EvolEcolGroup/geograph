% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/closestNode.R
\name{closestNode}
\alias{closestNode}
\alias{closestNode-methods}
\alias{closestNode,gGraph-method}
\alias{closestNode,gData-method}
\title{Find the closest node to a given location}
\usage{
closestNode(x, ...)

\S4method{closestNode}{gGraph}(x, loc, zoneSize = 5, attr.name = NULL, attr.values = NULL)

\S4method{closestNode}{gData}(x, zoneSize = 5, attr.name = NULL, attr.values = NULL)
}
\arguments{
\item{x}{a valid \linkS4class{gGraph} or \linkS4class{gData} object. In the
latter case, the \linkS4class{gGraph} to which the \linkS4class{gData} is
linked has to be in the current environment.}

\item{\dots}{further arguments passed to specific methods.}

\item{loc}{locations, specified as a list with two components indicating
longitude and latitude of locations. Alternatively, this can be a data.frame
or a matrix with longitude and latitude in columns, in this order. Note that
\code{locator()} can be used to specify interactively the locations.}

\item{zoneSize}{a numeric value indicating the size of the zone (in
latitude/longitude units) where the closest node is searched for. Note that
this only matters for speed purpose: if no closest node is found inside a
given zone, the zone is expanded until nodes are found.}

\item{attr.name}{the optional name of a node attribute. See details.}

\item{attr.values}{an optional vector giving values for \code{attr.names}.
See details.}
}
\value{
If \code{x} is a \linkS4class{gGraph} object: a vector of node
names.\cr

If \code{x} is a \linkS4class{gData} object: a \linkS4class{gData} object
with matching nodes stored in the \code{@nodes.id} slot. Note that previous
content of \code{@nodes.id} will be erased.\cr
}
\description{
The function \code{closestNode} searches for the closest node in a
\linkS4class{gGraph} or a \linkS4class{gData} object to a given location. It
is possible to restrain the research to given values of a node attribute.
For instance, one can search the closest node on land to a given
location.\cr
}
\details{
This function is also used to match locations of a \linkS4class{gData}
object with nodes of the \code{gGraph} object to which it is linked.

When creating a \linkS4class{gData} object, if the \code{gGraph.name}
argument is provided, then locations are matched with the \code{gGraph}
object automatically, by an internal call to closestNode. Note, however,
that it is not possible to specify node attributes (\code{attr.names} and
\code{attr.values}) this way.
}
\section{Functions}{
\itemize{
\item \code{closestNode(gGraph)}: Method for gGraph

\item \code{closestNode(gData)}: Method for gData

}}
\examples{
\dontrun{
## interactive example ##
plot(worldgraph.10k, reset = TRUE)

## zooming in
geo.zoomin(list(x = c(-6, 38), y = c(35, 73)))
title("Europe")

## click some locations
myNodes <- closestNode(worldgraph.10k, locator(), attr.name = "habitat", attr.value = "land")
myNodes

## here are the closestNodes
points(getCoords(worldgraph.10k)[myNodes, ], col = "red")
}

## example with a gData object ##
myLoc <- list(x = c(3, -8, 11, 28), y = c(50, 57, 71, 67)) # some locations
obj <- new("gData", coords = myLoc) # new gData object
obj

obj@gGraph.name <- "worldgraph.10k" # this could be done when creating obj
obj <- closestNode(obj, attr.name = "habitat", attr.value = "land")

## plot the result (original location -> assigned node)
plot(obj, method = "both", reset = TRUE)
title("'x'=location, 'o'=assigned node")


}
\seealso{
\code{\link{geo.add.edges}} and \code{\link{geo.remove.edges}} to
interactively add or remove edges in a \linkS4class{gGraph} object.
}
\keyword{methods}
\keyword{utilities}
