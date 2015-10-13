\name{dijkstra-methods}
\docType{methods}
\alias{dijkstraFrom}
\alias{dijkstraFrom-methods}
\alias{dijkstraFrom,gData-method}
\alias{dijkstraFrom,gGraph-method}
\alias{dijkstraBetween}
\alias{dijkstraBetween-methods}
\alias{dijkstraBetween,gData-method}
\alias{dijkstraBetween,gGraph-method}
\alias{gPath2dist}
\alias{gPath}
\alias{plot.gPath}
\title{ Shortest path using Dijkstra algorithm }
\description{ The methods \code{dijkstraFrom} and \code{dijkstraBetween}
  are wrappers of procedures implemented in RBGL package, designed for
  \linkS4class{gGraph} and \linkS4class{gData} object.\cr

  \code{dijkstraFrom} finds minimum costs paths to nodes from a given
  'source' node.\cr

  \code{dijkstraBetween} finds minimum costs paths between all possible
  pairs of nodes given two sets of nodes.\cr

  All these functions return objects with S3 class "gPath". These
  objects can be plotted using \code{plot.gPath}.
  
  \code{gPath2dist} extracts the pairwise distances from the \code{gPath}
  returned by \code{dijkstraBetween} and returns a \code{dist}
  object. Note that if the \code{gPath} does not contain pairwise
  information, a warning will be issued, but the resulting output will
  likely be meaningless.\cr
}
\usage{
  \S4method{dijkstraFrom}{gGraph}(x, start)
  \S4method{dijkstraFrom}{gData}(x, start)
  \S4method{dijkstraBetween}{gGraph}(x, from, to)
  \S4method{dijkstraBetween}{gData}(x)
  \method{plot}{gPath}(x, col="rainbow", lwd=3, \dots)
  gPath2dist(m, diag=FALSE, upper=FALSE, res.type=c("dist","vector"))
}
\arguments{
  \item{x}{a \linkS4class{gGraph} or a \linkS4class{gData}
    object. For plotting method of \code{gPath} objects, a \code{gPath} object.}
  \item{start}{a character string naming the 'source' node.}
  \item{from}{a vector of character strings giving node names.}
  \item{to}{a vector of character strings giving node names.}
  \item{col}{a character string indicating a color or
    a palette of colors to be used for plotting edges.}
  \item{lwd}{a numeric value indicating the width of edges.}
  \item{m}{a \code{gPath} object obtained by \code{dijkstraBetween}.}
  \item{diag,upper}{unused parameters added for consistency with
    \code{as.dist}.}
  \item{res.type}{a character string indicating what type of result
  should be returned: a \code{dist} object ('dist'), or a vector of
  distances ('vector'). Note that 'dist' should only be required for
  pairwise data, as output by dijkstraBetween (as opposed to dijkstraFrom).}
  \item{\dots}{further arguments passed to the \code{segments} method.}
}
\details{
  In 'dijkstraBetween', paths are seeked all possible pairs of
  nodes between 'from' and 'to'.
}
\value{
  A "gPath" object. These are basically the outputs of RBGL's
  \code{sp.between} function (see \code{?sp.between}), with a class
  attribute set to "gPath", and an additional slot 'xy' containing
  geographic coordinates of the nodes involved in the paths.\cr
}
\author{Thibaut Jombart (\email{t.jombart@imperial.ac.uk})}
\examples{
\dontrun{

## plotting
world <- worldgraph.40k
par(mar=rep(.1,4))
plot(world, reset=TRUE)

## check connectivity
isConnected(hgdp) # must be ok

## Lowest cost path from an hypothetical origin
ori.coord <- list(33,10) # one given location long/lat
points(data.frame(ori.coord), pch="x", col="black", cex=3) # an 'x' shows the putative origin
ori <- closestNode(world, ori.coord) # assign it the closest node

myPath <- dijkstraFrom(hgdp, ori) # compute shortest path

## plotting
plot(world,pch="") # plot the world
points(hgdp, lwd=3) # plot populations
points(data.frame(ori.coord), pch="x", col="black", cex=3) # add origin
plot(myPath) # plot the path
}
}
\keyword{methods}
\keyword{spatial}
