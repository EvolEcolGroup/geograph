\encoding{UTF-8}
\name{connectivity-checks}
\alias{areNeighbours}
\alias{areConnected}
\alias{isConnected,gData-method}
\alias{isReachable}
\alias{connectivityPlot}
\alias{connectivityPlot-methods}
\alias{connectivityPlot,gGraph-method}
\alias{connectivityPlot,gData-method}
\title{Check connectivity of a gGraph object}
\description{
  The functions \code{areNeighbours}, \code{areConnected} and the method
  \code{isConnected} test connectivity in different ways.\cr

  - \code{areNeighbours}: tests connectivity between couples of nodes on
  an object inheriting \code{graph} class (like a
  \linkS4class{graphNEL} object).\cr

  - \code{areConnected}: tests if a set of nodes form a connected set on
  a \linkS4class{gGraph} object.\cr

  - \code{isConnected}: tests if the nodes of a \linkS4class{gData}
  object form a connected set. Note that this is a method for
  \linkS4class{gData}, the generic being defined in the \code{graph}
  package.\cr

  - \code{isReachable}: tests if one location (actually, the closest
  node to it) is reachable from the set of nodes of a
  \linkS4class{gData} object.\cr

  - \code{connectivityPlot}: plots connected sets of a
  \linkS4class{gGraph} or a \linkS4class{gData} object with different
  colors.\cr
  
}
\usage{
areNeighbours(V1, V2, graph)
areConnected(x, nodes)
\S4method{isConnected}{gData}(object, \dots)
isReachable(x, loc)
\S4method{connectivityPlot}{gGraph}(x, \dots, seed=NULL)
\S4method{connectivityPlot}{gData}(x, col.gGraph=0, \dots, seed=NULL)
}
\arguments{
  \item{V1}{a vector of node names}
  \item{V2}{a vector of node names}
  \item{graph}{a valid \linkS4class{graphNEL} object.}
  \item{x}{a valid \linkS4class{gGraph} object.}
  \item{nodes}{a vector of node names}
  \item{object}{a valid \linkS4class{gData} object.}
  \item{\dots}{other arguments passed to other methods.}
  \item{loc}{location, specified as a list of two components giving
    respectively the longitude and the latitude. Alternatively, it can
    be a matrix-like object with one row and two columns.}
  \item{seed}{an optional integer giving the seed to be used when
    randomizing colors. One given seed will always give the same set of
    colors. NULL by default, meaning colors are randomized each time a
    plot is drawn.}
  \item{col.gGraph}{a character string or a number indicating the color
    of the nodes to be used when plotting the \linkS4class{gGraph}
    object. Defaults to '0', meaning that nodes are invisible.}
}
\value{
  - \code{areNeighbours}: a vector of logical, having one value for each
  couple of nodes.\cr

  - \code{areConnected}: a single logical value, being TRUE if nodes
    form a connected set.\cr

  - \code{isConnected}: a single logical value, being TRUE if nodes
    of the object form a connected set.\cr
}
\details{
  In \code{connectivityPlot}, isolated nodes (i.e. belonging to no
    connected set of size > 1) are plotted in light grey.
}
\author{Thibaut Jombart (\email{t.jombart@imperial.ac.uk})}
\examples{
connectivityPlot(rawgraph.10k)
connectivityPlot(worldgraph.10k)
}
\keyword{utilities}
\keyword{methods}
