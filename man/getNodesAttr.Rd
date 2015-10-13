\encoding{UTF-8}
\name{getNodesAttr}
\alias{getNodesAttr}
\alias{getNodesAttr-methods}
\alias{getNodesAttr,gGraph-method}
\alias{getNodesAttr,gData-method}
\title{Get nodes attributes from gGraph/gData object}
\description{
  The function \code{getNodesAttr} returns the values of a set of
  variables associated to the nodes (i.e. node attributes) of a
  \linkS4class{gGraph} or \linkS4class{gData} object.
}
\usage{
getNodesAttr(x, \dots)
\S4method{getNodesAttr}{gGraph}(x, nodes=NULL, attr.name=NULL, \dots)
\S4method{getNodesAttr}{gData}(x, attr.name=NULL, \dots)
}
\arguments{
  \item{x}{a valid \linkS4class{gGraph} or \linkS4class{gData} object.}
  \item{nodes}{an optional integer, logical, or character string indicating the
    subset of nodes to be used. If NULL, all nodes are used.}
  \item{attr.name}{an optional character string indicating which node
    attributes should be returned. If provided, it must match at least
    one of the columns of \code{x@nodes.attr}.}
  \item{\dots}{other arguments passed to other methods (currently
    unused).}
}
\value{
 A data.frame with the requested nodes attributes. Nodes are displayed
 in rows, variables in columns.
}
\seealso{
  Most other accessors are documented in \linkS4class{gGraph} and
  \linkS4class{gData} manpages.\cr
}
\author{Thibaut Jombart (\email{t.jombart@imperial.ac.uk})}
\examples{
## gGraph method
data(worldgraph.40k)
head(getNodesAttr(worldgraph.40k))


## gData method
data(hgdp)
getNodesAttr(hgdp)

}
\keyword{utilities}
\keyword{methods}