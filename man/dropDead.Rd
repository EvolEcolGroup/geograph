\encoding{UTF-8}
\name{dropDeadEdges}
\alias{dropDeadEdges}
\alias{dropDeadNodes}
\title{Get rid of some 'dead' edges or nodes}
\description{
  The functions \code{dropDeadEdges} and \code{dropDeadNodes} are used
  to remove 'dead edges' and 'dead nodes'.\cr

  Dead edges are edges associated to a prohibitive cost, that is, edges that no longer imply
  connectivity between two nodes.\cr

  Dead nodes are nodes that are not connected to any other node, thus
  not having any role in the connectivity of a graph.\cr
}
\usage{
dropDeadEdges(x, thres)
dropDeadNodes(x)
}
\arguments{
  \item{x}{a valid \linkS4class{gGraph}.}
  \item{thres}{a numeric value indicating the threshold cost for
  an edge to be removed. All costs strictly greater than \code{thres}
  will be removed.}
}
\value{
  A \linkS4class{gGraph} object.
}
\author{Thibaut Jombart (\email{t.jombart@imperial.ac.uk})}
\examples{
\dontrun{
data(worldgraph.10k)
plot(worldgraph.10k,reset=TRUE)
x <- dropDeadNodes(worldgraph.10k)
plot(x)
}
}
\keyword{utilities}
\keyword{methods}