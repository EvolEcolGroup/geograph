\encoding{UTF-8}
\name{setEdges}
\alias{setEdges}
\alias{setEdges-methods}
\alias{setEdges,gGraph-method}
\title{Add and remove edges from a gGraph object}
\description{
  The function \code{setEdges} allows one to add or remove edges in a
  \linkS4class{gGraph} by directly specifying the relevant nodes, as a
  list or a data.frame. This low-level function is called by \code{geo.add.edges}
  and \code{geo.remove.edges}.
}
\usage{
setEdges(x, \dots)
\S4method{setEdges}{gGraph}(x, add=NULL, remove=NULL, costs=NULL, \dots)
}
\arguments{
  \item{x}{a valid \linkS4class{gGraph} object.}
  \item{add}{a list or a dataframe containing node names of edges
    to be added. The first element of the list (or column of the data.frame) gives
    starting nodes of edges; the second gives ending nodes. Hence, the
    nodes of the i-th edge are \code{add[[1]][i]} and \code{add[[2]][i]} if \code{add} is a
    list, and \code{add[i,]} if \code{add} is a data.frame.}
  \item{remove}{same as \code{add} argument, but edges are removed.}
  \item{costs}{a numeric vector providing costs of the edges
    to be added. \code{costs[i]} is the weight of the i-th edge.}
  \item{\dots}{other arguments passed to other methods (currently
    unused).}
}
\value{
  A \linkS4class{gGraph} object with newly added or removed edges.
}
\seealso{
  \code{\link{geo.add.edges}} and \code{\link{geo.remove.edges}} to interactively add or remove edges in a
  \linkS4class{gGraph} object. \cr

  \code{\link{getEdges}} to retrieve edges in different formats.
}
\author{Thibaut Jombart (\email{t.jombart@imperial.ac.uk})}
\keyword{utilities}
\keyword{methods}