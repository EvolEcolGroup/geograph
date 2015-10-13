\encoding{UTF-8}
\name{getEdges}
\alias{getEdges}
\alias{getEdges-methods}
\alias{getEdges,gGraph-method}
\title{Get edges from a gGraph object}
\description{
  The function \code{getEdges} returns the edges of a
  \linkS4class{gGraph} object using different possible outputs.
}
\usage{
getEdges(x, \dots)
\S4method{getEdges}{gGraph}(x, res.type=c("asIs","matNames", "matId"), unique=FALSE, \dots)
}
\arguments{
  \item{x}{a valid \linkS4class{gGraph}.}
  \item{res.type}{a character string indicating which kind of output
    should be used. See value.}
  \item{unique}{a logical indicating whether all returned edges
    should be unique (TRUE) or if duplicated edges should be allowed
    (TRUE, default).}
  \item{\dots}{other arguments passed to other methods (currently
    unused).}
}
\value{
  The output depends on the value of the argument \code{res.type}:\cr
  - \code{asIs}: output is a named list of nodes, each slot containing
  nodes forming an edge with one given node. This format is that of the
  \code{edges} accessor for \linkS4class{graphNEL} objects.\cr

  - \code{matNames}: a matrix with two columns giving couples of node
  names forming edges.\cr

  - \code{matId}: a matrix with two columns giving couples of node
  indices forming edges.\cr
}
\seealso{
  Most other accessors are documented in \linkS4class{gGraph} manpage.\cr

  See \code{\link{setEdges}} to add/remove edges, or
  \code{\link{geo.add.edges}} and \code{\link{geo.remove.edges}} for
  interactive versions.
}
\author{Thibaut Jombart (\email{t.jombart@imperial.ac.uk})}
\examples{
example(gGraph)

getEdges(x)
getEdges(x,res.type="matNames")
getEdges(x,res.type="matId")
}
\keyword{utilities}
\keyword{methods}