\encoding{UTF-8}
\name{getCosts}
\alias{getCosts}
\alias{getCosts-methods}
\alias{getCosts,gGraph-method}
\alias{getNodeCosts}
\alias{getNodeCosts-methods}
\alias{getNodeCosts,gGraph-method}
\title{Get costs associated to edges of a gGraph object}
\description{
  The function \code{getCosts} returns the costs associated to the edges of a
  \linkS4class{gGraph} object using different possible outputs. These
  outputs are designed to match possible outputs of
  \code{\link{getEdges}} function.

  \code{getNodeCosts} returns the costs associated to nodes based on one
  node attribute.
}
\usage{
getCosts(x, \dots)
\S4method{getCosts}{gGraph}(x, res.type=c("asIs","vector"), unique=FALSE, \dots)
getNodeCosts(x, \dots)
\S4method{getNodeCosts}{gGraph}(x, attr.name, \dots)
}
\arguments{
  \item{x}{a valid \linkS4class{gGraph}.}
  \item{res.type}{a character string indicating which kind of output
    should be used. See value.}
  \item{unique}{a logical indicating whether the costs should be
    returned for unique edges (TRUE), or if duplicate edges should be
    considered as well (TRUE, default).}
  \item{attr.name}{the name of the node attribute used to define node costs.}
  \item{\dots}{other arguments passed to other methods (currently
    unused).}
}
\details{
  The notion of 'costs' in the context of \linkS4class{gGraph} objects is
  identical to the concept of 'weights' in \linkS4class{graph} (and thus
  \linkS4class{graphNEL}) objects. The larger it is for an edge, the
  less connectivity there is between the couple of concerned nodes.
}
\value{
  The output depends on the value of the argument \code{res.type}:\cr
  - \code{asIs}: output is a named list of weights, each slot containing
  weights associated to the edges stemming from one given node. This format is that of the
  \code{weights} accessor for \linkS4class{graphNEL} objects.\cr

  - \code{vector}: a vector of weights; this output matches matrix
    outputs of \code{\link{getEdges}}.\cr
}
\seealso{
  Most other accessors are documented in \linkS4class{gGraph} manpage.\cr
}
\author{Thibaut Jombart (\email{t.jombart@imperial.ac.uk})}
\examples{
data(worldgraph.10k)

head(getEdges(worldgraph.10k, res.type="matNames",unique=TRUE))
head(getCosts(worldgraph.10k,res.type="vector",unique=TRUE))

}
\keyword{utilities}
\keyword{methods}