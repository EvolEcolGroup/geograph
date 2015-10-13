\encoding{UTF-8}
\name{setCosts}
\alias{setCosts}
\title{Set friction in a gGraph object}
\description{
  The function \code{setCosts} define costs for the edges of a
  \linkS4class{gGraph} object according to a node attribute and some
  rules defined in the \code{@meta\$costs} slot of the object. Each node
  has a value for the chosen attribute, which is associated to a costs
  (a friction). The cost of an edge is computed as a function (see
  argument \code{method}) of the costs of its nodes.\cr

  Note that costs are inversely proportionnal to connectivity between
  edges: the larger the cost associated to an edge, the lower the
  connectivity between the two concerned nodes.\cr

  Also note that 'costs' defined in \code{geoGraph} are equivalent to
  'weights' as defined in \code{graph} and \code{RBGL} packages.
}
\usage{
setCosts(x, attr.name=NULL, node.costs=NULL, method=c("mean", "product"))
}
\arguments{
  \item{x}{a \linkS4class{gGraph} object with a least one node
    attribute, and a \code{@meta$costs} component (for an example, see
    worldgraph.10k dataset).}
  \item{attr.name}{the name of the node attribute used to compute
    costs (i.e., of one column of \code{@nodes.attr}).}
  \item{node.costs}{a numeric vector giving costs associated to the
  nodes. If provided, it will be used instead of \code{attr.name}.}
  \item{method}{a character string indicating which method should
    be used to compute edge cost from nodes costs. Currently available
    options are 'mean' and 'prod', where the cost associated to an edge
    is respectively computed as the mean, or as the product of the costs
    of its nodes.}
}
\value{
  A \linkS4class{gGraph} object with the newly defined costs used as
  weightings of edges.
}
\seealso{
  \code{\link{dropDeadEdges}}, to get rid of edge whose cost is below a
  given threshold. \code{\link{geo.add.edges}} to add edges to a \linkS4class{gGraph} object.
}
\author{Thibaut Jombart (\email{t.jombart@imperial.ac.uk})}
\examples{
data(rawgraph.10k)
plot(rawgraph.10k, reset=TRUE)

## zooming in
geo.zoomin(list(x=c(-6,38), y=c(35,73)))
title("Europe")

## defining a new object restrained to visible nodes
x <- rawgraph.10k[isInArea(rawgraph.10k)]

## define weights for edges
x <- setCosts(x, attr.name="habitat")
plot(x,edges=TRUE)
title("costs defined by habitat (land/land=1, other=100)")

}
\keyword{utilities}
