\encoding{UTF-8}
\name{geo.change.attr}
\alias{geo.change.attr}
\title{Change values of a node attribute}
\description{
  The functions \code{geo.change.attr} changes values of a given node
  attribute for a set of selected nodes of a \linkS4class{gGraph}
  object.
}
\usage{
geo.change.attr(x, mode=c("points","area"), attr.name, attr.value,
                only.name=NULL, only.value=NULL, newCol="black",
                restore.edges=FALSE, refObj = "rawgraph.40k")
}
\arguments{
  \item{x}{a valid \linkS4class{gGraph} object.}
  \item{mode}{a character string indicating whether selected nodes are
    clicked one by one ('points') or by defining a rectangular area ('area').}
  \item{attr.name}{the name of the node attribute to be modified.}
  \item{attr.value}{the new value of attribute assigned to
    selected nodes.}
  \item{only.name}{(optional) in area mode, the name of a node
    attribute to add an extra selection criterion. See details.}
  \item{only.value}{(optional) in area mode, and if
    \code{only.name} is specified, the values of \code{only.name} that
    can be selected. See details.}
  \item{newCol}{a character string giving the new color for the
    attribute value.}
  \item{restore.edges}{a logical indicating whether edges stemming from
    the modified nodes should be re-added to the graph, using
    \code{refObj} as a reference. This is useful when connectivity is to
  be redefined using \code{\link{setCosts}} for nodes that were
  previously disconnected.}
  \item{refObj}{a character string or a \linkS4class{gGraph} object,
    used as reference when re-adding edges. If a character string is
    provided, it must match one of the following dataset:
    'rawgraph.10k', 'rawgraph.40k'.}
}
\value{
  A \linkS4class{gGraph} object with modified node attributes.
}
\details{
  The argument \code{only.name} allows one to perform a more accurate
  selection of nodes whose attribute is changed, by specifying values
  (\code{only.value}) of an attribute (\code{only.name}) that can be
  selected. For instance, one may want to define new attributes for
  nodes of worldgraph.10k that are exclusively on land: this would be done by specifying
  \code{only.name="habitat"} and \code{only.value="land"}.
}
\author{Thibaut Jombart (\email{t.jombart@imperial.ac.uk})}
\examples{
\dontrun{
plot(worldgraph.10k, reset=TRUE)

## have to click here for an area
## all nodes are modified in the area
x <- geo.change.attr(worldgraph.10k, mode="area", attr.name="habitat", attr.value="fancy
habitat", newCol="pink") # modify selected area

plot(x,reset=TRUE) # modification in the whole selected area

## have to click here for an area
## only nodes on land are modified
x <- geo.change.attr(x, mode="area", attr.name="habitat", attr.value="fancy2
habitat", newCol="purple", only.name="habitat", only.value="land") 

plot(x,reset=TRUE) # modification in the whole selected area
}
}
\keyword{utilities}
