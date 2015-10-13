\encoding{UTF-8}
\name{hgdp}
\alias{hgdp}
\alias{hgdpPlus}
\docType{data}
\title{Human genome diversity panel - georeferenced data}
\description{
  The datasets \code{hgdp} and \code{hgdpPlus} provides genetic diversity several human
  populations worldwide. Both datasets are \linkS4class{gData} objects,
  interfaced with the \linkS4class{gGraph} object
  \code{\link{worldgraph.40k}}.\cr

  \code{hgdp} describes 52 populations from the original Human Genome
  Diversity Panel. \cr

  \code{hgdpPlus} describes \code{hgdp} populations plus 24 native
  American populations. \cr
}
\usage{
data(hgdp)
data(hgdpPlus)
}
\format{
  \code{hgdp} is a \linkS4class{gGraph} object with the following data:
%   \describe{
%     \item{@nodes.attr$habitat}{habitat corresponding to each
%       vertice; currently 'land' or 'sea'.}
%     \item{@meta$color}{a matrix assigning a color for plotting
%       vertices (second column) to different values of habitat (first
%       column).}	
%   }
}
\references{
 Authors \emph{Journal}, YEAR, \bold{nb}: pp-pp.
}
\examples{
## check object
hgdp

## plotting the object
plot(hgdp)


## results from Handley et al.
\dontrun{
## Addis Ababa
addis <- list(lon=38.74,lat=9.03)
addis <- closestNode(worldgraph.40k,addis) # this takes a while

## shortest path from Addis Ababa
myPath <- dijkstraFrom(hgdp, addis)

## plot results
plot(worldgraph.40k, col=0)
points(hgdp)
points(worldgraph.40k[addis], psize=3,pch="x", col="black")
plot(myPath)

## correlations distance/genetic div.
geo.dist <- sapply(myPath[-length(myPath)],function(e) e$length)
gen.div <- getData(hgdp)[,"Genetic.Div"]
plot(gen.div~geo.dist)
lm1 <- lm(gen.div~geo.dist)
abline(lm1, col="blue") # this regression is wrong
summary(lm1)
}
}
\keyword{datasets}
