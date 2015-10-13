\encoding{UTF-8}
\name{makeGrid}
\alias{makeGrid}
\title{Build a regular grid gGraph}
\description{
  The function \code{makeGrid} builds a \linkS4class{gGraph} using a
  regular grid for a given area. If no area is specified, currently
  plotted area is used. Note that such grid is only valid for small
  scales, for cases in which curvature of the surface of the earth can
  be neglected.
}
\usage{
makeGrid(size=NULL, n.lon=NULL, n.lat=NULL, lon.range=NULL, lat.range=NULL)
}
\arguments{
  \item{size}{an integer giving the approximate number of nodes of the
    grid. The function will attempt to make a square grid of (approximately) this size.}
  \item{n.lon}{the number of longitude coordinates of the grid (i.e.,
  width of the grid, in number of cells)}
  \item{n.lat}{the number of latitude coordinates of the grid (i.e.,
    height of the grid, in number of cells)}
  \item{lon.range, lat.range}{vectors of length two giving the range
    covered by the grid, in longitude and latitude, respectively.}
}
\value{
  A \linkS4class{gGraph} object.
}
\author{Thibaut Jombart (\email{t.jombart@imperial.ac.uk})}
\examples{
## zoom in to a smaller area
data(worldgraph.10k)
plot(worldgraph.10k)
geo.zoomin(c(-10,0, 50,54))


## make a new gGraph
newGraph <- makeGrid(1e3)
newGraph <- findLand(newGraph)
newGraph@meta$colors <- data.frame(habitat=c("sea","land"),
color=c("blue","green"))


## plot the new gGraph
plot(newGraph, reset=TRUE, edge=TRUE)
}
\keyword{utilities}
\keyword{methods}