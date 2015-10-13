\encoding{UTF-8}
\name{findLand}
\alias{findLand}
\alias{findLand-methods}
\alias{findLand,matrix-method}
\alias{findLand,data.frame-method}
\alias{findLand,gGraph-method}
\title{Find which nodes are on land}
\description{
  The generic function \code{findLand} uses information from a GIS
  shapefile to define which nodes are on land, and which are
  not. Strickly speaking, being 'on land' is in fact being inside a
  polygon of the shapefile.
  
  Nodes can be specified either as a matrix of geographic coordinates,
  or as a \linkS4class{gGraph} object.
}
\usage{
findLand(x, \dots)
\S4method{findLand}{matrix}(x, shape="world", \dots)
\S4method{findLand}{data.frame}(x, shape="world", \dots)
\S4method{findLand}{gGraph}(x, shape="world", attr.name="habitat", \dots)
}
\arguments{
  \item{x}{a matrix, a data.frame, or a valid \linkS4class{gGraph}
    object. For matrix and data.frame, input must have two columns
    giving longitudes and latitudes of locations being considered.}
  \item{shape}{a shapefile of the class \code{SpatialPolygonsDataFrame}
    (see \code{readShapePoly} in maptools package to import such data
    from a GIS shapefile). Alternatively, a character string indicating
    one shapefile released with geoGraph; currently, only 'world' is
    available (see \code{?data(worldshape)}).}
  \item{\dots}{further arguments to be passed to other
    methods. Currently not used.}
  \item{attr.name}{a character string giving the name of the node
    attribute in which the output is to be stored.}
}
\value{
  The output depends on the nature of the input:\cr
  - \code{matrix, data.frame}: a factor with two levels being 'land' and
  'sea'.\cr
  
  - \code{gGraph}: a \linkS4class{gGraph} object with a new node
    attribute, possibly added to previously existing node attributes
    (\code{@nodes.attr} slot).\cr
}
\seealso{
  \code{\link{extractFromLayer}}, to retrieve any information from a GIS shapefile.
}
\author{Thibaut Jombart (\email{t.jombart@imperial.ac.uk})}
\examples{

## create a new gGraph with random coordinates
myCoords <- data.frame(long=runif(1000,-180,180), lat=runif(1000,-90,90))
obj <- new("gGraph", coords=myCoords)
obj # note: no node attribute 
plot(obj)

## find which points are on land
obj <- findLand(obj)
obj # note: new node attribute

## define rules for colors
temp <- data.frame(habitat=c("land","sea"), color=c("green","blue"))
temp
obj@meta$color <- temp

## plot object with new colors
plot(obj)

}
\keyword{utilities}
\keyword{methods}
