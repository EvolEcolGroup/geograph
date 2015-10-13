\encoding{UTF-8}
\name{extractFromLayer}
\alias{extractFromLayer}
\alias{extractFromLayer-methods}
\alias{extractFromLayer,matrix-method}
\alias{extractFromLayer,data.frame-method}
\alias{extractFromLayer,list-method}
\alias{extractFromLayer,gGraph-method}
\alias{extractFromLayer,gData-method}
\title{Retrieves node attributes from a layer}
\description{
  The generic function \code{extractFromLayer} uses information from a GIS
  shapefile to define node attributes. For each node, information is
  retrieved from the layer and assigned to that node.\cr

  Nodes can be specified in different ways, including by providing
  a \linkS4class{gGraph} or a \linkS4class{gData} object. Outputs match 
  the input formats.
}
\usage{
extractFromLayer(x, \dots)
\S4method{extractFromLayer}{matrix}(x, layer="world", attr="all",\dots)
\S4method{extractFromLayer}{data.frame}(x, layer="world", attr="all",\dots)
\S4method{extractFromLayer}{list}(x, layer="world", attr="all",\dots)
\S4method{extractFromLayer}{gGraph}(x, layer="world", attr="all",\dots)
\S4method{extractFromLayer}{gData}(x, layer="world", attr="all",\dots)
}
\arguments{
  \item{x}{a matrix, a data.frame, a list, a valid \linkS4class{gGraph},
    or a valid \linkS4class{gData} object. For matrix and data.frame,
    input must have two columns giving longitudes and latitudes of
    locations being considered. For list, input must have two components
    being vectors giving longitudes and latitudes of locations.}
  \item{layer}{a shapefile of the class \code{SpatialPolygonsDataFrame}
    (see \code{readShapePoly} in maptools package to import such data
    from a GIS shapefile). Alternatively, a character string indicating
    one shapefile released with geoGraph; currently, only 'world' is
    available (see \code{?data(worldshape)}).}
  \item{attr}{a character vector giving names of the variables to
    be extracted from the layer. If 'all', all available variables are
    extracted. In case of problem, available names are displayed with the
    error message. Available data are also stored in \code{layer@data}.}
  \item{\dots}{further arguments to be passed to other
  methds. Currently not used.}
}
\value{
  The output depends on the nature of the input:\cr
  - \code{matrix, data.frame, list}: a data.frame with one row per
  location, and as many columns as requested variables ('attributes').\cr

  - \code{gGraph}: a \linkS4class{gGraph} object with new node
    attributes (\code{@nodes.attr} slot). If nodes attributes already
    existed, new attributes are added as new columns.\cr

  - \code{gData}: a \linkS4class{gData} object with new data associated
  to locations (\code{@data} slot). New information is merge to older
  information according to the type of data being stored. \cr
}
\seealso{
  \code{\link{findLand}}, to find which locations are on land.
}
\author{Thibaut Jombart (\email{t.jombart@imperial.ac.uk})}
\examples{
\dontrun{
data(worldgraph.10k)
data(worldshape)

plot(worldgraph.10k, reset=TRUE)


## see what info is available
names(worldshape@data)
unique(worldshape@data$CONTINENT)


## retrieve continent info for all nodes
## (might take a few seconds)
x <- extractFromLayer(worldgraph.10k, layer=worldshape, attr="CONTINENT") 
x
table(getNodesAttr(x, attr.name="CONTINENT"))


## subset Africa
temp <- getNodesAttr(x, attr.name="CONTINENT")=="Africa"
temp[is.na(temp)] <- FALSE
x <- x[temp]
plot(x, reset=TRUE)

}
}
\keyword{utilities}
\keyword{methods}