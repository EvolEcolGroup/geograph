#' Human genome diversity panel - georeferenced data
#'
#' The datasets \code{hgdp} and \code{hgdpPlus} provides genetic diversity
#' several human populations worldwide. Both datasets are \linkS4class{gData}
#' objects, interfaced with the \linkS4class{gGraph} object
#' \code{\link{worldgraph.40k}}.\cr
#'
#' \code{hgdp} describes 52 populations from the original Human Genome
#' Diversity Panel. \cr
#'
#' \code{hgdpPlus} describes \code{hgdp} populations plus 24 native American
#' populations. \cr
#'
#'
#' @name hgdp
#' @aliases hgdp hgdpPlus
#' @docType data
#' @format \code{hgdp} is a \linkS4class{gGraph} object with the following
#' data: % \describe{ % \item{@nodes.attr$habitat}{habitat corresponding to
#' each % vertice; currently 'land' or 'sea'.} % \item{@meta$color}{a matrix
#' assigning a color for plotting % vertices (second column) to different
#' values of habitat (first % column).} % }
#' @references Authors \emph{Journal}, YEAR, \bold{nb}: pp-pp.
#' @keywords datasets
#' @examples
#'
#' ## check object
#' hgdp
#'
#' ## plotting the object
#' plot(hgdp)
#'
#'
#' ## results from Handley et al.
#' \dontrun{
#' ## Addis Ababa
#' addis <- list(lon=38.74,lat=9.03)
#' addis <- closestNode(worldgraph.40k,addis) # this takes a while
#'
#' ## shortest path from Addis Ababa
#' myPath <- dijkstraFrom(hgdp, addis)
#'
#' ## plot results
#' plot(worldgraph.40k, col=0)
#' points(hgdp)
#' points(worldgraph.40k[addis], psize=3,pch="x", col="black")
#' plot(myPath)
#'
#' ## correlations distance/genetic div.
#' geo.dist <- sapply(myPath[-length(myPath)],function(e) e$length)
#' gen.div <- getData(hgdp)[,"Genetic.Div"]
#' plot(gen.div~geo.dist)
#' lm1 <- lm(gen.div~geo.dist)
#' abline(lm1, col="blue") # this regression is wrong
#' summary(lm1)
#' }
#'
NULL

#' Worldwide geographic graphs
#'
#' The datasets 'rawgraph.10k', 'rawgraph.40k', 'worldgraph.10k', and
#' 'worldgraph.40k' are geographic graphs (\linkS4class{gGraph} objects) of the
#' world, with respective resolutions of 10,242 and 40,962 vertices.\cr
#'
#' 'rawgraph's are raw graphs as obtained directly from the method provided in
#' references.\cr
#'
#' 'worldgraph's are 'rawgraph's that have been modified manually to rectify
#' connectivity between edges at some places. The most noticeable change is that
#' all edges involving sea vertices have been removed.\cr
#'
#' 'worldshape' is a shapefile of countries of the world (snapshot from 1994).
#'
#'
#' @name worldgraph
#' @aliases worldgraph rawgraph.10k rawgraph.40k worldgraph.10k worldgraph.40k
#' worldshape
#' @docType data
#' @format \code{worldgraph.10k} and \code{worldgraph.40k} are
#' \linkS4class{gGraph} objects with the following specificities: \describe{
#' \item{@nodes.attr\$habitat}{habitat corresponding to each vertice; currently
#' 'land' or 'sea'.} \item{@meta\$color}{a matrix assigning a color for
#' plotting vertices (second column) to different values of habitat (first
#' column).} }
#' @references === On the construction of the graph ===\cr Randall, D. A.;
#' Ringler, T. D.; Heikes, R. P.; Jones, P. & Baumgardner, J. Climate Modeling
#' with Spherical Geodesic Grids \emph{Computing in science & engineering},
#' 2002, \bold{4}: 32-41.
#' @source Graph reconstructed by Andrea Manica.
#' @keywords datasets
#' @examples
#'
#'
#' worldgraph.10k
#'
#' ## plotting the object
#' plot(worldgraph.10k, reset=TRUE)
#' title("Hello world")
#'
#' ## zooming in
#' geo.zoomin(list(x=c(-12,45), y=c(33,75)))
#' title("Europe")
#' geo.zoomin(list(x=c(-12,2), y=c(50,60)))
#' plotEdges(worldgraph.10k)
#' title("United Kingdom")
#'
#' ## zooming out
#' # geo.zoomout() # needs clicking on device
#' geo.zoomin(list(x=c(-6,38), y=c(35,73)))
#' title("Europe")
#'
#' ## defining the subset of visible points
#' x <- worldgraph.10k[isInArea(worldgraph.10k)]
#' plot(x,reset=TRUE, edges=TRUE)
#' title("One subsetted object.")
#'
#' \dontrun{
#' ## interactive zooming
#' geo.zoomin()
#' }
#'
#'
NULL


