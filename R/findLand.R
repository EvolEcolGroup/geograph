#' Find which nodes are on land
#'
#' The generic function \code{findLand} uses information from a GIS shapefile
#' to define which nodes are on land, and which are not. Strickly speaking,
#' being 'on land' is in fact being inside a polygon of the shapefile.
#'
#' Nodes can be specified either as a matrix of geographic coordinates, or as a
#' \linkS4class{gGraph} object.
#'
#'
#' @aliases findLand findLand-methods findLand,matrix-method
#' findLand,data.frame-method findLand,gGraph-method
#' @param x a matrix, a data.frame, or a valid \linkS4class{gGraph} object. For
#' matrix and data.frame, input must have two columns giving longitudes and
#' latitudes of locations being considered.
#' @param shape a shapefile of the class \code{SpatialPolygonsDataFrame} (see
#' \code{readShapePoly} in maptools package to import such data from a GIS
#' shapefile). Alternatively, a character string indicating one shapefile
#' released with geoGraph; currently, only 'world' is available (see
#' \code{?data(worldshape)}).
#' @param \dots further arguments to be passed to other methods. Currently not
#' used.
#' @param attr.name a character string giving the name of the node attribute in
#' which the output is to be stored.
#' @return The output depends on the nature of the input:\cr - \code{matrix,
#' data.frame}: a factor with two levels being 'land' and 'sea'.\cr
#'
#' - \code{gGraph}: a \linkS4class{gGraph} object with a new node attribute,
#' possibly added to previously existing node attributes (\code{@nodes.attr}
#' slot).\cr
#' @author Thibaut Jombart (\email{t.jombart@@imperial.ac.uk})
#' @seealso \code{\link{extractFromLayer}}, to retrieve any information from a
#' GIS shapefile.
#' @keywords utilities methods
#' @name findLand
#' @examples
#'
#'
#' ## create a new gGraph with random coordinates
#' myCoords <- data.frame(long=runif(1000,-180,180), lat=runif(1000,-90,90))
#' obj <- new("gGraph", coords=myCoords)
#' obj # note: no node attribute
#' plot(obj)
#'
#' ## find which points are on land
#' obj <- findLand(obj)
#' obj # note: new node attribute
#'
#' ## define rules for colors
#' temp <- data.frame(habitat=c("land","sea"), color=c("green","blue"))
#' temp
#' obj@meta$color <- temp
#'
#' ## plot object with new colors
#' plot(obj)
#'
#'
NULL


############
## findLand
############
#' @rdname findLand
#' @export
setGeneric("findLand", function(x,...) {
    standardGeneric("findLand")
})






################
## for matrices (of long/lat)
################
#' @rdname findLand
#' @export
setMethod("findLand", "matrix", function(x, shape="world", ...) {

    ## This functions automatically assigns to land all points overlapping the country polygons
#    if(!require(maptools)) stop("maptools package is required.")

    ## Load country shapefile
    if(is.character(shape) && shape[1]=="world"){
        shape <- worldshape
    }

    if(!is.null(shape)){ # with background
        if(!inherits(shape,"SpatialPolygonsDataFrame"))
          stop("Layer must be a SpatialPolygonsDataFrame object \n(see st_read and as_Spatial in sf to import such data from a GIS shapefile).")
    }

    long <- x[,1]
    lat <- x[,2]
    n.country <- length(shape@polygons)

    ## create land vector to score land
    land <- rep(0,length(lat))

    for(i in 1:n.country) {
        this.country <- shape@polygons[i][[1]]
        n.polys <- length(this.country@Polygons)

        for (p in 1:n.polys) {
            this.poly <- this.country@Polygons[p][[1]]
            land <- land + point.in.polygon(long,lat, this.poly@coords[,1], this.poly@coords[,2])
        }
    }
    land[land>1] <- 1
    land[land==0] <- "sea"
    land[land==1] <- "land"

    return(factor(land))
})



################
## for data.frames (of long/lat)
################
#' @rdname findLand
#' @export
setMethod("findLand", "data.frame", function(x, shape="world",...){
    x <- as.matrix(x)
    return(findLand(x, shape=shape, ...))
}) # end findLand






##############
## for gGraph
##############
#' @rdname findLand
#' @export
setMethod("findLand", "gGraph", function(x, shape="world", attr.name="habitat",...){
    coords <- getCoords(x)
    res <- findLand(coords, shape=shape, ...)
    if(nrow(x@nodes.attr)>1){
        x@nodes.attr <- cbind.data.frame(x@nodes.attr, res)
        names(x@nodes.attr)[ncol(x@nodes.attr)] <- attr.name
    } else {
        x@nodes.attr <- data.frame(res)
        names(x@nodes.attr) <- attr.name
    }
    return(x)
}) # end findLand

