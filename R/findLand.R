############
## findLand
############
setGeneric("findLand", function(x,...) {
    standardGeneric("findLand")
})






################
## for matrices (of long/lat)
################
setMethod("findLand", "matrix", function(x, shape="world", ...) {

    ## This functions automatically assigns to land all points overlapping the country polygons
    if(!require(maptools)) stop("maptools package is required.")

    ## Load country shapefile
    if(is.character(shape) && shape[1]=="world"){
        if(!require(sp)) stop("sp package needed to map the world")
        data(worldshape)
        shape <- worldshape
    }

    if(!is.null(shape)){ # with background
        if(!inherits(shape,"SpatialPolygonsDataFrame"))
            stop("Shape must be a SpatialPolygonsDataFrame object \n(see readShapePoly in maptools to import such data from a GIS shapefile).")
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
setMethod("findLand", "data.frame", function(x, shape="world",...){
    x <- as.matrix(x)
    return(findLand(x, shape=shape, ...))
}) # end findLand






##############
## for gGraph
##############
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

