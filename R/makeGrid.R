############
## makeGrid
############


#' Build a regular grid gGraph
#'
#' The function \code{makeGrid} builds a \linkS4class{gGraph} using a regular
#' grid for a given area. If no area is specified, currently plotted area is
#' used. Note that such grid is only valid for small scales, for cases in which
#' curvature of the surface of the earth can be neglected.
#'
#'
#' @param size an integer giving the approximate number of nodes of the grid.
#' The function will attempt to make a square grid of (approximately) this
#' size.
#' @param n.lon the number of longitude coordinates of the grid (i.e., width of
#' the grid, in number of cells)
#' @param n.lat the number of latitude coordinates of the grid (i.e., height of
#' the grid, in number of cells)
#' @param lon.range,lat.range vectors of length two giving the range covered by
#' the grid, in longitude and latitude, respectively.
#' @return A \linkS4class{gGraph} object.
#' @author Thibaut Jombart (\email{t.jombart@@imperial.ac.uk})
#' @keywords utilities methods
#' @examples
#'
#' ## zoom in to a smaller area
#' plot(worldgraph.10k)
#' geo.zoomin(c(-10,0, 50,54))
#'
#'
#' ## make a new gGraph
#' newGraph <- makeGrid(1e3)
#' newGraph <- findLand(newGraph)
#' newGraph@meta$colors <- data.frame(habitat=c("sea","land"),
#' color=c("blue","green"))
#'
#'
#' ## plot the new gGraph
#' plot(newGraph, reset=TRUE, edge=TRUE)
#'
makeGrid <- function(size=NULL, n.lon=NULL, n.lat=NULL, lon.range=NULL, lat.range=NULL){
    ## HANDLE ARGUMENTS ##
    if(is.null(n.lon)){
        if(is.null(size)) stop("Please provide either size or n.lon/n.lat")
         n.lon <- round(sqrt(size))
    }

    if(is.null(n.lat)){
        if(is.null(size)) stop("Please provide either size or n.lon/n.lat")
        n.lat <- round(sqrt(size))
    }

    if(is.null(size)){
        if(is.null(n.lon)|is.null(n.lat)) stop("Please provide either size or n.lon/n.lat")
    }

    size <- n.lon*n.lat
    if(size<4.1){
        size <- 4
        warning("Minimum grid size is 4 - ignoring required size.")
    }


    ## GET LON/LAT FROM ZOOM LOG ##
    ## get zoom log info
    geoEnv <- get(".geoGraphEnv", envir=.GlobalEnv)
    zoomLog <- get("zoom.log", envir=geoEnv)
    if(nrow(zoomLog)<1) {
        curZoom <- c(-180,180,-90,90)
    } else {
        curZoom <- zoomLog[1,]
    }

    if(is.null(lon.range)){
        lon.range <- curZoom[1:2]
    }

    if(is.null(lat.range)){
        lat.range <- curZoom[3:4]
    }

    lon.range <- sort(lon.range)
    lat.range <- sort(lat.range)


    ## CORRECT LON/LAT ##
    if(lon.range[1] < -180){
        lon.range[1] <- -180
        warning("Setting lowest longitude to -180 (i.e. 180 W)")
    }

    if(lon.range[2] > 180){
        lon.range[2] <- 180
        warning("Setting largest longitude to 180 (i.e. 180 E)")
    }

    if(lat.range[1] < -90){
        lat.range[1] <- -90
        warning("Setting lowest latitude to -90 (i.e. 90 S)")
    }

    if(lat.range[2] > 90){
        lat.range[2] <- 90
        warning("Setting largest latitude to 90 (i.e. 90 N)")
    }


    ## BUILD GRID AND FROM/TO MATRIX ##
    x.vec <- seq(lon.range[1], lon.range[2], length=n.lon)
    y.vec <- seq(lat.range[2], lat.range[1], length=n.lat)
    x <- rep(x.vec, each=n.lat)
    y <- rep(y.vec, n.lon)
    xy <- cbind(x,y)
    colnames(xy) <- c("x","y")


    ## lateral connections
    from <- 1:(size-n.lat)
    to <- n.lat + (1:(size-n.lat))

    ## vertical connections
    temp <- setdiff(1:(size-1), seq(n.lat, size, length=n.lon))
    from <- c(from, temp)
    to <- c(to, temp+1)
    ft <- cbind(from, to)

    ## CREATE graphNEL ##
    myGraph <- ftM2graphNEL(ft, V=as.character(1:size), edgemode="undirected")


    ## CREATE gGraph ##
    res <- new("gGraph", coords=xy, graph=myGraph)

    return(res)

} # end makeGrid
