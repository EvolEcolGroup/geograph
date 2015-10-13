############
## makeGrid
############
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
    zoomLog <- get("zoom.log", env=geoEnv)
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
