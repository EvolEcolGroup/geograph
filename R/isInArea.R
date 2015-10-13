############
## isInArea
############
setGeneric("isInArea", function(x, ...) {
    standardGeneric("isInArea")
})




################
## method for matrix
################
setMethod("isInArea", "matrix", function(x, reg="current", res.type=c("logical","integer","character"), buffer=0){
    ## some checks / definitiona
    res.type <- match.arg(res.type)
    env <- get(".geoGraphEnv", envir=.GlobalEnv) # env is our target environnement
    coords <- x

    ## get xlim and ylim
    if(exists("zoom.log", envir=env) && length(reg)==1 && reg=="zoom"){ # xlim/ylim taken from log
        zoomlog <- get("zoom.log", envir=env)
        zoomlog <- zoomlog[1,]

        xlim <- zoomlog[1:2]
        ylim <- zoomlog[3:4]

    } else if(length(reg)==1 && reg=="current"){ # xlim/ylim taken from par("usr")
        xlim <- sort(par("usr")[1:2])
        ylim <- sort(par("usr")[3:4])

    }  else if(is.list(reg)){ # xlim/ylim user-provided (reg)
        if(length(reg)!=2) stop("reg is not a list of length 2.")
        xlim <- sort(reg[[1]])[1:2]
        ylim <- sort(reg[[2]])[1:2]

    } else return(NA)


    ## main computations ##

    ## handle a buffer around area
    bufferx <- (xlim[2]-xlim[1])*buffer
    buffery <- (ylim[2]-ylim[1])*buffer

    xlim <- xlim + c(-bufferx, bufferx)
    ylim <- ylim + c(-buffery, buffery)

    toKeep <- ( (coords[,1] >= xlim[1]) & (coords[,1] <= xlim[2])  # matching longitude
               & (coords[,2] >= ylim[1]) & (coords[,2] <= ylim[2]) ) # matching latitude

    names(toKeep) <- rownames(coords)

    if(res.type=="logical"){ # return a named vector of logicals
        return(toKeep)
    }

    if(res.type=="integer"){ # return a named vector of node numbers
        return(which(toKeep))
    }

    if(res.type=="character"){ # return names of nodes in the area
        res <- names(toKeep)[toKeep]
        return(res)
    }

}) # end isInArea for matrix






################
## method for data.frame
################
setMethod("isInArea", "data.frame", function(x, reg="current", res.type=c("logical","integer","character"), buffer=0){

    ## preliminary stuff
    x <- as.data.frame(x)

    res <- isInArea(x=x, reg=reg, res.type=res.type, buffer=buffer)
    return(res)
}) # end isInArea for data.frame






################
## method for gGraph
################
setMethod("isInArea", "gGraph", function(x, reg="current", res.type=c("logical","integer","character"), buffer=0){

    ## preliminary stuff
    if(!is.gGraph(x)) stop("x is not a valid gGraph object")
    coords <- getCoords(x)

    res <- isInArea(x=coords, reg=reg, res.type=res.type, buffer=buffer)
    return(res)
}) # end isInArea for gGraph






################
## method for gData
################
setMethod("isInArea", "gData", function(x, reg="current", res.type=c("logical","integer","character"), buffer=0){

    ## preliminary stuff
    if(!is.gData(x)) stop("x is not a valid gGraph object")
    coords <- getCoords(x)

    res <- isInArea(x=coords, reg=reg, res.type=res.type, buffer=buffer)
    return(res)
}) # end isInArea for gData
