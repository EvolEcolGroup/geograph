###################
## plot for gGraph
###################
setMethod("plot", signature(x = "gGraph", y="missing"), function(x, y,shape="world", psize=NULL, pch=19, col=NULL,
                                      edges=FALSE, reset=FALSE, bg.col="gray", border.col="dark gray",
                                      lwd=1, useCosts=NULL, maxLwd=3, col.rules=NULL,...){
    ## some checks
    if(!is.gGraph(x)) stop("x is not a valid gGraph object")

    ## create the .geoGraphEnv if it does not exist
    if(!exists(".geoGraphEnv", envir=.GlobalEnv)) {
        assign(".geoGraphEnv",  new.env(parent=.GlobalEnv), envir=.GlobalEnv)
        warning(".geoGraphEnv was not present, which may indicate a problem in loading geoGraph.")
    }

    env <- get(".geoGraphEnv", envir=.GlobalEnv) # env is our target environnement

    coords <- getCoords(x)


    ## store original parameters to be passed to last.plot.param ##
    pch.ori <- pch
    col.ori <- col

    ## handle reset ##
    if(reset){
        assign("sticky.points",FALSE,envir=env)
        assign("last.points",expression(),envir=env)
    }

    ## handle xlim and ylim
    if((!exists("zoom.log", envir=env)) | reset) { # if xlim absent or if reset
        temp <- c(range(coords[,1]), range(coords[,2]))
        .zoomlog.up(temp)
    }

    zoomlog <- get("zoom.log", envir=env)
    zoomlog <- zoomlog[1,]

    xlim <- zoomlog[1:2]
    ylim <- zoomlog[3:4]

    ## handle zoom and psize
    if(is.null(psize)){
        psize <- get("psize", env=env)
    }

    ## handle color from attribute
    useAttrCol <- FALSE
    if(is.null(col.rules)){
        if(!is.null(x@meta$colors)){
            col.rules <- x@meta$colors
            useAttrCol <- TRUE
        }
    } else {
        useAttrCol <- TRUE
    }

    if(!is.null(col)) { # col overrides rules
        useAttrCol <- FALSE
    }


    toKeep <- isInArea(x, res.type="integer")
    coords <- coords[toKeep, ]


    ## store previous last.points in envir (is erased by plotEdges)
    if(exists("last.points", env=env)){
        last.points <- get("last.points", env=env)
    } else {
        last.points <- expression()
    }


    ## handle colors
    if(useAttrCol){
        col <- getColors(x, nodes=toKeep, attr.name=colnames(col.rules)[1], col.rules=col.rules)
    } else if(is.null(col.ori)){
        col <- "red"
    } else{
        col <- rep(col.ori, length=length(getNodes(x)))
        names(col) <- getNodes(x)
        col <- col[toKeep]
    }


    ## handle shape
    if(!is.null(shape) && is.character(shape) && shape=="world"){
        ## if(!require(sp)) stop("sp package needed to map the world")
        data(worldshape)
        shape <- worldshape
    }

    if(!is.null(shape)){ ## plot with background ##
        if(!inherits(shape,"SpatialPolygonsDataFrame"))
            stop("Shape must be a SpatialPolygonsDataFrame object \n(see readShapePoly in maptools to import such data from a GIS shapefile).")

        ## plot background
        plot(shape, col=bg.col, border=border.col, xlim=xlim, ylim=ylim)

        ## subset of points in area
        toKeep <- isInArea(x, reg="current", res.type="character")
        coords <- getCoords(x)[toKeep, ]

        ## define colors for these points
        if(useAttrCol){
            col <- getColors(x, nodes=toKeep, attr.name=colnames(col.rules)[1], col.rules=col.rules)
        } else if(is.null(col.ori)){
            col <- "red"
        } else{
            col <- rep(col.ori, length=length(getNodes(x)))
            names(col) <- getNodes(x)
            col <- col[toKeep]
        }


        if(edges){
            ## plotEdges(x, replot=FALSE, lwd=lwd, useCosts=useCosts, maxLwd=maxLwd)
            plotEdges(x, lwd=lwd, useCosts=useCosts, maxLwd=maxLwd)

        }
        points(coords, cex=psize, pch=pch, col=col, ...)

    } else{ ## plot only points ##
        plot(coords, xlab="longitude", ylab="latitude", xlim=xlim, ylim=ylim,
             cex=psize, pch=pch, col=col, ...)
        if(edges){
            ##       plotEdges(x, replot=TRUE, psize=psize, pch=pch, pcol=col, lwd=lwd,
            ##            useCosts=useCosts, maxLwd=maxLwd)
            plotEdges(x, psize=psize, pch=pch, pcol=col, lwd=lwd,
                      useCosts=useCosts, maxLwd=maxLwd)
        }
    }


    ## misc assignements in our dedicated environment
    assign("usr", par("usr"), envir=env)

    curCall <- sys.call(-1)
    assign("last.plot", curCall, envir=env)
    temp <- get("last.plot.param", envir=env)
    temp$psize <- psize
    temp$pch <- pch.ori
    temp$col <- col.ori
    assign("last.plot.param", temp, envir=env)

    ## must re-assign the last call to points in envir.
    assign("last.points", last.points, envir=env)

    ## add previously added points if needed ##
    sticky.points <- get("sticky.points", envir=env)
    if(sticky.points){
        temp <- get("last.points", envir=env) # this may be a list of calls
        invisible(lapply(temp, eval))
    }

    return(invisible())
}) # end plot method






#####################
## points for gGraph
#####################
setMethod("points", signature("gGraph"), function(x, psize=NULL, pch=NULL, col=NULL,
                                      edges=FALSE, lwd=1, useCosts=NULL, maxLwd=3, col.rules=NULL,
                                                  sticky.points=FALSE,...){
    ## some checks
    if(!is.gGraph(x)) stop("x is not a valid gGraph object")

    ## create the .geoGraphEnv if it does not exist
    if(!exists(".geoGraphEnv", envir=.GlobalEnv)) {
        assign(".geoGraphEnv",  new.env(parent=.GlobalEnv), envir=.GlobalEnv)
        warning(".geoGraphEnv was not present, which may indicate a problem in loading geoGraph.")
    }

    env <- get(".geoGraphEnv", envir=.GlobalEnv) # env is our target environnement

    zoomlog <- get("zoom.log", envir=env)
    zoomlog <- zoomlog[1,]

    xlim <- zoomlog[1:2]
    ylim <- zoomlog[3:4]

    ## store original parameters to be passed to last.plot.param ##
    pch.ori <- pch
    col.ori <- col


    ## subset data to visible area ##
    coords <- getCoords(x)
    toKeep <- isInArea(x, reg="current", res.type="integer")
    coords <- coords[toKeep, , drop=FALSE]

    ## handle plot param
    last.plot.param <- get("last.plot.param", envir=env)
    if(is.null(psize)) psize <- last.plot.param$psize
    if(is.null(pch)) pch <- last.plot.param$pch


    ## handle color from attribute
    useAttrCol <- FALSE
    if(is.null(col.rules)){
        if(!is.null(x@meta$colors)){
            col.rules <- x@meta$colors
            useAttrCol <- TRUE
        }
    } else {
        useAttrCol <- TRUE
    }

    if(!is.null(col)) { # col overrides rules
        useAttrCol <- FALSE
    }


    ## handle color
    if(useAttrCol){
        col <- getColors(x, nodes=toKeep, attr.name=colnames(col.rules)[1], col.rules=col.rules)
    } else if(is.null(col.ori)){
        col <- "red"
    } else{
        col <- rep(col.ori, length=length(getNodes(x)))
        names(col) <- getNodes(x)
        col <- col[toKeep]
    }


    ## define colors for these points
    if(useAttrCol){
        col <- getColors(x, nodes=toKeep, attr.name=colnames(col.rules)[1], col.rules=col.rules)
    } else if(is.null(col)){
        col <- "red"
    } else{
        col <- rep(col, length=length(getNodes(x)))
        names(col) <- getNodes(x)
        col <- col[toKeep]
    } # end handle color


    ## handle zoom and psize
    if(is.null(psize)){
        psize <- get("psize", env=env)
    }


    ## add only points and optionally edges
    if(edges){
        ## plotEdges(x, replot=FALSE, lwd=lwd, useCosts=useCosts, maxLwd=maxLwd)
        plotEdges(x, lwd=lwd, useCosts=useCosts, maxLwd=maxLwd)
    }
    points(coords, xlab="longitude", ylab="latitude", xlim=xlim, ylim=ylim,
           cex=psize, pch=pch, col=col, ...)


    ## if sticky points are used, store info in env ##
    if(sticky.points) {
        curCall <- sys.call(-1)
        temp <- get("last.points", envir=env) # might be a single expression or a list of expressions
        if(!is.list(temp)){
            temp <- list(temp) # make sure it is a list
        }
        ## do not add an existing expression ##
        existExp <- any(sapply(temp, identical, curCall))
        if(!existExp){
            temp[[length(temp)+1]] <- curCall
            assign("last.points", temp, envir=env)
        }
        assign("sticky.points", TRUE, envir=env)
    }

    return(invisible())
}) # end points method gGraph






############
## plotEdges
############
plotEdges <- function(x, useCosts=NULL, col="black", lwd=1,
                      lty=1, pch=NULL, psize=NULL, pcol=NULL, maxLwd=3, col.rules=NULL,
                      sticky.edges=FALSE,...){
    ## some checks
    if(!is.gGraph(x)) stop("x is not a valid gGraph object.")

    ## handle weights for edges
    if(is.null(useCosts)){
        useCosts <- hasCosts(x)
    }

    ## get the environment
    env <- get(".geoGraphEnv", envir=.GlobalEnv)


    if(exists("last.points", env=env)){
        last.points <- get("last.points", env=env)
    } else {
        last.points <- expression()
    }

    ## handle plot param # ! discarded: now call last points
    ## last.plot.param <- get("last.plot.param", envir=env)
    ## if(is.null(psize)) psize <- last.plot.param$psize
    ## if(is.null(pch)) pch <- last.plot.param$pch
    ## if(is.null(pcol)) pcol <- last.plot.param$col
    ## if(is.null(psize)){
    ##     psize <- get("psize", env=env)
    ## }

    ## retained coords (those within plotting area)
    coords <- getCoords(x)
    toKeep <- isInArea(x, reg="current", res.type="integer")
    keptCoords <- coords[toKeep, , drop=FALSE]

    ## adjust pcol to subset of points in area

     if(is.null(pcol)) {
         ## handle color from attribute
         useAttrCol <- FALSE
         if(is.null(col.rules)){
             if(!is.null(x@meta$colors)){
                 col.rules <- x@meta$colors
                 useAttrCol <- TRUE
             }
         } else {
             useAttrCol <- TRUE
         }

         if(!is.null(pcol)) { # pcol overrides color by attribute
             useAttrCol <- FALSE
             pcol <- pcol[toKeep]
         }

        if(useAttrCol){
            if(is.null(col.rules)){
                col.rules <- colnames(x@meta$colors)[1] # default attribute used for colors
            }

            pcol <- getColors(x, nodes=toKeep, attr.name=colnames(col.rules)[1], col.rules=col.rules)

        } else {
            pcol <- "black"
        } # end handle pcol
    }


    edges <- getEdges(x, res.type="matNames", unique=TRUE) # retrieve (unique) edges
    temp <- (edges[,1] %in% rownames(keptCoords)) & (edges[,2] %in% rownames(keptCoords))
    keptEdges <- edges[temp, ]

    if(nrow(keptEdges) < 1) {
        cat("\nNo edge to plot.\n")
        return(invisible())
    }

    ## handle costs
    if(useCosts){
        edges.w <- getCosts(x, res.type="vector", unique=TRUE)
        edges.w <- edges.w[temp]
        lwd <- edges.w / max(edges.w) # max lwd = 1
        lwd <- 1 - lwd # invert scale (to have thiner edges for larger costs)
        lwd <- lwd * maxLwd # max lwd = maxLwd
        lty <- rep(1, length(lwd)) # make a lty vector
        lty[lwd < 1e-5] <- 3 # assign 3 (doted line) to dead edges.
    }

    ## plot segments
    idx1 <- match(as.character(keptEdges[,1]), rownames(keptCoords))
    idx2 <- match(as.character(keptEdges[,2]), rownames(keptCoords))

    segments(keptCoords[idx1, 1], keptCoords[idx1, 2],
             keptCoords[idx2, 1], keptCoords[idx2, 2], col=col, lwd=lwd, lty=lty, ...)


    ## replot points
    ##points(keptCoords[,1], keptCoords[,2], pch=pch, cex=psize, col=pcol)
    eval(last.points)


    ## if sticky edges are used, store info in env ##
    if(sticky.edges) {
        ## curCall <- sys.call(-1) # does not work as plotEdges is not a S4 method
        curCall <- match.call()
        temp <- get("last.points", envir=env) # might be a single expression or a list of expressions
        if(!is.list(temp)){
            temp <- list(temp) # make sure it is a list
        }
         ## do not add an existing expression ##
        existExp <- any(sapply(temp, identical, curCall))
        if(!existExp){
            temp[[length(temp)+1]] <- curCall
            assign("last.points", temp, envir=env)
        }
        assign("sticky.points", TRUE, envir=env)
    }

    return(invisible())
} # end plotEdges






#####################
## points for gData
#####################
setMethod("points", signature(x = "gData"), function(x, type=c("nodes","original","both"),
                                                     pch.ori=4, pch.nodes=1,
                                                     col.ori="black", col.nodes="red",
                                                     sticky.points=TRUE,...){
    ## some checks
    if(!is.gData(x)) stop("x is not a valid gData object")
    type <- match.arg(type)

    ## get the environment
    env <- get(".geoGraphEnv", envir=.GlobalEnv)

    ## subset data to visible area ##
    coords.ori <- getCoords(x)
    if(type %in% c("nodes","both")){ # need to get coords of nodes
        if(!exists(x@gGraph.name, env=.GlobalEnv)){ # if the gGraph is missing, stop
            stop(paste("The gGraph object",x@gGraph.name,"is missing."))
        }

        if(length(x@nodes.id)==0) { # if nodes have not been assigned, stop
            stop("No nodes are assigned (@nodes.id empty); nothing to plot.")
        }

        myGraph <- get(x@gGraph.name, envir=.GlobalEnv)
        coords.nodes <- getCoords(myGraph)[x@nodes.id,, drop=FALSE]
        ## toKeep <- isInArea(coords.nodes, reg="usr", res.type="integer") # useless, messy
        ## coords.nodes <- coords.nodes[toKeep, , drop=FALSE]
    }

    ## restrain coords to current area ## # no need for this
    ## toKeep <- isInArea(coords.ori, reg="current", res.type="integer")
    ## coords.ori <- coords.ori[toKeep, , drop=FALSE]

    ## add points ##
    if(type=="original" | type=="both"){ # plot original coordinates
        points(coords.ori[,1], coords.ori[,2], pch=pch.ori, col=col.ori, ...)
    }

    if(type=="nodes" | type=="both"){ # plot assigned nodes
        points(coords.nodes[,1], coords.nodes[,2], pch=pch.nodes, col=col.nodes, ...)
    }

    if(type=="both"){ # add arrows from original location to assigned node
        arrows(coords.ori[,1], coords.ori[,2], coords.nodes[,1], coords.nodes[,2], angle=15, length=.1)
    }

     ## if sticky points are used, store info in env ##
    if(sticky.points){
        curCall <- sys.call(-1)
        temp <- get("last.points", envir=env) # might be a single expression or a list of expressions
        if(!is.list(temp)){
            temp <- list(temp) # make sure it is a list
        }
         ## do not add an existing expression ##
        existExp <- any(sapply(temp, identical, curCall))
        if(!existExp){
            temp[[length(temp)+1]] <- curCall
            assign("last.points", temp, envir=env)
        }
        assign("sticky.points", TRUE, envir=env)
    }

    return(invisible())
}) # end points for gData











#####################
## plot for gData
#####################
setMethod("plot", signature(x="gData", y="missing"), function(x, type=c("nodes","original","both"),
                                                 pch.ori=4, pch.nodes=1,
                                                 col.ori="black", col.nodes="red",
                                                 col.gGraph=NULL,
                                                 reset=FALSE, sticky.points=TRUE,...){
    ## some checks
    if(!is.gData(x)) stop("x is not a valid gData object")
    type <- match.arg(type)

    ## get the environment
    env <- get(".geoGraphEnv", envir=.GlobalEnv)

    if(!exists(x@gGraph.name, env=.GlobalEnv)){ # if the gGraph is missing, stop
            stop(paste("The gGraph object",x@gGraph.name,"is missing."))
        }

    myGraph <- get(x@gGraph.name, envir=.GlobalEnv) # get the gGraph object

    if((type %in% c("nodes","both")) & (length(x@nodes.id)==0)){ # no nodes assigned
        stop("Locations are not assigned to nodes (x@nodes.id is empty).")
    }


    ## cleaning if required ##
    if(reset){
        assign("sticky.points", FALSE, envir=env) # remove possible sticky points
        assign("last.points", expression(), envir=env) # remove possible sticky points
    }

    ## define visible area if reset ##
    if((!exists("zoom.log", envir=env)) | reset){
        loc <- getCoords(x)
        coords.nodes <- getCoords(myGraph)[x@nodes.id,, drop=FALSE]
        temp <- rbind(loc, coords.nodes)
        myRegion <- as.vector(apply(temp,2,range)) # return xmin, xmax, ymin, ymax
        .zoomlog.up(myRegion) # define new window limits
    }

    zoomlog <- get("zoom.log", envir=env)
    zoomlog <- zoomlog[1,]

    xlim <- zoomlog[1:2]
    ylim <- zoomlog[3:4]


    ## plot the gGraph object ##
    plot(myGraph, col=col.gGraph)


    ## call to points ##
    ## store previous last.points in envir (is erased by points)
    if(exists("last.points", env=env)){
        last.points <- get("last.points", env=env)
    } else {
        last.points <- expression()
    }

    points(x, type=type,
           pch.ori=pch.ori, pch.nodes=pch.nodes,col.ori=col.ori,
           col.nodes=col.nodes,sticky.points=sticky.points,...)


    ## some assignments
    curCall <- sys.call(-1)
    assign("last.plot", curCall, envir=env)
    ## must re-assign the last call to points in envir.
    assign("last.points", last.points, envir=env)

    ## add previously added points if needed ##
    sticky.points <- get("sticky.points", envir=env)
    if(sticky.points){
        temp <- get("last.points", envir=env) # this may be a list of calls
        invisible(lapply(temp, eval))
    }

    return(invisible())
}) # end plot method

