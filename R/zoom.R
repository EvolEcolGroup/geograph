###############
## .zoomlog.up
###############
.zoomlog.up <- function(vec){ # vec is xmin, xmax, ymin, ymax
    if(!is.vector(vec) || length(vec)!=4 || !is.numeric(vec)) stop("Updating zoomlog using a wrong value.")

    geoEnv <- get(".geographEnv", envir=.GlobalEnv)
    oldZoomLog <- get("zoom.log", env=geoEnv)
    newZoomLog <- rbind(vec, oldZoomLog)
    colnames(newZoomLog) <- colnames(oldZoomLog)

    if(nrow(newZoomLog) > 100){
        newZoomLog <- newZoomLog[1:100,]
    }
    assign("zoom.log", newZoomLog,env=geoEnv)


    return(invisible())
}





##############
## geo.zoomin
##############
geo.zoomin <- function(reg=NULL){ # reg should be a list as returned by locator()
    ## a few checks
    if(is.list(reg)){
        names(reg) <- c("x", "y")
    }

    if(is.numeric(reg) && length(reg)==4){ # is reg is a vector: x1, x2, y1, y2
        temp <- reg
        reg <- list(x=temp[1:2], y=temp[3:4])
    }

    ## get environment
    geoEnv <- get(".geographEnv", envir=.GlobalEnv)

    ## get last plot
    last.plot.call <- get("last.plot", envir=geoEnv)


    ## reg provided => no loop ##
    if(!is.null(reg)){

        ## define new xlim and ylim
        if(!is.list(reg) || length(reg)!=2) stop("Wrong reg specified.")
        reg <- lapply(reg, sort)

        ## make it a square
        reg.size <- max(diff(reg[[1]]), diff(reg[[2]])) # largest edge of rectangle
        reg.cen <- unlist(lapply(reg,mean)) # center of the rectangle
        reg[[1]][1] <- reg.cen[1] - reg.size/2 # new x1
        reg[[1]][2] <- reg.cen[1] + reg.size/2 # new x2
        reg[[2]][1] <- reg.cen[2] - reg.size/2 # new y1
        reg[[2]][2] <- reg.cen[2] + reg.size/2 # new y2

        .zoomlog.up(c(reg$x, reg$y))

        ## reconstruct a valid call to plot
        temp <- deparse(last.plot.call)
        temp <- sub("reset[^,]*,","",temp) # remove subset if provided
        temp <- sub(",[[:blank:]]*reset[^)]*", "",temp) # same thing, if last arg

        ##     temp <- sub("ylim[^,]*,","",temp) # idem, ylim
        ##     temp <- sub(")$","",temp) # idem, ylim
        ##     temp <- paste(temp, ", xlim = c(", reg$x[1], ",", reg$x[2],")")
        ##     temp <- paste(temp, ", ylim = c(", reg$y[1], ",", reg$y[2],")")
        ##     temp <- paste(temp, ")")

        newCall <- parse(text=temp)
        eval(newCall, env=.GlobalEnv)

    } else { ## reg not provided => looping ##

        reg <- data.frame(x=1:2,y=1:2)

        ## getting input from the user
        while(nrow(reg) > 1) {
            reg <- reg[integer(0),]
            reg <- data.frame(locator(2))

            if(nrow(reg) > 1) {
                ## define new xlim and ylim
                reg <- as.list(reg)
                reg <- lapply(reg, sort)

                .zoomlog.up(c(reg$x, reg$y))

                ## reconstruct a valid call to plot
                temp <- deparse(last.plot.call)
                temp <- sub("res..[^,]*,","",temp) # remove 'reset' if provided
                temp <- sub(",[[:blank:]]*res..[^)]*", "",temp) # same thing, if last arg

                newCall <- parse(text=temp)
                eval(newCall, env=.GlobalEnv)

                reg <- data.frame(reg)
            } # end if nrow(reg) > 1

        } # end while
    } # end else

    return(invisible())
} # end geo.zoomin





###############
## geo.zoomout
###############
geo.zoomout <- function(){
    ## get environment
    geoEnv <- get(".geographEnv", envir=.GlobalEnv)

    ## loop ##
    while(!is.null(locator(1))){
        ## get last plot
        last.plot.call <- get("last.plot", envir=geoEnv)

        ## get former coordinates and go one step back
        zoomLog <- get("zoom.log", env=geoEnv)
        if(nrow(zoomLog) < 2) {
            cat("\nNo previous zoom coordinates in zoom history.\n")
            return(invisible())
        }

        ## find center of the current frame
        size.x <- abs(diff(zoomLog[1,1:2]))
        size.y <- abs(diff(zoomLog[1,3:4]))

        newReg <- zoomLog[1,,drop=TRUE]
        newReg[1:2] <- newReg[1:2] + c(-size.x*0.5, size.x*0.5) # new region
        newReg[3:4] <- newReg[3:4] + c(-size.y*0.5, size.y*0.5) # new region

        ## make sure we are not going to far
        fullSize <- 0L
        if(newReg[1] < -180) {
            newReg[1] <- -180
            fullSize <- fullSize + 1L
        }
        if(newReg[2] > 180){
            newReg[2] <- 180
            fullSize <- fullSize + 1L
        }
        if(newReg[3] < -90){
            newReg[3] <- -90
            fullSize <- fullSize + 1L
        }
        if(newReg[4] > 90){
            newReg[4] <- 90
            fullSize <- fullSize + 1L
        }

        if(fullSize==4){
            cat("\nFull area already displayed.\n")
            return(invisible())
        }

        ## update zoom log
        .zoomlog.up(newReg)

        ## reconstruct a valid call to plot
        temp <- deparse(last.plot.call)

        newCall <- parse(text=temp)

        eval(newCall, env=.GlobalEnv)
    }

    return(invisible())
} # end geo.zoomout





############
## geo.back
############
geo.back <- function(){
    ## get environment
    geoEnv <- get(".geographEnv", envir=.GlobalEnv)

    ## loop ##
    while(!is.null(locator(1))){
        ## get last plot
        last.plot.call <- get("last.plot", envir=geoEnv)

        ## get former coordinates and go one step back
        zoomLog <- get("zoom.log", env=geoEnv)
        if(nrow(zoomLog) < 2) {
            cat("\nNo previous zoom coordinates in zoom history.\n")
            return(invisible())
        }

        zoomLog <- zoomLog[-1,,drop=FALSE]
        assign("zoom.log", zoomLog, env=geoEnv)

        ## reconstruct a valid call to plot
        temp <- deparse(last.plot.call)

        newCall <- parse(text=temp)

        eval(newCall, env=.GlobalEnv)
    }

    return(invisible())
} # end geo.back





#############
## geo.slide
#############
geo.slide <- function(){
    ## get environment
    geoEnv <- get(".geographEnv", envir=.GlobalEnv)

    ## loop ##
    while(!is.null(spoint <- locator(1))){
        ## get last plot
        last.plot.call <- get("last.plot", envir=geoEnv)

        ## get former coordinates and go one step back
        zoomLog <- get("zoom.log", env=geoEnv)

        ## find center of the current frame
        size.x <- abs(diff(zoomLog[1,1:2]))
        size.y <- abs(diff(zoomLog[1,3:4]))

        newReg <- zoomLog[1,,drop=TRUE]
        newReg[c(1,3)] <- c(spoint$x - size.x/2, spoint$y - size.y/2)
        newReg[c(2,4)] <- c(spoint$x + size.x/2, spoint$y + size.y/2)

        .zoomlog.up(newReg)

        ## reconstruct a valid call to plot
        temp <- deparse(last.plot.call)

        newCall <- parse(text=temp)

        eval(newCall, env=.GlobalEnv)
    }

    return(invisible())
} # end geo.slide






############
## geo.bookmark
############
geo.bookmark <- function(name=NULL){
    ## get environment
    geoEnv <- get(".geographEnv", envir=.GlobalEnv)

    if(is.null(name)){
        cat("\nAvailable bookmarks:\n")
        return(get("bookmarks", env=geoEnv))
    }


    ## get current zoom coords
    zoomLog <- get("zoom.log", env=geoEnv)
    new.book <- zoomLog[1,]

    ## update bookmarks
    bookmarks <- get("bookmarks", env=geoEnv)
    if(name %in% rownames(bookmarks)){ # erase previous bookmark if it exists
        bookmarks[name,] <-  new.book
        warning("This bookmark already existed; removing previous bookmark.")
    } else {
        onames <- rownames(bookmarks)
        bookmarks <- rbind(bookmarks,as.vector(new.book))
        rownames(bookmarks) <- c(onames, name)
        cat("\nBookmark '", name, " 'saved.\n")
    }

    assign("bookmarks", bookmarks, env=geoEnv)

    return(invisible())
} # end geo.bookmark





############
## geo.goto
############
geo.goto <- function(name){
    ## get environment
    geoEnv <- get(".geographEnv", envir=.GlobalEnv)

    ## get next zoom coords
    bookmarks <- get("bookmarks", env=geoEnv)
    zoomLog <- get("zoom.log", env=geoEnv)
    last.plot.call <- get("last.plot", envir=geoEnv)

    if(! name %in% rownames(bookmarks)) {
        cat("\nUnknown bookmark\n")
        return(geo.bookmark(NULL))
    }

    zoomLog <- rbind(as.vector(bookmarks[name, ]), zoomLog)
    assign("zoom.log", zoomLog, env=geoEnv)

    ## reconstruct a valid call to plot
    temp <- deparse(last.plot.call)
    newCall <- parse(text=temp)
    eval(newCall, env=.GlobalEnv)

    return(invisible())
} # end geo.goto
