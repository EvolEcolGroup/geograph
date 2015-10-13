#################
## geo.add.edges
#################
geo.add.edges <- function(x, mode=c("points","area","all"), refObj="rawgraph.40k") {
    ## preliminary stuff
    if(!is.gGraph(x)) stop("x is not a valid gGraph object")
    mode <- match.arg(mode)
    ## temp <- isInArea(x) # not needed
    ## coords <- getCoords(x)[temp,]
    ## nodes <- getNodes(x)[temp]
    coords <- getCoords(x)
    nodes <- getNodes(x)
    lon <- coords[,1]
    lat <- coords[,2]
    env <- get(".geoGraphEnv", envir=.GlobalEnv) # env is our target environnement

    ## handle refObj
    if(is.character(refObj) && refObj=="rawgraph.10k"){
        data(rawgraph.10k)
        refObj <- rawgraph.10k
    } else if(is.character(refObj) && refObj=="rawgraph.40k"){
        data(rawgraph.40k)
        refObj <- rawgraph.40k
    } else if(!is.gGraph(refObj)){
        stop("refObj is not a valid gGraph object.")
    }

    ## handle plot param
    last.plot.param <- get("last.plot.param", envir=env)
    psize <- last.plot.param$psize
    pch <- last.plot.param$pch

    ## initialize toAdd
    toAdd <- list(from=NULL, to=NULL)

    ## "points" mode ##
    if(mode=="points"){
        spoint <- 1:2
        ## getting input from the user
        while (length(spoint) > 1) {
            spoint <- NULL
            spoint <- identify(lon, lat, plot=FALSE, n=2)
            if(length(spoint) > 1) {
                segments(lon[spoint[1]], lat[spoint[1]], lon[spoint[2]], lat[spoint[2]], col="green")
                points(lon[spoint[1]],lat[spoint[1]],cex=psize, col="green", pch=pch)
                points(lon[spoint[2]],lat[spoint[2]],cex=psize, col="green", pch=pch)

                toAdd$from <- c(toAdd$from, nodes[spoint[1]])
                toAdd$to <- c(toAdd$to, nodes[spoint[2]])
            }
        }
    } # end mode "points"

    ## "area" mode ##
    if(mode=="area"){
        selArea <- data.frame(x=1:2,y=1:2)

        ## getting input from the user
        while(nrow(selArea) > 1) {
            ##  selArea <- selArea[integer(0),]  not needed
            selArea <- data.frame(locator(2))

            if(nrow(selArea) > 1) {
                selNodes <- isInArea(refObj, reg=selArea, res.type="integer") # indices of selected points
                selEdges <- getEdges(refObj, res.type="matId", unique=TRUE) # edges, nodes=numerical indices
                temp <- (selEdges[,1] %in% selNodes) & (selEdges[,2] %in% selNodes)
                selEdges <- selEdges[temp,] # edges of refobj wholly inside the selected area

                segments(lon[selEdges[,1]], lat[selEdges[,1]], lon[selEdges[,2]], lat[selEdges[,2]], col="red")
                points(lon[selNodes], lat[selNodes], cex=psize, col="green", pch=pch)

                toAdd$from <- c(toAdd$from, getNodes(refObj)[selEdges[,1]])
                toAdd$to <- c(toAdd$to, getNodes(refObj)[selEdges[,2]])
            }
        } # end while
    } # end mode "area"

    ## "all" mode ##
    if(mode=="all"){
        x@graph <- getGraph(refObj)
        return(x)
    }

    ## make sure added edges are unique
    toAdd <- as.matrix(as.data.frame(toAdd))
    toAdd <- t(apply(toAdd,1,sort)) # sorting
    toAdd <- paste(toAdd[,1], toAdd[,2], sep="-") # making strings
    toAdd <- unique(toAdd) # keep unique strings
    toAdd <- strsplit(toAdd, "-")
    from <- sapply(toAdd, function(e) e[1])
    to <- sapply(toAdd, function(e) e[2])

    ## call to setEdges
    res <- setEdges(x=x, add=cbind(from, to) )

    return(res)
} # end geo.add.edges






####################
## geo.remove.edges
####################
geo.remove.edges <- function(x, mode=c("points","area")) {
    ## preliminary stuff
    if(!is.gGraph(x)) stop("x is not a valid gGraph object")
    temp <- isInArea(x)
    # coords <- getCoords(x)[temp,] # not needed: can work with whole object
    coords <- getCoords(x)
    nodeNames <- getNodes(x)
    lon <- coords[,1]
    lat <- coords[,2]
    env <- get(".geoGraphEnv", envir=.GlobalEnv) # env is our target environnement
    psize <- get("psize", env=env)
    mode <- match.arg(mode)

    ## handle plot param
    last.plot.param <- get("last.plot.param", envir=env)
    psize <- last.plot.param$psize
    pch <- last.plot.param$pch

    ## initialize toRemove
    toRemove <- list(from=NULL, to=NULL)


    ## mode: points ##

    if(mode=="points"){
        spoint <- 1:2
        ## getting input from the user
        while(length(spoint) > 1) {
            spoint <- NULL
            spoint <- identify(lon, lat, plot=FALSE, n=2)
            if(length(spoint) > 1) {
                segments(lon[spoint[1]], lat[spoint[1]], lon[spoint[2]], lat[spoint[2]], col="red")
                points(lon[spoint[1]], lat[spoint[1]], cex=psize, col="red", pch=pch)
                points(lon[spoint[2]], lat[spoint[2]], cex=psize, col="red", pch=pch)

                toRemove$from <- c(toRemove$from, nodeNames[spoint[1]])
                toRemove$to <- c(toRemove$to, nodeNames[spoint[2]])
            }
        }
    } # end mode: points


    ## mode: area ##

    if(mode=="area"){
        selArea <- data.frame(x=1:2,y=1:2)

        ## getting input from the user
        while(nrow(selArea) > 1) {
            ##  selArea <- selArea[integer(0),]  not needed
            selArea <- data.frame(locator(2))

            if(nrow(selArea) > 1) {
                selIdx <- which(isInArea(x, reg=selArea)) # indices of selected points
                selEdges <- getEdges(x, res.type="matId", unique=TRUE) # edges, nodes=numerical indices
                temp <- (selEdges[,1] %in% selIdx) & (selEdges[,2] %in% selIdx)
                selEdges <- selEdges[temp,] # edges wholly inside the selected area

                segments(lon[selEdges[,1]], lat[selEdges[,1]], lon[selEdges[,2]], lat[selEdges[,2]], col="red")
                points(lon[selIdx], lat[selIdx], cex=psize*1.5, col="red")

                toRemove$from <- c(toRemove$from, nodeNames[selEdges[,1]])
                toRemove$to <- c(toRemove$to, nodeNames[selEdges[,2]])
            }
        }

    } # end mode: area


    ## handle toRemove ##
    ## make sure removed edges are unique
    toRemove <- as.matrix(as.data.frame(toRemove))
    toRemove <- t(apply(toRemove,1,sort)) # sorting
    toRemove <- paste(toRemove[,1], toRemove[,2], sep="-") # making strings
    toRemove <- unique(toRemove) # keep unique strings
    toRemove <- strsplit(toRemove, "-")
    from <- sapply(toRemove, function(e) e[1])
    to <- sapply(toRemove, function(e) e[2])

    ## call to setEdges
    res <- setEdges(x=x, remove=cbind(from, to) )

    return(res)
} # end geo.remove.edges






###################
## geo.change.attr
###################
geo.change.attr <- function(x, mode=c("points","area"), attr.name, attr.value,
                            only.name=NULL, only.value=NULL, newCol="black",
                            restore.edges=FALSE, refObj="rawgraph.40k") {

    ## preliminary stuff ##
    if(!is.gGraph(x)) stop("x is not a valid gGraph object")

    ## handle "only" ##
    if(!is.null(only.name)){
        temp <- unlist(getNodesAttr(x, attr.name=only.name))
        temp <- as.character(unlist(temp))
        hasRightAttr <- which(temp==only.value)
        if(length(hasRightAttr)==0) stop(paste("specified values of",only.name,"never found."))
    } else{
        hasRightAttr <- 1:nrow(getCoords(x))
    }

    ## handle refObj ##
    if(restore.edges){
        if(is.character(refObj) && refObj=="rawgraph.10k"){
            data(rawgraph.10k)
            refObj <- rawgraph.10k
        } else if(is.character(refObj) && refObj=="rawgraph.40k"){
            data(rawgraph.40k)
            refObj <- rawgraph.40k
        } else if(!is.gGraph(refObj)){
            stop("refObj is not a valid gGraph object.")
        }
    } # end handle refObj


    coords <- getCoords(x)
    lon <- coords[,1]
    lat <- coords[,2]
    env <- get(".geoGraphEnv", envir=.GlobalEnv) # env is our target environnement
    mode <- match.arg(mode)
    if(!attr.name %in% colnames(x@nodes.attr)) stop("specified node attribute name not found")

    ## set replacement colors
    if( (!is.null(x@meta$colors)) && (attr.name %in% colnames(x@meta$colors)) ){
        temp <- which(attr.value == x@meta$colors[,attr.name])[1]
        if(!is.na(temp)){ # attr.value is documented in @meta$colors
            newCol <- x@meta$colors[temp,2]
        } else{ # if attr.value is not documented, we document it in @meta$colors
            if(is.factor(x@meta$colors[,attr.name])){ # if attr is a factor
                x@meta$colors[,attr.name] <- as.character(x@meta$colors[,attr.name]) # convert as character
                x@meta$colors <- rbind.data.frame(x@meta$colors, c(attr.value,newCol))
                x@meta$colors[,attr.name] <- factor(x@meta$colors[,attr.name]) # restore factor type
            } else { # attr is not a factor
                x@meta$colors <- rbind.data.frame(x@meta$colors, c(attr.value,newCol))
            }
        }
    } # end setting replacement colors


    ## handle plot param
    last.plot.param <- get("last.plot.param", envir=env)
    psize <- last.plot.param$psize
    pch <- last.plot.param$pch

    ## initialize toChange
    toChange <- integer(0)


    ## mode: points ##

    if(mode=="points"){
        spoint <- 0
        ## getting input from the user
        while(length(spoint) > 0) {
            spoint <- NULL
            spoint <- identify(lon, lat, plot=FALSE, n=1)
            if(length(spoint) > 0) {
                spoint <- spoint[spoint %in% hasRightAttr] # only nodes with a given attributes will be modified
                points(lon[spoint], lat[spoint], cex=psize, pch=pch, col=newCol)

                toChange <- c(toChange, spoint)

            }
        }
    } # end mode: points

    if(mode=="area"){
        selArea <- data.frame(x=1:2,y=1:2)

        ## getting input from the user
        while(nrow(selArea) > 1) {
            selArea <- selArea[integer(0),]
            selArea <- data.frame(locator(2))

            if(nrow(selArea) > 1) {
                selIdx <- which(isInArea(x, reg=selArea)) # indices of selected points
                selIdx <- selIdx[selIdx %in% hasRightAttr] # only nodes with replaced attribute
                points(lon[selIdx], lat[selIdx], cex=psize, pch=pch, col=newCol)

                toChange <- c(toChange, selIdx)
            }
        }

    } # end mode: area


    ## make changes ##
    toChange <- unique(toChange) # unique id
    res <- x

    if(is.factor(res@nodes.attr[,attr.name])){ # special handling if attr is a factor
        temp <- as.character(res@nodes.attr[, attr.name])
        temp[toChange] <- attr.value
        res@nodes.attr[,attr.name] <- factor(temp)
    } else { # in other cases...
        res@nodes.attr[toChange,attr.name] <- attr.value
    }

    ## re-add some edges if restore.edges is TRUE ##
    if(restore.edges){
        nodeLab <- getNodes(res)[toChange] # label of changed nodes
        temp <- adj(getGraph(refObj), nodeLab)
        toAdd1 <- rep(names(temp),sapply(temp,length))
        toAdd2 <- unlist(temp)
        toAdd <- list(toAdd1,toAdd2)
        res <- setEdges(res, add=toAdd)
    }
    ## need to save the call here ! ##

    return(res)
} # end geo.change.attr


