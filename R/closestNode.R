###############
## closestNode
###############
setGeneric("closestNode", function(x,...) {
    standardGeneric("closestNode")
})






###############
## closestNode for gGraph
###############
setMethod("closestNode", "gGraph", function(x, loc, zoneSize=5, attr.name=NULL, attr.values=NULL){

    ## handle arguments
    if(!require(fields)) stop("package fields is required.")
    if(!is.gGraph(x)) stop("x is not a valid gGraph object.")
    loc <- as.data.frame(loc)
    if(ncol(loc) != 2) stop("coords does not have two columns.")
    coords <- getCoords(x)
    nodes <- getNodes(x)

    ## handle attribute specification if provided
    if(!is.null(attr.name)){
        temp <- unlist(getNodesAttr(x, attr.name=attr.name))
        temp <- as.character(temp)
        hasRightAttr <- temp %in% attr.values
        if(!any(hasRightAttr)) stop(paste("specified values of",attr.name,"never found."))
    } else{
        hasRightAttr <- TRUE
    }

    ## function finding the closest node for 1 loc ##
    closeOne <- function(oneLoc){
        ## define area around loc
        reg <- list()
        toKeep <- character(0) # will contain node names

        while(length(toKeep) < 3){ # enlarge zoneSize until at least 3 candidates appear
            ## define region
            reg$x <- oneLoc[1] + c(-zoneSize,zoneSize) # +- zoneZine in long
            reg$y <- oneLoc[2] + c(-zoneSize,zoneSize) # +- zoneZine in lat

            ## isolate nodes in this area
            toKeep <- isInArea(x, reg) # ! from now nodes indices won't match those of x and coords

            ## intersect with attribute selection
            toKeep <- toKeep & hasRightAttr

            ## toKeep must be a character to insure matching
            toKeep <- nodes[toKeep]

            ## increment zoneSize
            zoneSize <-  zoneSize*1.5
        } # end while

        xy <- coords[toKeep,,drop=FALSE]

        ## compute all great circle distances between nodes and loc
        temp <- rdist.earth(xy, matrix(oneLoc, nrow=1))
        closeNode <- rownames(temp)[which.min(temp)]
        return(closeNode)
    } # end closeOne


    ## apply closeOne to all requested locations
    res <- apply(loc, 1, closeOne) # these are node labels

    ## must not return indices, as this would not work for subsets of data
    ## e.g. closestPoint[x[getNodesAttr(x)=="land"]] will return wrong indices
    ## temp <- res
    ## res <- match(res, getNodes(x))
    ## names(res) <- temp

    return(res)
}) # end closestNode for gGraph






###############
## closestNode for gData
###############
setMethod("closestNode", "gData", function(x, zoneSize=5, attr.name=NULL, attr.values=NULL){

    ## get coords ##
    xy <- getCoords(x)

    ## get gGraph object ##
    if(!exists(x@gGraph.name, envir=.GlobalEnv)) stop(paste("gGraph object",x@gGraph.name,"does not exist."))
    obj <- get(x@gGraph.name, envir=.GlobalEnv)

    ## make a call to the gGraph method ##
    res <- closestNode(obj, loc=xy, zoneSize=zoneSize, attr.name=attr.name, attr.values=attr.values)

    ## return result ##
    x@nodes.id <- res

    return(x)
}) # end closestNode for gData
