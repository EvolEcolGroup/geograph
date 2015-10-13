#################
## areNeighbours
#################
areNeighbours <- function(V1, V2, graph){
    V1 <- as.character(V1)
    V2 <- as.character(V2)
    if(length(V1) != length(V2)) stop("V1 and V2 have different lengths.")

    edg <- edges(graph)

    ## function testing if two nodes are directly connected
    f1 <- function(A,B){
        return(any(edg[[A]]==B))
    }

    res <- mapply(function(x,y) f1(x,y), V1, V2)

    return(res)
} # end areNeighbours






################
## areConnected
################
areConnected <- function(x, nodes){ # x is a gGraph
    ## some checks ##
    ##if(!require(RBGL)) stop("RBGL package is required.") not needed
    if(!is.gGraph(x)) stop("x is not a valid gGraph object")
    if(!all(nodes %in% getNodes(x))) stop("Some specified nodes were not found in the gGraph object.")
    nodes <- unique(nodes)


    ## This is now pointless, function is already fast ##
    ##   ## first check that all our nodes are part of an edge ##
    ##     temp <- unique(as.vector(getEdges(x, res.type="matName")))
    ##     nodes.in.edges <- nodes %in% temp
    ##     if(!all(nodes.in.edges)) return(FALSE) # not a connected set if some nodes aren't connected at all


    ## get connected sets ##
    ## !! use connectedComp from RBGL rather than connComp from graph
    ## 100 times faster
    connected.sets <- connectedComp(getGraph(x))

    ## just keep sets > 1 node
    temp <- sapply(connected.sets, length)
    reOrd <- order(temp,decreasing=TRUE) # sets ordered in decreasing size
    temp <- temp[reOrd]
    if(min(temp)==1){
        connected.sets <- connected.sets[reOrd][1:(which.min(temp)-1)]
    }

    names(connected.sets) <- paste("set",1:length(connected.sets))

    res <- sapply(connected.sets, function(e) all(nodes %in% e))
    res <- any(res)

    return(res)
} # end areConnected






#########################
## isConnected for gData
#########################
## the GENERIC of this method is given in package 'graph'
setMethod("isConnected", "gData", function(object, ...){
    ## checks ##
    x <- object
    if(!is.gData(x)) stop("'object' is not a valid gData object.")
    if(!exists(x@gGraph.name, envir=.GlobalEnv)) stop(paste("gGraph object",x@gGraph.name,"not found."))


    ## set args for areConnected ##
    myGraph <- get(x@gGraph.name, env=.GlobalEnv)
    myNodes <- getNodes(x)

    ## wrapper ##
    res <- areConnected(myGraph, myNodes)

    ## return res ##
    return(res)
}) # end isConnected for gData






#################
## isReachable
#################
isReachable <- function(x, loc){ # x is a gData object
    ## checks ##
    if(!is.gData(x)) stop("x is not a valid gData object.")
    if(!exists(x@gGraph.name, envir=.GlobalEnv)) stop(paste("gGraph object",x@gGraph.name,"not found."))
    mygGraph <- get(x@gGraph.name, envir=.GlobalEnv)


    ## get connected sets ##
    connected.sets <- connectedComp(getGraph(x))


    ## just keep sets > 1 node
    temp <- sapply(connected.sets, length)
    reOrd <- order(temp,decreasing=TRUE) # sets ordered in decreasing size
    temp <- temp[reOrd]
    if(min(temp)==1){
        connected.sets <- connected.sets[reOrd][1:(which.min(temp)-1)]
    }

    names(connected.sets) <- paste("set",1:length(connected.sets))


    ## check which set contains refNode ##
    refNode <- closestNode(mygGraph,loc)
    temp <- sapply(connected.sets, function(e) refNode %in% e)
    if(!any(temp)) {
        warning("The reference node is not connected to any node.")
        return(FALSE)
    }
    refSet <- connected.sets[[which(temp)]]

    ## check reachability for each node ##
    myNodes <- getNodes(x)

    f1 <- function(oneNode){ # finds the set in which a node is
        temp <- sapply(connected.sets, function(e) oneNode %in% refSet)
        return(any(temp))
    }

    res <- sapply(myNodes, f1)
    names(res) <- myNodes

   ## return res ##
    return(res)
} # end isReachable






#####################
## connectivityPlot
#####################
setGeneric("connectivityPlot", function(x,...) {
    standardGeneric("connectivityPlot")
})



##################
## gGraph method
##################
setMethod("connectivityPlot", "gGraph", function(x, ..., seed=NULL){
    ## some checks ##
    if(!is.gGraph(x)) stop("x is not a valid gGraph object")

    ## create the .geographEnv if it does not exist
    if(!exists(".geographEnv", envir=.GlobalEnv)) {
        assign(".geographEnv",  new.env(parent=.GlobalEnv), envir=.GlobalEnv)
        warning(".geographEnv was not present, which may indicate a problem in loading geograph.")
    }

    env <- get(".geographEnv", envir=.GlobalEnv) # env is our target environnement


    ## get connected sets ##
    connected.sets <- connectedComp(getGraph(x))

    ## just keep sets > 1 node
    temp <- sapply(connected.sets, length)
    reOrd <- order(temp,decreasing=TRUE) # sets ordered in decreasing size
    temp <- temp[reOrd]
    if(min(temp)==1){
        connected.sets <- connected.sets[reOrd][1:(which.min(temp)-1)]
    }

    names(connected.sets) <- paste("set",1:length(connected.sets))


    ## define colors ##
    nbSets <- length(connected.sets)
    if(!is.null(seed) && is.numeric(seed)) {
        set.seed(seed)
    }

    colSets <- sample(rainbow(nbSets))

    myNodes <- getNodes(x)
    col <- rep("lightgray", length(myNodes))
    names(col) <- myNodes

    for(i in 1:nbSets){
        e <- connected.sets[[i]] # 'e' is a vector of connected nodes
        col[e] <- colSets[i]
    }


    ## call to plot ##
    plot(x, col=col, ...)

    ## save plot param ## (will be used by plot gGraph
    dots <- list(...)
    temp <- get("last.plot.param", envir=env)
    if(!is.null(dots$psize)) {
        temp$psize <- dots$psize
    }
    if(!is.null(dots$pch)){
        temp$pch <- dots$pch
    }
    temp$col <- col
    assign("last.plot.param", temp, envir=env)

    ## fix last call ##
    curCall <- sys.call(-1)
    assign("last.plot", curCall, envir=env)

    return(invisible(col))
}) # end connectivityPlot gGraph






#################
## gData method
#################
setMethod("connectivityPlot", "gData", function(x, col.gGraph=0, ...,seed=NULL){
    ## some checks ##
    if(!is.gData(x)) stop("x is not a valid gData object")

    env <- get(".geographEnv", envir=.GlobalEnv) # env is our target environnement

    ## get connected sets ##
    connected.sets <- connectedComp(getGraph(x))

    ## just keep sets > 1 node
    temp <- sapply(connected.sets, length)
    reOrd <- order(temp,decreasing=TRUE) # sets ordered in decreasing size
    temp <- temp[reOrd]
    if(min(temp)==1){
        connected.sets <- connected.sets[reOrd][1:(which.min(temp)-1)]
    }

    names(connected.sets) <- paste("set",1:length(connected.sets))


    ## define colors ##
    nbSets <- length(connected.sets)
    ## find the number of relevant sets
    nbRelSets <- 0
    myNodes <- getNodes(x)

    for(i in 1:nbSets){
        if(any(myNodes %in% connected.sets[[i]])){
            nbRelSets <- nbRelSets + 1
        }
    }

    if(!is.null(seed) && is.numeric(seed)) {
        set.seed(seed)
    }
    colSets <- sample(rainbow(nbRelSets))

    col <- rep("lightgray", length(myNodes))
    names(col) <- myNodes

    for(i in 1:nbSets){
        e <- connected.sets[[i]] # 'e' is a vector of connected nodes
        col[names(col) %in% e] <- colSets[i]
    }


    ## call to plot ##
    plot(x, col.ori=col, col.nodes=col, col.gGraph=col.gGraph, ...)


    ## fix last call ##
    curCall <- sys.call(-1)
    assign("last.plot", curCall, envir=env)

    return(invisible(col))
}) # end connectivityPlot gData
