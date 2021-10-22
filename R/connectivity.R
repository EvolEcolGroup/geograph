#' Check connectivity of a gGraph object
#'
#' The functions \code{areNeighbours}, \code{areConnected} and the method
#' \code{isConnected} test connectivity in different ways.\cr
#'
#' - \code{areNeighbours}: tests connectivity between couples of nodes on an
#' object inheriting \code{graph} class (like a \linkS4class{graphNEL}
#' object).\cr
#'
#' - \code{areConnected}: tests if a set of nodes form a connected set on a
#' \linkS4class{gGraph} object.\cr
#'
#' - \code{isConnected}: tests if the nodes of a \linkS4class{gData} object
#' form a connected set. Note that this is a method for \linkS4class{gData},
#' the generic being defined in the \code{graph} package.\cr
#'
#' - \code{isReachable}: tests if one location (actually, the closest node to
#' it) is reachable from the set of nodes of a \linkS4class{gData} object.\cr
#'
#' - \code{connectivityPlot}: plots connected sets of a \linkS4class{gGraph} or
#' a \linkS4class{gData} object with different colors.\cr
#'
#' In \code{connectivityPlot}, isolated nodes (i.e. belonging to no connected
#' set of size > 1) are plotted in light grey.
#'
#' @aliases areNeighbours areConnected isConnected,gData-method isReachable
#' connectivityPlot connectivityPlot-methods connectivityPlot,gGraph-method
#' connectivityPlot,gData-method
#' @param V1 a vector of node names
#' @param V2 a vector of node names
#' @param graph a valid \linkS4class{graphNEL} object.
#' @param x a valid \linkS4class{gGraph} object.
#' @param nodes a vector of node names
#' @param object a valid \linkS4class{gData} object.
#' @param \dots other arguments passed to other methods.
#' @param loc location, specified as a list of two components giving
#' respectively the longitude and the latitude. Alternatively, it can be a
#' matrix-like object with one row and two columns.
#' @param seed an optional integer giving the seed to be used when randomizing
#' colors. One given seed will always give the same set of colors. NULL by
#' default, meaning colors are randomized each time a plot is drawn.
#' @param col.gGraph a character string or a number indicating the color of the
#' nodes to be used when plotting the \linkS4class{gGraph} object. Defaults to
#' '0', meaning that nodes are invisible.
#' @return - \code{areNeighbours}: a vector of logical, having one value for
#' each couple of nodes.\cr
#'
#' - \code{areConnected}: a single logical value, being TRUE if nodes form a
#' connected set.\cr
#'
#' - \code{isConnected}: a single logical value, being TRUE if nodes of the
#' object form a connected set.\cr
#' @author Thibaut Jombart (\email{t.jombart@@imperial.ac.uk})
#' @keywords utilities methods
#' @name connectivity
#' @examples
#'
#' connectivityPlot(rawgraph.10k)
#' connectivityPlot(worldgraph.10k)
#'
NULL




#################
## areNeighbours
#################
#' @rdname connectivity
#' @export

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
#' @rdname connectivity
#' @export
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
    ## !! use RBGL::connectedComp from RBGL rather than connComp from graph
    ## 100 times faster
    connected.sets <- RBGL::connectedComp(getGraph(x))

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
#' @rdname connectivity
#' @export
setMethod("isConnected", "gData", function(object, ...){
    ## checks ##
    x <- object
    if(!is.gData(x)) stop("'object' is not a valid gData object.")
    if(!exists(x@gGraph.name, envir=.GlobalEnv)) stop(paste("gGraph object",x@gGraph.name,"not found."))


    ## set args for areConnected ##
    myGraph <- get(x@gGraph.name, envir=.GlobalEnv)
    myNodes <- getNodes(x)

    ## wrapper ##
    res <- areConnected(myGraph, myNodes)

    ## return res ##
    return(res)
}) # end isConnected for gData






#################
## isReachable
#################
#' @rdname connectivity
#' @export
isReachable <- function(x, loc){ # x is a gData object
    ## checks ##
    if(!is.gData(x)) stop("x is not a valid gData object.")
    if(!exists(x@gGraph.name, envir=.GlobalEnv)) stop(paste("gGraph object",x@gGraph.name,"not found."))
    mygGraph <- get(x@gGraph.name, envir=.GlobalEnv)


    ## get connected sets ##
    connected.sets <- RBGL::connectedComp(getGraph(x))


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
#' @rdname connectivity
#' @export
setGeneric("connectivityPlot", function(x,...) {
    standardGeneric("connectivityPlot")
})



##################
## gGraph method
##################
#' @rdname connectivity
#' @export
setMethod("connectivityPlot", "gGraph", function(x, ..., seed=NULL){
    ## some checks ##
    if(!is.gGraph(x)) stop("x is not a valid gGraph object")

    ## create the .geoGraphEnv if it does not exist
    # am315 This should not be necessary, as .geoGraphEnv should always exist
    if(!exists(".geoGraphEnv", envir=.GlobalEnv)) {
        stop (".geoGraphEnv was not present, which may indicate a problem in loading geoGraph.")
    }
    # if(!exists(".geoGraphEnv", envir=.GlobalEnv)) {
    #     assign(".geoGraphEnv",  new.env(parent=.GlobalEnv), envir=.GlobalEnv)
    #     warning(".geoGraphEnv was not present, which may indicate a problem in loading geoGraph.")
    # }

    env <- get(".geoGraphEnv", envir=.GlobalEnv) # env is our target environnement


    ## get connected sets ##
    connected.sets <- RBGL::connectedComp(getGraph(x))

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

    colSets <- sample(grDevices::rainbow(nbSets))

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
#' @rdname connectivity
#' @export
setMethod("connectivityPlot", "gData", function(x, col.gGraph=0, ...,seed=NULL){
    ## some checks ##
    if(!is.gData(x)) stop("x is not a valid gData object")

    env <- get(".geoGraphEnv", envir=.GlobalEnv) # env is our target environnement

    ## get connected sets ##
    connected.sets <- RBGL::connectedComp(getGraph(x))

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
    colSets <- sample(grDevices::rainbow(nbRelSets))

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
