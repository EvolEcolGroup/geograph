############
## generic
############
setGeneric("setDistCosts", function(x,...) {
    standardGeneric("setDistCosts")
})





#################
## gGraph method
#################
setMethod("setDistCosts", "gGraph", function(x, ...){

    ## some checks ##
    if(!require(fields)) stop("Package fields is required.")
    if(!is.gGraph(x)) stop("x is not a valid gGraph object")


    ## get edges and coords ##
    E <- getEdges(x, res.type="matNames")

    xy <- getCoords(x)
    xy1 <- xy[E[,1],]
    xy2 <- xy[E[,2],]


    ## get costs ##
    w <- sapply(1:nrow(E), function(i) rdist.earth(xy1[i,,drop=FALSE], xy2[i,,drop=FALSE])) # list of costs

    ## assign costs to the graphNEL ##
    edgeData(x@graph, from = E[,1], to = E[,2], attr = "weight") <- w

    return(x)
}) # end setDistCosts gGraph
