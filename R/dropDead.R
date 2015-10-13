#################
## dropDeadEdges
#################
dropDeadEdges <- function(x, thres){ # x is a gGraph object
    if(!is.gGraph(x)) stop("x is not a valid gGraph object.")
    if(!hasCosts(x)) return(x)

    ## check weights under threshold
    myGraph <- getGraph(x)
    edgeW <- edgeWeights(myGraph)
    edgeL <- edgeL(myGraph)
    toKeep <- lapply(edgeW, function(v) v <= thres)

    newEdgeL <- list()
    for(i in 1:length(edgeL)){
        newEdgeL[[i]] <- list()
        newEdgeL[[i]]$edges <- edgeL[[i]]$edges[toKeep[[i]]]
        newEdgeL[[i]]$weights <- edgeW[[i]][toKeep[[i]]]
    }

    names(newEdgeL) <- nodes(myGraph) # items of the list must be named

    newGraph <- new("graphNEL", nodes=nodes(myGraph), edgeL=newEdgeL)
    res <- x
    res@graph <- newGraph

    return(res)
} # end dropDeadEdges






#################
## dropDeadNodes
#################
dropDeadNodes <- function(x){ # x is a gGraph object
    if(!is.gGraph(x)) stop("x is not a valid gGraph object.")

    ## get names of connected nodes
    nodes.in.edges <- unique(as.vector(getEdges(x,res.type="matNames")))

    ## get all nodes
    res <- x[nodes.in.edges]

    return(res)
} # end dropDeadNodes
