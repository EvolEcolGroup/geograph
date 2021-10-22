#################
## dropDeadEdges
#################


#' Get rid of some 'dead' edges or nodes
#' 
#' The functions \code{dropDeadEdges} and \code{dropDeadNodes} are used to
#' remove 'dead edges' and 'dead nodes'.\cr
#' 
#' Dead edges are edges associated to a prohibitive cost, that is, edges that
#' no longer imply connectivity between two nodes.\cr
#' 
#' Dead nodes are nodes that are not connected to any other node, thus not
#' having any role in the connectivity of a graph.\cr
#' 
#' 
#' @aliases dropDeadEdges dropDeadNodes
#' @param x a valid \linkS4class{gGraph}.
#' @param thres a numeric value indicating the threshold cost for an edge to be
#' removed. All costs strictly greater than \code{thres} will be removed.
#' @return A \linkS4class{gGraph} object.
#' @author Thibaut Jombart (\email{t.jombart@@imperial.ac.uk})
#' @keywords utilities methods
#' @examples
#' 
#' \dontrun{
#' plot(worldgraph.10k,reset=TRUE)
#' x <- dropDeadNodes(worldgraph.10k)
#' plot(x)
#' }
#' 
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
