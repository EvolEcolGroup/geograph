#' @include classes.R
NULL


## ##############
## ## getHistory
## ##############
## setGeneric("getHistory", function(x,...) {
##     standardGeneric("getHistory")
## })


## setMethod("getHistory", "gGraph", function(x, ...) {
##     res <- x@history
##     return(res)
## })





##############
## getGraph
##############
setGeneric("getGraph", function(x,...) {
    standardGeneric("getGraph")
})


setMethod("getGraph", "gGraph", function(x, ...) {
    res <- x@graph
    return(res)
})


setMethod("getGraph", "gData", function(x, ...) {
    if(!exists(x@gGraph.name, envir=.GlobalEnv)) stop(paste("gGraph object",x@gGraph.name,"not found."))
    res <- getGraph(get(x@gGraph.name, envir=.GlobalEnv))
    return(res)
})




################
## getNodesAttr
################
#' Get nodes attributes from gGraph/gData object
#'
#' The function \code{getNodesAttr} returns the values of a set of variables
#' associated to the nodes (i.e. node attributes) of a \linkS4class{gGraph} or
#' \linkS4class{gData} object.
#'
#'
#' @aliases getNodesAttr getNodesAttr-methods getNodesAttr,gGraph-method
#' getNodesAttr,gData-method
#' @param x a valid \linkS4class{gGraph} or \linkS4class{gData} object.
#' @param nodes an optional integer, logical, or character string indicating
#' the subset of nodes to be used. If NULL, all nodes are used.
#' @param attr.name an optional character string indicating which node
#' attributes should be returned. If provided, it must match at least one of
#' the columns of \code{x@nodes.attr}.
#' @param \dots other arguments passed to other methods (currently unused).
#' @return A data.frame with the requested nodes attributes. Nodes are
#' displayed in rows, variables in columns.
#' @author Thibaut Jombart (\email{t.jombart@@imperial.ac.uk})
#' @seealso Most other accessors are documented in \linkS4class{gGraph} and
#' \linkS4class{gData} manpages.\cr
#' @keywords utilities methods
#' @examples
#'
#' ## gGraph method
#' head(getNodesAttr(worldgraph.40k))
#'
#'
#' ## gData method
#' getNodesAttr(hgdp)
#'
#'
#' @export
setGeneric("getNodesAttr", function(x,...) {
    standardGeneric("getNodesAttr")
})

#' @describeIn getNodesAttr Method for gGraph objects
#' @export
setMethod("getNodesAttr", "gGraph", function(x, nodes=NULL, attr.name=NULL,...) {
    if(is.null(nodes)){ # no node specified -> all nodes kept
        nodes <- TRUE
    }
    if(is.null(attr.name)){ # no attr specified -> all attr kept
        attr.name <- TRUE
    }

    res <- x@nodes.attr[nodes,attr.name, drop=FALSE]

    return(res)
})


#' @describeIn getNodesAttr Method for gData objects
#' @export
setMethod("getNodesAttr", "gData", function(x, attr.name=NULL,...) {
    if(is.null(attr.name)){ # no attr specified -> all attr kept
        attr.name <- TRUE
    }

    myNodes <- getNodes(x)
    if(!exists(x@gGraph.name, .GlobalEnv)) stop("gGraph object not found in global environment.")
    mygGraph <- get(x@gGraph.name, envir=.GlobalEnv)

    res <- getNodesAttr(mygGraph, nodes=myNodes, attr.name=attr.name)

    return(res)
})




#############
## getDates
#############
## setGeneric("getDates", function(x, ...) {
##     standardGeneric("getDates")
## })



## setMethod("getDates", "gGraphHistory", function(x, ...) {
##     res <- x@dates
##     res <- as.POSIXct(res)
##     return(res)
## })



## setMethod("getDates", "gGraph", function(x, ...) {
##     res <- getDates(getHistory(x))
##     return(res)
## })




#############
## getCoords
#############
#' @export
setGeneric("getCoords", function(x, ...) {
    standardGeneric("getCoords")
})


#' @export
setMethod("getCoords", "gGraph", function(x, ...) {
    res <- x@coords
    return(res)
})


#' @export
setMethod("getCoords", "gData", function(x, original=TRUE, ...) {
    if(original){ # original coords
        res <- x@coords
    } else {
        res <- getCoords(get(x@gGraph.name, envir=.GlobalEnv))[getNodes(x),,drop=FALSE] #
    }
    rownames(res) <- x@nodes.id
    return(res)
})



#############
## getNodes
#############
#' @export
setGeneric("getNodes", function(x, ...) {
    standardGeneric("getNodes")
})


#' @export
setMethod("getNodes", "gGraph", function(x, ...) {
    res <- rownames(x@coords)
    return(res)
})


#' @export
setMethod("getNodes", "gData", function(x, ...) {
    res <- x@nodes.id
    return(res)
})




#############
## getEdges
#############
#' Get edges from a gGraph object
#'
#' The function \code{getEdges} returns the edges of a \linkS4class{gGraph}
#' object using different possible outputs.
#'
#'
#' @aliases getEdges getEdges-methods getEdges,gGraph-method
#' @param x a valid \linkS4class{gGraph}.
#' @param res.type a character string indicating which kind of output should be
#' used. See value.
#' @param unique a logical indicating whether all returned edges should be
#' unique (TRUE) or if duplicated edges should be allowed (TRUE, default).
#' @param \dots other arguments passed to other methods (currently unused).
#' @return The output depends on the value of the argument \code{res.type}:\cr
#' - \code{asIs}: output is a named list of nodes, each slot containing nodes
#' forming an edge with one given node. This format is that of the \code{edges}
#' accessor for \linkS4class{graphNEL} objects.\cr
#'
#' - \code{matNames}: a matrix with two columns giving couples of node names
#' forming edges.\cr
#'
#' - \code{matId}: a matrix with two columns giving couples of node indices
#' forming edges.\cr
#' @author Thibaut Jombart (\email{t.jombart@@imperial.ac.uk})
#' @seealso Most other accessors are documented in \linkS4class{gGraph}
#' manpage.\cr
#'
#' See \code{\link{setEdges}} to add/remove edges, or
#' \code{\link{geo.add.edges}} and \code{\link{geo.remove.edges}} for
#' interactive versions.
#' @keywords utilities methods
#' @export
#' @examples
#'
#' example(gGraph)
#'
#' getEdges(x)
#' getEdges(x,res.type="matNames")
#' getEdges(x,res.type="matId")
#'
setGeneric("getEdges", function(x, ...) {
    standardGeneric("getEdges")
})


#' @export
#' @describeIn getEdges Method for gGraph objects
setMethod("getEdges", "gGraph", function(x, res.type=c("asIs","matNames", "matId"), unique=FALSE, ...) {
    res.type <- match.arg(res.type)
##    if(res.type=="asIs") return(x@graph@edgeL)
    if(res.type=="asIs") return(edges(x@graph))

    if(res.type=="matNames"){ # return matrix of node names
        res <- edges(x@graph)
        temp <- sapply(res, length)
        col1 <- rep(names(res), temp)
        ## col1 <- rep(1:length(res), temp)
        col2 <- unlist(res)
        res <- cbind(Vi=col1, Vj=col2)
    }

    if(res.type=="matId"){ # return matrix of node numbers
        res <- edgeL(x@graph)
        temp <- sapply(res, function(e) length(e$edges))
        col1 <- rep(1:length(res), temp)
        col2 <- unlist(res)
        res <- cbind(Vi=col1, Vj=col2)
    }

    if(unique){
            toKeep <- res[,1] < res[,2]
            res <- res[toKeep,, drop=FALSE]
        }

    rownames(res) <- NULL
    return(res)
})





#############
## setEdges
#############
#' Add and remove edges from a gGraph object
#'
#' The function \code{setEdges} allows one to add or remove edges in a
#' \linkS4class{gGraph} by directly specifying the relevant nodes, as a list or
#' a data.frame. This low-level function is called by \code{geo.add.edges} and
#' \code{geo.remove.edges}.
#'
#'
#' @aliases setEdges setEdges-methods setEdges,gGraph-method
#' @param x a valid \linkS4class{gGraph} object.
#' @param add a list or a dataframe containing node names of edges to be added.
#' The first element of the list (or column of the data.frame) gives starting
#' nodes of edges; the second gives ending nodes. Hence, the nodes of the i-th
#' edge are \code{add[[1]][i]} and \code{add[[2]][i]} if \code{add} is a list,
#' and \code{add[i,]} if \code{add} is a data.frame.
#' @param remove same as \code{add} argument, but edges are removed.
#' @param costs a numeric vector providing costs of the edges to be added.
#' \code{costs[i]} is the weight of the i-th edge.
#' @param \dots other arguments passed to other methods (currently unused).
#' @return A \linkS4class{gGraph} object with newly added or removed edges.
#' @author Thibaut Jombart (\email{t.jombart@@imperial.ac.uk})
#' @seealso \code{\link{geo.add.edges}} and \code{\link{geo.remove.edges}} to
#' interactively add or remove edges in a \linkS4class{gGraph} object. \cr
#'
#' \code{\link{getEdges}} to retrieve edges in different formats.
#' @keywords utilities methods
#' @export
setGeneric("setEdges", function(x, ...) {
    standardGeneric("setEdges")
})


#' @export
#' @describeIn setEdges Method for gGraph object
setMethod("setEdges", "gGraph", function(x, add=NULL, remove=NULL, costs=NULL, ...) {
    ## some checks
    if(is.null(add) & is.null(remove)) return(x)

    if(!is.null(add)){ ## add edges ##
        add <- as.data.frame(add)
        if(ncol(add) != 2) stop("add does not have two columns")
        from <- as.character(add[[1]])
        to <- as.character(add[[2]])
        if(!all(unique(c(from,to)) %in% getNodes(x))) stop("unknown specified nodes") # unknown nodes
        if(is.null(costs)){
            costs <- rep(1, length(from))
        }

        myGraph <- suppressWarnings(addEdge(from=from, to=to, graph=x@graph, weights=costs))

    } else { ## remove edges ##
        remove <- as.data.frame(remove)
        if(ncol(remove) != 2) stop("remove does not have two columns")
        from <- as.character(remove[[1]])
        to <- as.character(remove[[2]])
        if(!all(unique(c(from,to)) %in% getNodes(x))) stop("unknown specified nodes") # unknown nodes

        ## avoid attempts to removing non-existing edges
        temp <- areNeighbours(from, to, x@graph)
        myGraph <- removeEdge(from=from[temp], to=to[temp], graph=x@graph)
    }

    ##  subx <- deparse(substitute(x))
    res <- x
    res@graph <- myGraph

    ## remember this action
    curCall <- match.call()
    ##newHist <- new("gGraphHistory", res@history, cmd=curCall, comments="Modified edges using setEdges.")
    ##res@history <- newHist

    ## make assignement
    ## parEnv <- parent.frame()
    ## assign(subx, res, parEnv)

    return(res)
}) # end setEdges






#' Get costs associated to edges of a gGraph object
#'
#' The function \code{getCosts} returns the costs associated to the edges of a
#' \linkS4class{gGraph} object using different possible outputs. These outputs
#' are designed to match possible outputs of \code{\link{getEdges}} function.
#'
#' \code{getNodeCosts} returns the costs associated to nodes based on one node
#' attribute.
#'
#' The notion of 'costs' in the context of \linkS4class{gGraph} objects is
#' identical to the concept of 'weights' in \linkS4class{graph} (and thus
#' \linkS4class{graphNEL}) objects. The larger it is for an edge, the less
#' connectivity there is between the couple of concerned nodes.
#'
#' @aliases getCosts getCosts-methods getCosts,gGraph-method getNodeCosts
#' getNodeCosts-methods getNodeCosts,gGraph-method
#' @param x a valid \linkS4class{gGraph}.
#' @param res.type a character string indicating which kind of output should be
#' used. See value.
#' @param unique a logical indicating whether the costs should be returned for
#' unique edges (TRUE), or if duplicate edges should be considered as well
#' (TRUE, default).
#' @param attr.name the name of the node attribute used to define node costs.
#' @param \dots other arguments passed to other methods (currently unused).
#' @return The output depends on the value of the argument \code{res.type}:\cr
#' - \code{asIs}: output is a named list of weights, each slot containing
#' weights associated to the edges stemming from one given node. This format is
#' that of the \code{weights} accessor for \linkS4class{graphNEL} objects.\cr
#'
#' - \code{vector}: a vector of weights; this output matches matrix outputs of
#' \code{\link{getEdges}}.\cr
#' @author Thibaut Jombart (\email{t.jombart@@imperial.ac.uk})
#' @seealso Most other accessors are documented in \linkS4class{gGraph}
#' manpage.\cr
#' @keywords utilities methods
#' @examples
#'
#' head(getEdges(worldgraph.10k, res.type="matNames",unique=TRUE))
#' head(getCosts(worldgraph.10k,res.type="vector",unique=TRUE))
#'
#'
##############
## getCosts
##############
setGeneric("getCosts", function(x, ...) {
    standardGeneric("getCosts")
})


#' @describeIn getCosts Method for gGraph object
#' @export
setMethod("getCosts", "gGraph", function(x, res.type=c("asIs","vector"), unique=FALSE, ...) {
    res.type <- match.arg(res.type)
    if(res.type=="asIs") return(edgeWeights(x@graph))

    if(res.type=="vector"){ # return a matrix of node names
        res <- edgeWeights(x@graph)
        res <- unlist(res) # res is a vector of edge weights named as Ni.Nj
    }

    if(unique){
        nodeNames <- names(res)
        temp <- strsplit(nodeNames, "[.]")
        toKeep <- sapply(temp, function(v) v[1] < v[2])
        res <- res[toKeep]
    }

    return(res)
})





###############
## dropCosts
###############
setGeneric("dropCosts", function(x, ...) {
    standardGeneric("dropCosts")
})



setMethod("dropCosts", "gGraph", function(x) {
    myGraph <- getGraph(x)
    myGraph@edgeData@data <- list()
    x@graph <- myGraph

    return(x)
})




#############
## getData
#############
setGeneric("getData", function(x, ...) {
    standardGeneric("getData")
})



setMethod("getData", "gData", function(x, ...) {
    res <- x@data
    return(res)
})







#############
## getColors
#############
#' Get colors associated to edges of a gGraph object
#'
#' The function \code{getColors} returns the colors associated to the nodes of
#' a \linkS4class{gGraph} object, based on a specified node attribute.
#'
#' Colors are based on a node attribute, that is, on a column of the
#' \code{nodes.attr} data.frame. This attribute should have a finite number of
#' values, and would most likely be a factor. Correspondence between values of
#' this variable and colors must be provided in the \code{@meta\$color} slot,
#' or as \code{col.rules} argument. Color rules mus be provided as a two-column
#' matrix; the first column contains values of a node attribute, and is named
#' after this attribute; the second must be named "color", and contain valid
#' colors.
#'
#' See example section to know how this slot should be designed.
#'
#' @aliases getColors getColors-methods getColors,gGraph-method
#' @param x a valid \linkS4class{gGraph}.
#' @param nodes a vector of character strings or of integers identifying nodes
#' by their name or their index. Can be "all", in which case all nodes are
#' considered.
#' @param attr.name a character string indicating the name of node attribute to
#' be used to define colors.
#' @param col.rules a matrix giving the rules for plotting attribute values
#' with different colors. See details.
#' @param \dots other arguments passed to other methods.
#' @return A vector of characters being valid colors.\cr
#' @author Thibaut Jombart (\email{t.jombart@@imperial.ac.uk})
#' @keywords utilities methods
#' @export
#' @examples
#'
#' worldgraph.10k # there is a node attribute 'habitat'
#' worldgraph.10k@meta$color
#'
#' head(getNodes(worldgraph.10k))
#' head(getColors(worldgraph.10k,res.type="vector", attr.name="habitat"))
#'
#'
setGeneric("getColors", function(x, ...) {
    standardGeneric("getColors")
})

#' @export
#' @describeIn getColors Method for gGraph objects

setMethod("getColors", "gGraph", function(x, nodes="all", attr.name, col.rules=NULL, ...) {
    if(!attr.name %in% colnames(getNodesAttr(x))) {
        stop("Requested attribute not found in x@nodes.attr.")
    }

    if(is.null(col.rules)){
        if(is.null(x@meta$colors)){
            stop("No rule for color provided, and none defined in x (x@meta$colors is NULL).")
        } else {
            col.rules <- x@meta$colors
        }
    }

    if(is.null(ncol(col.rules)) || ncol(col.rules)!=2){
        stop("Color rules does not contain two columns.")
    }

    if(!attr.name %in% colnames(col.rules)){
        stop(paste("Nothing known about",attr.name,"in color rules."))
    }

    ## handle nodes ##
    if(length(nodes)==1 && nodes=="all"){
        toKeep <- TRUE
    } else if(is.numeric(nodes)){
        toKeep <- nodes
    }
    else if(is.character(nodes)){
        toKeep <- match(nodes, getNodes(x))
    } else{
        stop("Don't know what to do with 'nodes': wrong specification.")
    }

    ## define colors ##
    criterion <- getNodesAttr(x, nodes=toKeep, attr.name=attr.name) # seek criterion in nodes.attr
    col <- as.character(unlist(criterion))

    for(i in 1:nrow(col.rules)){
        col[col==col.rules[i,1]] <- col.rules[i,2]
    }

    names(col) <- getNodes(x)[toKeep]
    return(col)
}) # end getColors for gGraph








#################
## getNodeCosts
#################
#' @export
#' @describeIn getCosts Function to get the costs values for nodes
setGeneric("getNodeCosts", function(x, ...) {
    standardGeneric("getNodeCosts")
})


#' @describeIn getCosts Method to get node costs for gGraph object
#' @export
setMethod("getNodeCosts", "gGraph", function(x, attr.name, ...) {
    if(!is.gGraph(x)) stop("x is not a valid gGraph object")

    ## assign costs to vertices
    nodeAttr <- unlist(getNodesAttr(x, attr.name=attr.name))
    if(!is.null(x@meta$costs)){
        if(!any(attr.name %in% colnames(x@meta$costs))) {
            stop("attr.name is not documented in x@meta$costs.")
        }
        nodeCosts <- as.character(nodeAttr)
        rules <- x@meta$costs
        for(i in 1:nrow(x@meta$costs)){
            nodeCosts[nodeCosts==rules[i,attr.name]] <- rules[i,ncol(rules)]
        }
        nodeCosts <- as.numeric(nodeCosts)
    } else stop("x@meta does not contain a 'costs' component.")


    return(nodeCosts)
}) # end getNodeCosts

