###############
## setCosts
###############
setCosts <- function(x, attr.name=NULL, node.costs=NULL, method=c("mean", "product")){
    ## some checks + argument handling
    if(!is.gGraph(x)) stop("x is not a valid gGraph object")
    method <- match.arg(method)


    ## assign costs to vertices
    if(is.null(node.costs)){ # costs from a node attribute
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
    } else{ # cost directly provided
        if(!is.numeric(node.costs)) stop("Provided 'node.costs' not numeric.")
        node.costs <- rep(node.costs, length=length(getNodes(x))) # recycling node costs
        nodeCosts <- node.costs
        ## might add some more checks here...
    }


    ## find costs of edges as a function of terminating vertices
    EL <- getGraph(x)@edgeL

    ## method == mean ##
    if(method=="mean"){
        for(i in 1:length(EL)){
            EL[[i]]$weights <- (nodeCosts[i] + nodeCosts[EL[[i]]$edges]) / 2
            }
    }

    ## method == product ##
    if(method=="product"){
        for(i in 1:length(EL)){
            EL[[i]]$weights <- nodeCosts[i] * nodeCosts[EL[[i]]$edges]
            }
    }

    ## return result
    newGraph <- new("graphNEL", nodes=getNodes(x), edgeL=EL)
    res <- x
    res@graph <- newGraph

    return(res)
} # end setCosts
