###############
## setCosts
###############


#' Set friction in a gGraph object
#'
#' The function \code{setCosts} define costs for the edges of a
#' \linkS4class{gGraph} object according to a node attribute and some rules
#' defined in the \code{@meta\$costs} slot of the object. Each node has a value
#' for the chosen attribute, which is associated to a costs (a friction). The
#' cost of an edge is computed as a function (see argument \code{method}) of
#' the costs of its nodes.\cr
#'
#' Note that costs are inversely proportionnal to connectivity between edges:
#' the larger the cost associated to an edge, the lower the connectivity
#' between the two concerned nodes.\cr
#'
#' Also note that 'costs' defined in \code{geoGraph} are equivalent to
#' 'weights' as defined in \code{graph} and \code{RBGL} packages.
#'
#'
#' @param x a \linkS4class{gGraph} object with a least one node attribute, and
#' a \code{@meta$costs} component (for an example, see worldgraph.10k dataset).
#' @param attr.name the name of the node attribute used to compute costs (i.e.,
#' of one column of \code{@nodes.attr}).
#' @param node.costs a numeric vector giving costs associated to the nodes. If
#' provided, it will be used instead of \code{attr.name}.
#' @param method a character string indicating which method should be used to
#' compute edge cost from nodes costs. Currently available options are 'mean'
#' and 'prod', where the cost associated to an edge is respectively computed as
#' the mean, or as the product of the costs of its nodes.
#' @return A \linkS4class{gGraph} object with the newly defined costs used as
#' weightings of edges.
#' @author Thibaut Jombart (\email{t.jombart@@imperial.ac.uk})
#' @seealso \code{\link{dropDeadEdges}}, to get rid of edge whose cost is below
#' a given threshold. \code{\link{geo.add.edges}} to add edges to a
#' \linkS4class{gGraph} object.
#' @keywords utilities
#' @export
#' @examples
#'
#' plot(rawgraph.10k, reset=TRUE)
#'
#' ## zooming in
#' geo.zoomin(list(x=c(-6,38), y=c(35,73)))
#' title("Europe")
#'
#' ## defining a new object restrained to visible nodes
#' x <- rawgraph.10k[isInArea(rawgraph.10k)]
#'
#' ## define weights for edges
#' x <- setCosts(x, attr.name="habitat")
#' plot(x,edges=TRUE)
#' title("costs defined by habitat (land/land=1, other=100)")
#'
#'
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
