#########
## buffer
#########
setGeneric("buffer", function(x, ...) {
    standardGeneric("buffer")
})





################
## gGraph method
################
setMethod("buffer", "gGraph", function(x, nodes, d, res.type=c("nodes", "gGraph"), ...){
    ## CHECKS ##
    if(!is.gGraph(x)) stop("x is not a valid gGraph object")
    if(!is.numeric(d)) stop("d is not numeric")
    if(d > 1e4) warning("Buffer distance is greater than 10,000km; computations may be long.")
    res.type <- match.arg(res.type)

    ALL.NODES <- getNodes(x)
    if(!all(nodes %in% ALL.NODES)) stop("Some requested nodes do not exist in the gGraph grid.")

    GRAPH <- getGraph(x)
    EDGES <- edges(GRAPH)
    XY <- getCoords(x)


    ## FIND BUFFER FOR A NODE ##
    find.buf.onenode <- function(node, d){
        curNodes <- node
        res <- node
        visited.nodes <- node

        while(TRUE){
            neig <- unlist(EDGES[curNodes])
            neig <- setdiff(neig, visited.nodes)
            visited.nodes <- c(visited.nodes, neig)

            temp <- rdist.earth(XY[node,,drop=FALSE], XY[neig,,drop=FALSE], miles=FALSE, R=NULL)
            toKeep <- temp < d
            if(!any(toKeep)) break # exit
            curNodes <- neig[toKeep]
            res <- c(res, neig[toKeep])
        }
        return(res)
    }


    ## FIND BUFFER FOR ALL REQUESTED NODES ##
    res <- unlist(lapply(nodes,  find.buf.onenode, d))


    ## RETURN RESULTS ##
    res <- unique(res)
    if(res.type == "nodes") return(res) # if res.type is nodes


    #### DOES NOT WORK
    ## ISSUES WHEN DEPARSING THE GGRAPH
    ## if(res.type == "gData"){ # if res.type is gData
    ##     graphName <- gsub("\"","",deparse(x, back=FALSE))
    ##     return(graphName)
    ##     temp <- new("gData", coords=XY[res,,drop=FALSE], gGraph.name=graphName)
    ##     return(temp)
    ## }


    ## else ... (res.type==gGraph)
    bufAttr <- rep(FALSE, length(ALL.NODES))
    names(bufAttr) <- ALL.NODES
    bufAttr[res] <- TRUE

    ## set new attributes
    ALL.ATTR <- getNodesAttr(x)
    newATTR <- cbind.data.frame(ALL.ATTR, buffer=bufAttr)
    x@nodes.attr <- newATTR

    ## set new color rules
    x@meta$buf.colors <- data.frame(buffer=c(TRUE,FALSE), color=c("orange", "lightgrey"))
    return(x)
}) # end buffer for gGraph







################
## gGraph method
################
setMethod("buffer", "gData", function(x, d, res.type=c("nodes", "gData", "gGraph"), ...){
    ## CHECKS ##
    res.type <- match.arg(res.type)
    if(!is.gData(x)) stop("x is not a valid gData object")


    ## EXTRACT ARGUMENTS FOR FURTHER METHOD ##
    myNodes <- getNodes(x)
    myGraph <- get(x@gGraph.name, env=.GlobalEnv)


    ## CALL UPON gGraph METHOD ##
    if(res.type=="gGraph"){ # if result seeked is gGraph
        res <- buffer(myGraph, myNodes, d, res.type="gGraph")
        return(res)
    }

    # if result seeked is nodes or gData
    temp <- buffer(myGraph, myNodes, d, res.type="nodes")
    if(res.type=="nodes") return(temp) # if res.type is nodes

    ## else ... (res.type == gData)
    res <- new("gData", coords=getCoords(myGraph)[temp,,drop=FALSE], gGraph.name=x@gGraph.name)

    return(res)
}) # end buffer for gData
