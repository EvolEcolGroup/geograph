#################
## BASIC METHODS
#################

###################
## [ gGraphHistory
###################
## setMethod("[", "gGraphHistory", function(x, i, j = "missing", drop = "missing") {
##     if(missing(i)) i <- TRUE

##     res <- x
##     res@cmd <- res@cmd[i]
##     res@dates <- res@dates[i]
##     res@comments <- res@comments[i]

##     return(res)
## })






############
## [ gGraph
############
setMethod("[", "gGraph", function(x, i, j, ..., drop=TRUE) {
    if(missing(i)) {
        i <- TRUE
    }
    if(is.logical(i)){
        i <- rep(i, length=nrow(getCoords(x)))
    }
    if(is.character(i)) {
        i <- match(i, getNodes(x))
        if(any(is.na(i))) stop("Some specified node labels were not found.")
    }
    if(missing(j)) {
        j <- TRUE
    }

    if(is.logical(i) && is.logical(j) && all(c(i,j))) return(x) # don't loose time for silly trials

    argList <- list(...)
    if(is.null(argList$useSubGraph)){
        useSubGraph <- TRUE
    } else {
        useSubGraph <- argList$useSubGraph
    }

    oldNodeNames <- getNodes(x) # node names before subsetting
    newNodeNames <- oldNodeNames[i] # node names after subsetting


    ## do the subsetting ##
    res <- x
    res@coords <- res@coords[i, , drop=FALSE]
    if(nrow(res@nodes.attr)>0){
        res@nodes.attr <- res@nodes.attr[i, j, drop=FALSE]
    }
    ##if(useSubGraph){ # use procedure from graph package to subset graph (slow) #

    myGraph <- subGraph(nodes(res@graph)[i], res@graph)

    ##} else ## { # use a customized procedure (faster) #
    ##         myGraph <- getGraph(res)
    ##         myGraph@nodes <- myGraph@nodes[i]
    ##         myGraph@edgeL <- myGraph@edgeL[myGraph@nodes]
    ##         ## special handling of i, to know which indexes are kept
    ##         if(is.character(i)){ # type == character
    ##             keptIdx <- match(i, nodeNames)
    ##             keptIdx <- !is.na(keptIdx)
    ##         }
    ##         if(is.logical(i)){ # type == logical
    ##             keptIdx <- which(i)
    ##         }
    ##         if(is.numeric(i)){ # type == numeric
    ##             if(i[1]>0) {
    ##                 keptIdx <- i
    ##             } else{
    ##                 keptIdx <- setdiff(1:nrow(x@coords), i)
    ##             }
    ##         }

    ##         f1.noweights <- function(nodeIdc){ # function to subset graph without weights
    ##             nodeIdc$edges <- nodeIdc$edges[nodeIdc$edges %in% keptIdx] # erase non kept indices
    ##             nodeIdc$edges <- match(oldNodeNames[nodeIdc$edges], newNodeNames) # match indices with new positions
    ##             return(nodeIdc)
    ##         }
    ##         f1.withweights <- function(oneNode){ # function to subset graph with weights
    ##             temp <- oneNode$edges %in% keptIdx
    ##             oneNode$edges <- oneNode$edges[temp]
    ##             oneNode$weights <- oneNode$weights[temp]
    ##             return(oneNode)
    ##         }

    ##         if(is.null(myGraph@edgeL[[1]]$weights)){
    ##             myGraph@edgeL <- lapply(myGraph@edgeL, f1.noweights)
    ##         } else {
    ##             myGraph@edgeL <- lapply(myGraph@edgeL, f1.withweights)
    ##         }
    ##     }
                                        # end subset graph

    res@graph <- myGraph

    ## remember this subsetting
    curCall <- match.call()
    ## newHist <- new("gGraphHistory", res@history, cmd=curCall, comments="Subsetting using [...]")
    ## res@history <- newHist

    return(res)
})






###########
## [ gData
###########
setMethod("[", "gData", function(x, i, j, ..., drop=FALSE) {
    if(missing(i)) {
        i <- TRUE
    }
    if(is.logical(i)){
        i <- rep(i, length=nrow(getCoords(x)))
    }
    if(is.character(i)) {
        i <- match(i, getNodes(x))
        if(any(is.na(i))) stop("Some specified node labels were not found.")
    }
    if(missing(j)) {
        j <- TRUE
    }

    if(is.logical(i) && is.logical(j) && all(c(i,j))) return(x) # don't loose time for silly trials


    ## do the subsetting ##

    ## coords
    res <- x
    N <- nrow(res@coords)
    res@coords <- res@coords[i, , drop=FALSE]

    ## nodes id
    res@nodes.id <- res@nodes.id[i]

    ## data
    if(!is.null(getData(x))){
        if(nrow(getData(x))==N){
            res@data <- res@data[i, j, drop=FALSE]
        } else if(length(getData)==N){
            res@data <- res@data[i]
        } else if(existsMethod("[",class(res@data)[1])){
            res@data <- res@data[i,j, ..., drop=drop]
        } else{
            warning("Don't know what to do with @data.")
        }
    }

    return(res)
})






################
## SHOW METHODS
################

######################
## show gGraphHistory
######################
## setMethod("show", "gGraphHistory", function(object){
##     x <- object
##     N <- length(x@cmd)

##     ## printing
##     ## cat("\n=== gGgraphHistory ===\n")
##     if(N > 0){
##         for(i in 1:N){
##             cat("=",i, "=\n")
##             cat("Date:", x@dates[i], "\n")
##             cat("Comment:", x@comments[i], "\n")
##             cat("Command: ")
##             print(x@cmd[[i]])
##             cat("\n")
##         }
##     } else{
##         cat("\t- empty object -\n")
##     }

## }) # end show gGraphHistory






###############
## show gGraph
###############
setMethod("show", "gGraph", function(object){
    x <- object
    N <- nrow(x@coords)
    nDisp <- 3

    ## printing
    cat("\n=== gGraph object ===\n")
    cat("\n@coords: spatial coordinates of",nrow(x@coords),"nodes\n")
    print(head(x@coords, nDisp))
    if(N > nDisp) cat("...\n")

    cat("\n@nodes.attr:",ncol(x@nodes.attr),"nodes attributes\n")
    print(head(x@nodes.attr, nDisp))
    if(nrow(x@nodes.attr) > nDisp) cat("...\n")

    cat("\n@meta: list of meta information with", length(x@meta),"items\n")
    if(length(x@meta)>0) print(paste("$", names(x@meta), sep=""))

    cat("\n@graph:\n")
    print(x@graph)

    ##cat("\n@history: (", length(x@history@cmd)," items )\n")
    ##print(x@history[1:min(nDisp,length(x@history@cmd))])
    ##if(length(x@history@cmd) > nDisp) cat("\n...\n")

}) # end show gGraph






###############
## show gData
###############
setMethod("show", "gData", function(object){
    x <- object
    N <- nrow(x@coords)
    nDisp <- 3

    ## printing
    cat("\n=== gData object ===\n")
    cat("\n@coords: spatial coordinates of",nrow(x@coords),"nodes\n")
    print(head(x@coords, nDisp))
    if(N > nDisp) cat("...\n")

    cat("\n@nodes.id:",nrow(x@nodes.id),"nodes identifiers\n")
    print(head(x@nodes.id, nDisp))
    if(length(x@nodes.id) > nDisp) cat("...\n")

    cat("\n@data:",nrow(x@data),"data\n")
    print(head(x@data, nDisp))
    if(N > nDisp) cat("...\n")

    ##cat("\nAssociated gGraph:",x@gGraph.name, "[",x@gGraph.version,"]\n")
    cat("\nAssociated gGraph:",x@gGraph.name,"\n")

}) # end show gData

