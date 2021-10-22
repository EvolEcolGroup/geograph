###############################################################
###############################################################
## CLASSES DEFINITION FOR THE GEOGRAPH PACKAGE
###############################################################
###############################################################




######################
## CLASSES DEFINITION
######################

## setClass("gGraphHistory", representation(cmd = "list", dates = "character", comments = "character"))



#' @export
setClass("gGraph",
         representation(coords = "matrix", nodes.attr = "data.frame", meta = "list",
                        graph = "graphNEL"),
         prototype(coords = matrix(numeric(0), ncol=2, dimnames=list(NULL, c("lon","lat"))),
                   nodes.attr = data.frame(),
                   meta = list(),
                   graph = new("graphNEL"))
         )


#' Formal class "gData"
#' 
#' The class \code{gData} is a formal (S4) class storing georeferenced data,
#' consisting in a set of locations (longitude and latitude) where one or
#' several variables have been measured. These data are designed to be matched
#' against a \linkS4class{gGraph} object, each location being assigned to the
#' closest node of the \linkS4class{gGraph} object.\cr
#' 
#' Note that for several operations on a \code{gData} object, the
#' \linkS4class{gGraph} object to which it is linked will have to be present in
#' the same environment.
#' 
#' 
#' @name gData-class
#' @aliases gData gData-class [,gData-method [,gData,ANY,ANY-method
#' [,gData,ANY,ANY,ANY-method getCoords,gData-method getData-methods
#' getData,gData-method getData getNodes,gData-method initialize,gData-method
#' show,gData-method is.gData getGraph,gData-method
#' @docType class
#' @section Objects from the class gData: \code{gData} objects can be created
#' by calls to \code{new("gData", ...)}, where '...' can be the following
#' arguments:
#' 
#' \describe{ \item{list("coords")}{a matrix of spatial coordinates with two
#' columns, being respectively longitude (from -180 to 180) and latitude.
#' Positive numbers are intended as 'east' and 'north', respectively.}
#' \item{list("nodes.id")}{a vector of character strings giving the name of the
#' nodes (of the \linkS4class{gGraph} object) associated to the locations.}
#' \item{list("data")}{any kind of data associated to the locations in coords.
#' For matrix-like objects, rows should correspond to locations.}
#' \item{list("gGraph.name")}{a character string the name of the
#' \linkS4class{gGraph} object against which the object is matched.} } Note
#' that none of these is mandatory: \code{new("gData")} would work, and create
#' an empty \code{gGraph} object. Also note that a finer matching of locations
#' against the nodes of a \code{gGraph} object can be achieved after creating
#' the object, for instance using the \code{closestNode} method.
#' @author Thibaut Jombart (\email{t.jombart@@imperial.ac.uk})
#' @seealso Related class:\cr - \code{\linkS4class{gGraph}}\cr
#' @keywords classes spatial
#' @examples
#' 
#' hgdp
#' 
#' ## plot data
#' plot(worldgraph.40k, pch="")
#' points(hgdp)
#' 
#' ## subset and plot data
#' onlyNorth <- hgdp[hgdp@data$Latitude >0] # only northern populations
#' 
#' plot(worldgraph.40k, reset=TRUE)
#' abline(h=0) # equator
#' points(onlyNorth, pch.node=20, cex=2, col.node="purple")
#' 
#' 
NULL




#' @export
setClass("gData", representation(coords="matrix", nodes.id="character", data="ANY",
                                 gGraph.name="character"),
         prototype(coords = matrix(numeric(0), ncol=2, dimnames=list(NULL, c("lon","lat"))),
                   nodes.id = character(0),
                   data=NULL,
                   gGraph.name="")
         )





####################
## VALIDITY METHODS
####################
#' @export
.gGprah.valid <- function(object){
    x <- object
    N <- nrow(x@coords)

    if(N == 0) return(TRUE) # empty object always valid

    ## several cases of non-validity

    ## coords not numeric
    if(!is.numeric(x@coords)){
        cat("\n Content of coords is not numeric.")
        return(FALSE)
    }

    ## wrong nrow for nodes attributes
    temp <- nrow(x@nodes.attr)
    if(temp > 0 && temp != N){
        cat("\n Number of coords do not match number of node attributes.")
        return(FALSE)
    }

    ## NAs in coords
    if(any(is.na(x@coords))){
        cat("\n NAs in coords coordinates.")
        return(FALSE)
    }

    ## node labels consistency
    if(!all(rownames(x@coords)==nodes(x@graph))){
        cat("\n Row names of @coords do not match node names of @graph.")
        return(FALSE)
    }


    return(TRUE)
} # end .gGprah.valid





## .gGprahHistory.valid <- function(object){
##     x <- object
##     Lcmd <- length(x@cmd)
##     Ldates <- length(x@dates)
##     Lcomments <- length(x@comments)
##     ## several cases of non-validity ##

##     ## empty object always ok
##     if(all(c(Lcmd,Ldates,Lcomments) == 0)) return(TRUE)

##     ## different length
##     if(length(unique(c(Lcmd, Ldates, Lcomments)))>1) {
##         cat("\n Components have different lengths.")
##         return(FALSE)
##     }

##     ## cmd wrong class
##     if(!all(sapply(x@cmd, class)=="expression")){
##         cat("\n Some cmd components are not calls.")
##         return(FALSE)
##     }

##     return(TRUE)
## } # end .gGprahHistory.valid





#' @export
.gData.valid <- function(object){
    x <- object
    Ncoords <- nrow(x@coords)
    Nnodes <- length(x@nodes.id)

    ## dim matching
    if(Ncoords != Nnodes){
        cat("\n Number of coordinates and of nodes do not match.")
        return(FALSE)
    }

    ## gGraph object
    if(!exists(x@gGraph.name, env=.GlobalEnv)){
        warning(paste("The gGraph object",x@gGraph.name,"is missing."))
    }

    return(TRUE)
} # end .gData.valid





#' @export
setValidity("gGraph", .gGprah.valid)
## setValidity("gGraphHistory", .gGprahHistory.valid)
#' @export
setValidity("gData", .gData.valid)


## is.gGraphHistory <- function(x){
##     res <- (is(x, "gGraphHistory") & validObject(x))
##     return(res)
## }

#' @export
is.gGraph <- function(x){
    res <- (is(x, "gGraph") & validObject(x))
    return(res)
}

#' @export
is.gData <- function(x){
    res <- (is(x, "gData") & validObject(x))
    return(res)
}






################
## CONSTRUCTORS
################

##################
## gGraphHistory
##################
## setMethod("initialize", "gGraphHistory", function(.Object, ...) {
##     x <- .Object
##     input <- list(...)
##     inputClasses <- sapply(input, class)


##     ## handle ... ##
##     if(is.null(input$cmd)){
##         input$cmd <- expression()
##     }

##     if(is.null(input$dates)){
##         input$dates <- format(Sys.time())
##     } else{
##         input$dates <- as.character(input$dates)
##     }

##     if(is.null(input$comments)){
##         input$comments <- ""
##     } else{
##         input$comments <- as.character(input$comments)
##     }


##     ## if a gGraphHistory object is provided in ..., merge data with it. ##
##     if(length(input)>0 && any(inputClasses=="gGraphHistory")){
##         prevObj <- input[[which(inputClasses=="gGraphHistory")[1]]] # 1st obj taken if several provided
##         res <- prevObj
##         res@cmd[[length(res@cmd)+1]] <- input$cmd
##         res@dates <- c(res@dates, input$dates)
##         res@comments <- c(res@comments, input$comments)
##     } else{
##         res <- x
##         res@cmd[[length(res@cmd)+1]] <- input$cmd
##         res@dates <- input$dates
##         res@comments <- input$comments
##     }

##     return(res)
## }) # end gGraphHistory constructor






##########
## gGraph
##########
#' @export
setMethod("initialize", "gGraph", function(.Object, ...) {
    x <- .Object
    input <- list(...)

    ## handle @coords ##
    if(!is.null(input$coords)){
        if(is.list(input$coords) && length(input$coords)==2) {
            input$coords <- as.data.frame(input$coords)
        }

        if(is.data.frame(input$coords)){
            input$coords <- as.matrix(input$coords)
        }

        if(nrow(input$coords)>0 && !is.numeric(input$coords)) stop("Argument coords has to be numeric.")

        ## names of the matrix
        colnames(input$coords) <- c("lon","lat")
        rownames(input$coords) <- as.character(1:nrow(input$coords))

        ## check/rectify longitudes
        temp <- input$coords[,"lon"]>180
        input$coords[temp,"lon"] <- input$coords[temp,"lon"]-360

         x@coords <- input$coords
    }


    ## handle @nodes.attr ##
    if(!is.null(input$nodes.attr)){
        input$nodes.attr <- as.data.frame(input$nodes.attr)

        if(nrow(input$nodes.attr) != nrow(x@coords)){
            stop("Number of rows in nodes.attr differ from that of coords.")
        }

        x@nodes.attr <- input$nodes.attr
    }


    ## handle @graph ##
    if(is.null(input$graph)){ # graph not provided
        if(nrow(x@coords)>0){
            input$graph <- new("graphNEL", nodes=rownames(x@coords))
        } else{
            input$graph <- new("graphNEL")
        }
    } else { # graph provided
        if(nrow(x@coords)>0){
            nodes(input$graph) <- rownames(x@coords)
        }
    }

    x@graph <- input$graph


    ## ## handle history ##
    ## if(is.null(input$cmd)){
    ##     input$cmd <-sys.call(-2)
    ## }

    ## if(is.null(input$comments) || input$comments==""){
    ##     input$comments <- "Creation of the object (using new)."
    ## }

    ## x@history <- new("gGraphHistory", history=input$history,
    ##              cmd=input$cmd, dates=input$dates, comments=input$comments)


    ## return object
    return(x)
}) # end gGraph constructor






##########
## gData
##########
#' @export
setMethod("initialize", "gData", function(.Object, ...) {
    x <- .Object
    input <- list(...)
    inputClasses <- sapply(input, class)


    ## handle @coords ##
    if(!is.null(input$coords)){
        if(is.list(input$coords) && length(input$coords)==2) {
            input$coords <- as.data.frame(input$coords)
        }

        if(is.data.frame(input$coords)){
            input$coords <- as.matrix(input$coords)
        }

        if(nrow(input$coords)>0 && !is.numeric(input$coords)) stop("Argument coords has to be numeric.")

        ## names of the matrix
        colnames(input$coords) <- c("lon","lat")
        rownames(input$coords) <- as.character(1:nrow(input$coords))

        ## check/rectify longitudes
        temp <- input$coords[,"lon"]>180
        input$coords[temp,"lon"] <- input$coords[temp,"lon"]-360

        x@coords <- input$coords
    }


    ## handle gGraph.name and gGraph.version
    if(!is.null(input$gGraph.name)){
        if(!exists(input$gGraph.name, env=.GlobalEnv)){
            warning(paste("The gGraph object",input$gGraphName,"is missing."))
            myGraph <- NULL
        } else {
            myGraph <- get(input$gGraph.name, env=.GlobalEnv) # used later for node.id
            x@gGraph.name <- input$gGraph.name
        }

        ## if(is.null(input$gGraph.version) & !is.null(myGraph)){
        ##     x@gGraph.version <- myGraph@history@dates[length(myGraph@history@dates)]
        ## }
    } else{
        myGraph <- NULL
    }


    ## handle nodes.id ##
    if(is.null(input$nodes.id)){ # if nodes.id is not provided...
        if(!is.null(myGraph)){ # ... and if the gGraph is available
            x@nodes.id <- closestNode(myGraph, loc=x@coords) # deduce nodes.id from the gGraph
        }
    } else {
        x@nodes.id <- as.character(x@nodes.id)
        if(!is.null(myGraph)){
            if(!all(x@nodes.id %in% getNodes(myGraph))){
                warning(paste("Some nodes were not found in the gGraph object",x@gGraphName,"."))
            }
        }
    }


    ## handle data ##
    if(!is.null(input$data)){
        x@data <- input$data
    }

     return(x)
}) # end gData constructor




