###############################################################
###############################################################
## CLASSES DEFINITION FOR THE GEOGRAPH PACKAGE
###############################################################
###############################################################

#' @import methods
#' @import graph
NULL

######################
## CLASSES DEFINITION
######################
#' Formal class "gGraph"
#'
#' The class \code{gGraph} is a formal (S4) class storing geographic data.\cr
#' Such data are composed of a set of geographic coordinates of vertices (or
#' 'nodes'), and a graph describing connectivity between these vertices. Data
#' associated to the nodes can also be stored ('nodes attributes'), as well as
#' meta-information used when plotting the object, or when computing weights
#' associated to the edges based on nodes attributes.\cr
#'
#' In all slots, nodes are uniquely identified by their name (reference is
#' taken from the row names of \code{@coords} slot).
#'
#'
#' @name gGraph-class
#' @aliases gGraph gGraph-class [,gGraph-method [,gGraph,ANY,ANY-method
#' [,gGraph,ANY,ANY,ANY-method getCoords,gGraph-method getGraph,gGraph-method
#' getNodes,gGraph-method initialize,gGraph-method dropCosts,gGraph-method
#' dropCosts show,gGraph-method is.gGraph getGraph getCoords getNodes
#' @docType class
#' @section Objects from the class gGraph: \code{gGraph} objects can be created
#' by calls to \code{new("gGraph", ...)}, where '...' can be the following
#' arguments:
#'
#' @slot coords a matrix of spatial coordinates with two
#' columns, being respectively longitude (from -180 to 180) and latitude.
#' Positive numbers are intended as 'east' and 'north', respectively.
#' @slot nodes.attr a data.frame whose rows are nodes, and whose
#' columns are different variables associated to the nodes.
#' @slot meta list, most likely containing named data.frames (see
#' Slots).
#' @slot graph an object of the class \linkS4class{graphNEL},
#' from the \code{graph} package (see \code{class?graphNEL}), describing
#' connectivity among nodes.
#'
#' Note that none of these is mandatory: \code{new("gGraph")} would work, and
#' create an empty \code{gGraph} object.
#' @author Thibaut Jombart (\email{t.jombart@@imperial.ac.uk})
#' @seealso Related classes are:\cr % - \code{\linkS4class{graphNEL}}
#' (graph package): slot \code{@graph} in \code{gGraph}.\cr
#' @keywords classes spatial graphs
#' @exportClass gGraph
#' @examples
#'
#' ## create an empty object
#' new("gGraph")
#'
#'
#' ## plotting the object
#' plot(rawgraph.10k, reset = TRUE)
#'
#' ## zooming in
#' geo.zoomin(list(x = c(-6, 38), y = c(35, 73)))
#' title("Europe")
#'
#' ## to play interactively with graphics, use:
#' # geo.zoomin()
#' # geo.zoomout()
#' # geo.slide()
#' # geo.back()
#'
#' ## defining a new object restrained to visible nodes
#' x <- rawgraph.10k[isInArea(rawgraph.10k)]
#' plot(x, reset = TRUE, edges = TRUE)
#' title("x does just contain these visible nodes.")
#'
#' ## define weights for edges
#' x <- setCosts(x, attr.name = "habitat", method = "prod")
#' plot(x, edges = TRUE)
#' title("costs defined by habitat (land/land=1, other=100)")
#'
#' ## drop 'dead edges' (i.e. with weight 0)
#' x <- dropDeadEdges(x, thres = 10)
#' plot(x, edges = TRUE)
#' title("after droping edges with null weight")
#'
setClass(
  "gGraph",
  representation(
    coords = "matrix", nodes.attr = "data.frame", meta = "list",
    graph = "graphNEL"
  ),
  prototype(
    coords = matrix(numeric(0), ncol = 2, dimnames = list(NULL, c("lon", "lat"))),
    nodes.attr = data.frame(),
    meta = list(),
    graph = new("graphNEL")
  )
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
#' @slot coords a matrix of spatial coordinates with two
#' columns, being respectively longitude (from -180 to 180) and latitude.
#' Positive numbers are intended as 'east' and 'north', respectively.
#' @slot nodes.id a vector of character strings giving the name of the
#' nodes (of the \linkS4class{gGraph} object) associated to the locations.
#' @slot data any kind of data associated to the locations in coords.
#' For matrix-like objects, rows should correspond to locations.
#' @slot gGraph.name a character string the name of the
#' \linkS4class{gGraph} object against which the object is matched.
#'
#' Note
#' that none of these is mandatory: \code{new("gData")} would work, and create
#' an empty \code{gGraph} object. Also note that a finer matching of locations
#' against the nodes of a \code{gGraph} object can be achieved after creating
#' the object, for instance using the \code{closestNode} method.
#' @author Thibaut Jombart (\email{t.jombart@@imperial.ac.uk})
#' @seealso Related class:\cr - \code{\linkS4class{gGraph}}\cr
#' @examples
#'
#' hgdp
#'
#' ## plot data
#' plot(worldgraph.40k, pch = "")
#' points(hgdp)
#'
#' ## subset and plot data
#' onlyNorth <- hgdp[hgdp@data$Latitude > 0] # only northern populations
#'
#' plot(worldgraph.40k, reset = TRUE)
#' abline(h = 0) # equator
#' points(onlyNorth, pch.node = 20, cex = 2, col.node = "purple")
#'
#' @exportClass gData
setClass(
  "gData", representation(
    coords = "matrix", nodes.id = "character", data = "ANY",
    gGraph.name = "character"
  ),
  prototype(
    coords = matrix(numeric(0), ncol = 2, dimnames = list(NULL, c("lon", "lat"))),
    nodes.id = character(0),
    data = NULL,
    gGraph.name = ""
  )
)





####################
## VALIDITY METHODS
####################
#' @export
.gGraph.valid <- function(object) {
  x <- object
  N <- nrow(x@coords)

  if (N == 0) {
    return(TRUE)
  } # empty object always valid

  ## several cases of non-validity

  ## coords not numeric
  if (!is.numeric(x@coords)) {
    cat("\n Content of coords is not numeric.")
    return(FALSE)
  }

  ## wrong nrow for nodes attributes
  temp <- nrow(x@nodes.attr)
  if (temp > 0 && temp != N) {
    cat("\n Number of coords do not match number of node attributes.")
    return(FALSE)
  }

  ## NAs in coords
  if (any(is.na(x@coords))) {
    cat("\n NAs in coords coordinates.")
    return(FALSE)
  }

  ## node labels consistency
  if (!all(rownames(x@coords) == nodes(x@graph))) {
    cat("\n Row names of @coords do not match node names of @graph.")
    return(FALSE)
  }


  return(TRUE)
} # end .gGprah.valid




#' @export
.gData.valid <- function(object) {
  x <- object
  Ncoords <- nrow(x@coords)
  Nnodes <- length(x@nodes.id)

  ## dim matching
  if (Ncoords != Nnodes) {
    cat("\n Number of coordinates and of nodes do not match.")
    return(FALSE)
  }

  ## gGraph object
  if (!exists(x@gGraph.name, envir = .GlobalEnv)) {
    warning(paste("The gGraph object", x@gGraph.name, "is missing."))
  }

  return(TRUE)
} # end .gData.valid





#' @export
setValidity("gGraph", .gGraph.valid)
## setValidity("gGraphHistory", .gGprahHistory.valid)
#' @export
setValidity("gData", .gData.valid)

#' @export
is.gGraph <- function(x) {
  res <- (is(x, "gGraph") & validObject(x))
  return(res)
}

#' @export
is.gData <- function(x) {
  res <- (is(x, "gData") & validObject(x))
  return(res)
}






################
## CONSTRUCTORS
################

##########
## gGraph
##########
#' @export
setMethod("initialize", "gGraph", function(.Object, ...) {
  x <- .Object
  input <- list(...)

  ## handle @coords ##
  if (!is.null(input$coords)) {


    if (is.list(input$coords)) {
      input$coords <- as.data.frame(input$coords)
    }

    if (is.data.frame(input$coords)) {
      input$coords <- as.matrix(input$coords)
    }
    
    if (ncol(input$coords)!=2){
      stop("Argument coords must include only two columns (longitude and latitude).")
    }
    
    if (nrow(input$coords) > 0 && !is.numeric(input$coords)) {
      stop("Argument coords has to be numeric.")
      }
    
    ## NAs in coords
    if (any(is.na(input$coords))) {
      stop("Argument coords includes NAs")
    }
    
    ## Convert all column names to lower case
    colnames(input$coords) <- tolower(colnames(input$coords))
    ## Create list of lon/lat column heading names
    lonlist <- list("lon", "longitude", "x")
    latlist <- list("lat", "latitude", "y")
    ## Test if the column order is inverted
    if (is.element(colnames(input$coords)[1], latlist) & 
        is.element(colnames(input$coords)[2], lonlist)) {
      input$coords[, c(1, 2)] <- input$coords[, c(2, 1)]
    } else if  (!(is.element(colnames(input$coords)[1], lonlist) & 
                  is.element(colnames(input$coords)[2], latlist))){
      message("The coordinate column names are not part of the standardised list;\n",
      "we will use the order they were given in, make sure it corresponds to x and y!")
    } # if neither of the if catches it, then the names are part of the lists and in the correct order

    ## names of the matrix
    colnames(input$coords) <- c("lon", "lat")
    rownames(input$coords) <- as.character(1:nrow(input$coords))

    ## check/rectify longitudes
    temp <- input$coords[, "lon"] > 180
    input$coords[temp, "lon"] <- input$coords[temp, "lon"] - 360

    x@coords <- input$coords
  }


  ## handle @nodes.attr ##
  if (!is.null(input$nodes.attr)) {
    input$nodes.attr <- as.data.frame(input$nodes.attr)

    if (nrow(input$nodes.attr) != nrow(x@coords)) {
      stop("Number of rows in nodes.attr differ from that of coords.")
    }

    x@nodes.attr <- input$nodes.attr
  }


  ## handle @graph ##
  if (is.null(input$graph)) { # graph not provided
    if (nrow(x@coords) > 0) {
      input$graph <- new("graphNEL", nodes = rownames(x@coords))
    } else {
      input$graph <- new("graphNEL")
    }
  } else { # graph provided
    if (nrow(x@coords) > 0) {
      nodes(input$graph) <- rownames(x@coords)
    }
  }

  x@graph <- input$graph

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
  if (!is.null(input$coords)) {
    if (is.list(input$coords) && length(input$coords) == 2) {
      input$coords <- as.data.frame(input$coords)
    }

    if (is.data.frame(input$coords)) {
      input$coords <- as.matrix(input$coords)
    }

    if (nrow(input$coords) > 0 && !is.numeric(input$coords)) stop("Argument coords has to be numeric.")

    ## names of the matrix
    colnames(input$coords) <- c("lon", "lat")
    rownames(input$coords) <- as.character(1:nrow(input$coords))

    ## check/rectify longitudes
    temp <- input$coords[, "lon"] > 180
    input$coords[temp, "lon"] <- input$coords[temp, "lon"] - 360

    x@coords <- input$coords
  }


  ## handle gGraph.name and gGraph.version
  if (!is.null(input$gGraph.name)) {
    if (!exists(input$gGraph.name, envir = .GlobalEnv)) {
      warning(paste("The gGraph object", input$gGraphName, "is missing."))
      myGraph <- NULL
    } else {
      myGraph <- get(input$gGraph.name, envir = .GlobalEnv) # used later for node.id
      x@gGraph.name <- input$gGraph.name
    }

    ## if(is.null(input$gGraph.version) & !is.null(myGraph)){
    ##     x@gGraph.version <- myGraph@history@dates[length(myGraph@history@dates)]
    ## }
  } else {
    myGraph <- NULL
  }


  ## handle nodes.id ##
  if (is.null(input$nodes.id)) { # if nodes.id is not provided...
    if (!is.null(myGraph)) { # ... and if the gGraph is available
      x@nodes.id <- closestNode(myGraph, loc = x@coords) # deduce nodes.id from the gGraph
    }
  } else {
    x@nodes.id <- as.character(x@nodes.id)
    if (!is.null(myGraph)) {
      if (!all(x@nodes.id %in% getNodes(myGraph))) {
        warning(paste("Some nodes were not found in the gGraph object", x@gGraphName, "."))
      }
    }
  }


  ## handle data ##
  if (!is.null(input$data)) {
    x@data <- input$data
  }

  return(x)
}) # end gData constructor
