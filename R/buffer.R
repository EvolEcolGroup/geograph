#' Compute buffers around locations for gGraph and gData objects
#'
#' The generic function \code{buffer} finds buffers around specified locations
#' of a \linkS4class{gGraph} or a \linkS4class{gData} object. Different format
#' for the output are available.
#'
#' The computed buffers are sets of nodes lying within a given distance of
#' specified locations. All nodes of a buffer need to be connected to the
#' location they surround.
#'
#'
#' @aliases buffer buffer-methods buffer,gGraph-method buffer,gData-method
#' @param x a valid \linkS4class{gGraph} or \linkS4class{gData} object.
#' @param \dots further arguments passed to specific methods.
#' @param nodes a character vector identifying the nodes around which buffers
#' should be computed.
#' @param d the radius of the buffer, in km.
#' @param res.type the type of result that should be returned (see section
#' \code{value}.
#' @return The output depends on the value of the argument \code{res.type}:\cr
#' - \code{nodes}: a vector of characters identifying the nodes of the
#' buffers.\cr
#'
#' - \code{gGraph}: a \linkS4class{gGraph} object with a new attribute "buffer"
#' (TRUE: within buffers; FALSE: outside buffers), and new color rules for this
#' attribute in \code{@meta$buf.colors}.\cr
#'
#' - \code{gData}: a \linkS4class{gData} object including all the nodes of the
#' buffers.\cr

#' @keywords utilities methods
#' @name buffer
#' @examples
#'
#' #### gGraph example ####
#' ## zoom in to an area
#' plot(worldgraph.10k, reset = TRUE)
#' geo.zoomin(list(x = c(-6, 38), y = c(35, 73)))
#'
#' ## identify one node
#' oneNodeXY <- c(getCoords(worldgraph.10k)[9299, 1], getCoords(worldgraph.10k)[9299, 2])
#' points(oneNodeXY[1], oneNodeXY[2], col = "red")
#'
#' ## find some buffers
#' buffer(worldgraph.10k, "9299", 100) # nothing around 100km
#' buffer(worldgraph.10k, "9299", 500)
#' buf500km <- buffer(worldgraph.10k, "9299", 500, res = "gGraph")
#' plot(buf500km, col.rules = buf500km@meta$buf.colors)
#' buf1000km <- buffer(worldgraph.10k, "9299", 1000, res = "gGraph")
#' plot(buf1000km, col.rules = buf1000km@meta$buf.colors)
#'
#'
#' #### gData example ####
#' x <- hgdp[27:30] # retain a subset of hgdp
#' plot(x, reset = TRUE, col.g = "lightgrey", pch.node = 20)
#' buf.200 <- buffer(x, 200, res = "gData")
#' buf.400 <- buffer(x, 400, res = "gData")
#' buf.600 <- buffer(x, 600, res = "gData")
#' buf.1000 <- buffer(x, 1000, res = "gData")
#' points(buf.1000, col.node = "black")
#' points(buf.600, col.node = "yellow")
#' points(buf.400, col.node = "gold")
#' points(buf.200, col.node = "orange")
#' title("Different buffers for a gData \n(100km, 200km, 500km)")
#'
NULL

#########
## buffer
#########
#' @export
setGeneric("buffer", function(x, ...) {
  standardGeneric("buffer")
})





################
## gGraph method
################
#' @export
#' @rdname buffer

setMethod("buffer", "gGraph", function(x, nodes, d, res.type = c("nodes", "gGraph"), ...) {
  ## CHECKS ##
  if (!is.gGraph(x)) stop("x is not a valid gGraph object")
  if (!is.numeric(d)) stop("d is not numeric")
  if (d > 1e4) warning("Buffer distance is greater than 10,000km; computations may be long.")
  res.type <- match.arg(res.type)

  ALL.NODES <- getNodes(x)
  if (!all(nodes %in% ALL.NODES)) stop("Some requested nodes do not exist in the gGraph grid.")

  GRAPH <- getGraph(x)
  EDGES <- edges(GRAPH)
  XY <- getCoords(x)


  ## FIND BUFFER FOR A NODE ##
  find.buf.onenode <- function(node, d) {
    curNodes <- node
    res <- node
    visited.nodes <- node

    while (TRUE) {
      neig <- unlist(EDGES[curNodes])
      neig <- setdiff(neig, visited.nodes)
      visited.nodes <- c(visited.nodes, neig)

      temp <- fields::rdist.earth(XY[node, , drop = FALSE], XY[neig, , drop = FALSE], miles = FALSE, R = NULL)
      toKeep <- temp < d
      if (!any(toKeep)) break # exit
      curNodes <- neig[toKeep]
      res <- c(res, neig[toKeep])
    }
    return(res)
  }


  ## FIND BUFFER FOR ALL REQUESTED NODES ##
  res <- unlist(lapply(nodes, find.buf.onenode, d))


  ## RETURN RESULTS ##
  res <- unique(res)
  if (res.type == "nodes") {
    return(res)
  } # if res.type is nodes


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
  newATTR <- cbind.data.frame(ALL.ATTR, buffer = bufAttr)
  x@nodes.attr <- newATTR

  ## set new color rules
  x@meta$buf.colors <- data.frame(buffer = c(TRUE, FALSE), color = c("orange", "lightgrey"))
  return(x)
}) # end buffer for gGraph







################
## gGraph method
################
#' @export
#' @rdname buffer
setMethod("buffer", "gData", function(x, d, res.type = c("nodes", "gData", "gGraph"), ...) {
  ## CHECKS ##
  res.type <- match.arg(res.type)
  if (!is.gData(x)) stop("x is not a valid gData object")


  ## EXTRACT ARGUMENTS FOR FURTHER METHOD ##
  myNodes <- getNodes(x)
  myGraph <- get(x@gGraph.name, envir = .GlobalEnv)


  ## CALL UPON gGraph METHOD ##
  if (res.type == "gGraph") { # if result sought is gGraph
    res <- buffer(myGraph, myNodes, d, res.type = "gGraph")
    return(res)
  }

  # if result sought is nodes or gData
  temp <- buffer(myGraph, myNodes, d, res.type = "nodes")
  if (res.type == "nodes") {
    return(temp)
  } # if res.type is nodes

  ## else ... (res.type == gData)
  res <- new("gData", coords = getCoords(myGraph)[temp, , drop = FALSE], gGraph.name = x@gGraph.name)

  return(res)
}) # end buffer for gData
