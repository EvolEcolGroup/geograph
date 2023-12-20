#' Find the closest node to a given location
#'
#' The function \code{closestNode} searches for the closest node in a
#' \linkS4class{gGraph} or a \linkS4class{gData} object to a given location. It
#' is possible to restrain the research to given values of a node attribute.
#' For instance, one can search the closest node on land to a given
#' location.\cr
#'
#' This function is also used to match locations of a \linkS4class{gData}
#' object with nodes of the \code{gGraph} object to which it is linked.
#'
#' When creating a \linkS4class{gData} object, if the \code{gGraph.name}
#' argument is provided, then locations are matched with the \code{gGraph}
#' object automatically, by an internal call to closestNode. Note, however,
#' that it is not possible to specify node attributes (\code{attr.names} and
#' \code{attr.values}) this way.
#'
#' @aliases closestNode closestNode-methods closestNode,gGraph-method
#' closestNode,gData-method
#' @param x a valid \linkS4class{gGraph} or \linkS4class{gData} object. In the
#' latter case, the \linkS4class{gGraph} to which the \linkS4class{gData} is
#' linked has to be in the current environment.
#' @param \dots further arguments passed to specific methods.
#' @param loc locations, specified as a list with two components indicating
#' longitude and latitude of locations. Alternatively, this can be a data.frame
#' or a matrix with longitude and latitude in columns, in this order. Note that
#' \code{locator()} can be used to specify interactively the locations.
#' @param zoneSize a numeric value indicating the size of the zone (in
#' latitude/longitude units) where the closest node is searched for. Note that
#' this only matters for speed purpose: if no closest node is found inside a
#' given zone, the zone is expanded until nodes are found.
#' @param attr.name the optional name of a node attribute. See details.
#' @param attr.values an optional vector giving values for \code{attr.names}.
#' See details.
#' @return If \code{x} is a \linkS4class{gGraph} object: a vector of node
#' names.\cr
#'
#' If \code{x} is a \linkS4class{gData} object: a \linkS4class{gData} object
#' with matching nodes stored in the \code{@nodes.id} slot. Note that previous
#' content of \code{@nodes.id} will be erased.\cr

#' @seealso \code{\link{geo.add.edges}} and \code{\link{geo.remove.edges}} to
#' interactively add or remove edges in a \linkS4class{gGraph} object.
#' @keywords utilities methods
#' @export
#' @examples
#' \dontrun{
#' ## interactive example ##
#' plot(worldgraph.10k, reset = TRUE)
#'
#' ## zooming in
#' geo.zoomin(list(x = c(-6, 38), y = c(35, 73)))
#' title("Europe")
#'
#' ## click some locations
#' myNodes <- closestNode(worldgraph.10k, locator(), attr.name = "habitat", attr.value = "land")
#' myNodes
#'
#' ## here are the closestNodes
#' points(getCoords(worldgraph.10k)[myNodes, ], col = "red")
#' }
#'
#' ## example with a gData object ##
#' myLoc <- list(x = c(3, -8, 11, 28), y = c(50, 57, 71, 67)) # some locations
#' obj <- new("gData", coords = myLoc) # new gData object
#' obj
#'
#' obj@gGraph.name <- "worldgraph.10k" # this could be done when creating obj
#' obj <- closestNode(obj, attr.name = "habitat", attr.value = "land")
#'
#' ## plot the result (original location -> assigned node)
#' plot(obj, method = "both", reset = TRUE)
#' title("'x'=location, 'o'=assigned node")
#'
#'
###############
## closestNode
###############
setGeneric("closestNode", function(x, ...) {
  standardGeneric("closestNode")
})






###############
## closestNode for gGraph
###############
#' @describeIn closestNode Method for gGraph
#' @export
setMethod("closestNode", "gGraph", function(x, loc, zoneSize = 5, attr.name = NULL, attr.values = NULL) {
  ## handle arguments
  if (!is.gGraph(x)) stop("x is not a valid gGraph object.")
  loc <- as.data.frame(loc)
  if (ncol(loc) != 2) stop("coords does not have two columns.")
  coords <- getCoords(x)
  nodes <- getNodes(x)

  ## handle attribute specification if provided
  if (!is.null(attr.name)) {
    temp <- unlist(getNodesAttr(x, attr.name = attr.name))
    temp <- as.character(temp)
    hasRightAttr <- temp %in% attr.values
    if (!any(hasRightAttr)) stop(paste("specified values of", attr.name, "never found."))
  } else {
    hasRightAttr <- TRUE
  }

  ## function finding the closest node for 1 loc ##
  closeOne <- function(oneLoc) {
    ## define area around loc
    reg <- list()
    toKeep <- character(0) # will contain node names

    while (length(toKeep) < 3) { # enlarge zoneSize until at least 3 candidates appear
      ## define region
      reg$x <- oneLoc[1] + c(-zoneSize, zoneSize) # +- zoneZine in long
      reg$y <- oneLoc[2] + c(-zoneSize, zoneSize) # +- zoneZine in lat

      ## isolate nodes in this area
      toKeep <- isInArea(x, reg) # ! from now nodes indices won't match those of x and coords

      ## intersect with attribute selection
      toKeep <- toKeep & hasRightAttr

      ## toKeep must be a character to insure matching
      toKeep <- nodes[toKeep]

      ## increment zoneSize
      zoneSize <- zoneSize * 1.5
    } # end while

    xy <- coords[toKeep, , drop = FALSE]

    ## compute all great circle distances between nodes and loc
    temp <- fields::rdist.earth(xy, matrix(oneLoc, nrow = 1))
    closeNode <- rownames(temp)[which.min(temp)]
    return(closeNode)
  } # end closeOne


  ## apply closeOne to all requested locations
  res <- apply(loc, 1, closeOne) # these are node labels

  ## must not return indices, as this would not work for subsets of data
  ## e.g. closestPoint[x[getNodesAttr(x)=="land"]] will return wrong indices
  ## temp <- res
  ## res <- match(res, getNodes(x))
  ## names(res) <- temp

  return(res)
}) # end closestNode for gGraph






###############
## closestNode for gData
###############
#' @describeIn closestNode Method for gData
#' @export
setMethod("closestNode", "gData", function(x, zoneSize = 5, attr.name = NULL, attr.values = NULL) {
  ## get coords ##
  xy <- getCoords(x)

  ## get gGraph object ##
  if (!exists(x@gGraph.name, envir = .GlobalEnv)) stop(paste("gGraph object", x@gGraph.name, "does not exist."))
  obj <- get(x@gGraph.name, envir = .GlobalEnv)

  ## make a call to the gGraph method ##
  res <- closestNode(obj, loc = xy, zoneSize = zoneSize, attr.name = attr.name, attr.values = attr.values)

  ## return result ##
  x@nodes.id <- res

  return(x)
}) # end closestNode for gData
