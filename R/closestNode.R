#' Find the closest node to a given location
#'
#' The function `closestNode` searches for the closest node in a
#' [`gGraph`] or a [`gData`] object to a given location. It is possible to
#' restrain the research to given values of a node attribute. For instance,
#' one can search the closest node on land to a given location.
#'
#' This function is also used to match locations of a [`gData`] object with
#' nodes of the [`gGraph`] object to which it is linked.
#'
#' When creating a [`gData`] object, if the `gGraph.name` argument is
#' provided, then locations are matched with the [`gGraph`] object
#' automatically, by an internal call to `closestNode`. Note, however, that
#' it is not possible to specify node attributes (`attr.name` and
#' `attr.values`) this way.
#'
#' @param x a valid [`gGraph`] or [`gData`] object. In the latter case, the
#'   [`gGraph`] to which the [`gData`] is linked has to be in the current
#'   environment.
#' @param loc locations, specified as a list with two components indicating
#'   longitude and latitude of locations. Alternatively, this can be a
#'   `data.frame` or a matrix with longitude and latitude in columns, in
#'   this order. Note that [`locator()`] can be used to specify the
#'   locations interactively.
#' @param method the method to use for finding the closest node. The default is
#'  `"knn"`, which uses a fast FNN-based spatial index over unit-sphere Cartesian coordinates.
#'  The legacy `"inArea"` method is also available, but it is slower.
#' @param zoneSize The initial size of the search area (in degrees). Only needs to
#' be specified if `method = "inArea"`. The search zone will be expanded until at
#' least 3 candidate nodes are found.
#' @param attr.name the optional name of a node attribute. See details.
#' @param attr.values an optional vector giving values for `attr.name`.
#'   See details.
#' @param ... further arguments passed to specific methods.
#' @return If `x` is a [`gGraph`] object: a vector of node names.
#'
#'   If `x` is a [`gData`] object: a [`gData`] object with matching nodes
#'   stored in the `@nodes.id` slot. Note that previous content of
#'   `@nodes.id` will be erased.
#' @export
#' @importFrom FNN get.knnx
#'
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
#' points(getCoords(worldgraph.10k)[myNodes, , drop = FALSE], col = "red")
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
#' plot(obj, type = "both", reset = TRUE)
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
setMethod("closestNode", "gGraph", function(x, loc, method = "knn",
                                            zoneSize = 5, attr.name = NULL, attr.values = NULL) {
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
    hasRightAttr <- rep(TRUE, length(nodes))
  }

  ## use the specified method
  if (method == "knn") {
    res <- .closeOneKnn(x, loc, coords, nodes, hasRightAttr)
  } else if (method == "inArea") {
    res <- apply(loc, 1, .closeOneLegacy,
      coords = coords, nodes = nodes,
      hasRightAttr = hasRightAttr, zoneSize = zoneSize, x = x
    )
  } else {
    stop("method must be either 'knn' or 'inArea'.")
  }

  return(res)
}) # end closestNode for gGraph


###############
## closestNode for gData
###############
#' @describeIn closestNode Method for gData
#' @export
setMethod("closestNode", "gData", function(x, method = "knn", zoneSize = 5,
                                           attr.name = NULL, attr.values = NULL) {
  ## get coords ##
  xy <- getCoords(x)

  ## get gGraph object ##
  if (!exists(x@gGraph.name, envir = .GlobalEnv)) stop(paste("gGraph object", x@gGraph.name, "does not exist."))
  obj <- get(x@gGraph.name, envir = .GlobalEnv)

  ## make a call to the gGraph method, pass zoneSize only if user supplied it
  res <- closestNode(obj, method = method, zoneSize = zoneSize, loc = xy,
                     attr.name = attr.name, attr.values = attr.values)

  ## return result ##
  x@nodes.id <- res

  return(x)
}) # end closestNode for gData


#' internal: convert a lon/lat matrix to unit-sphere Cartesian coordinates
#' @noRd
.buildNnIndex <- function(coords) {
  lon <- coords[, 1] * pi / 180
  lat <- coords[, 2] * pi / 180
  cbind(
    cos(lat) * cos(lon),
    cos(lat) * sin(lon),
    sin(lat)
  )
}


#' internal: knn version of the function
#' @noRd
## default: FNN over unit-sphere XYZ
.closeOneKnn <- function(x, loc, coords, nodes, hasRightAttr) {
  if (is.null(x@meta$.xyz)) x@meta$.xyz <- .buildNnIndex(coords)
  cand.xyz <- x@meta$.xyz[hasRightAttr, , drop = FALSE]
  cand.nodes <- nodes[hasRightAttr]
  qxyz <- .buildNnIndex(as.matrix(loc))
  nn.idx <- FNN::get.knnx(cand.xyz, qxyz, k = 1)$nn.index[, 1]
  res <- cand.nodes[nn.idx]
  names(res) <- rownames(loc)
  return(res)
}

#' internal: legacy zone expansion version of the function
#' @noRd
.closeOneLegacy <- function(oneLoc, coords, nodes, hasRightAttr, zoneSize, x) {
  ## define area around loc
  reg <- list()
  toKeep <- character(0) # will contain node names

  while (length(toKeep) < 3) { # enlarge zoneSize until at least 3 candidates appear
    ## define region
    reg$x <- oneLoc[1] + c(-zoneSize, zoneSize) # +- zoneZine in long
    reg$y <- oneLoc[2] + c(-zoneSize, zoneSize) # +- zoneZine in lat

    ## isolate nodes in this area
    toKeep <- isInArea(x, reg, quiet = TRUE) # ! from now nodes indices won't match those of x and coords

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
}
