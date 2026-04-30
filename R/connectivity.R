#' Check connectivity of a gGraph object
#'
#' The functions \code{areNeighbours}, \code{areConnected} and the method
#' \code{isConnected} test connectivity in different ways.\cr
#'
#' - \code{isConnected}: tests if the nodes of a \linkS4class{gData} object
#' form a connected set. Note that this is a method for \linkS4class{gData},
#' the generic being defined in the \code{graph} package.\cr
#'
#'
#' - \code{connectivityPlot}: plots connected sets of a \linkS4class{gGraph} or
#' a \linkS4class{gData} object with different colors.\cr
#'
#' In \code{connectivityPlot}, isolated nodes (i.e. belonging to no connected
#' set of size > 1) are plotted in light gray.
#'
#' @aliases
#' connectivityPlot connectivityPlot-methods connectivityPlot,gGraph-method
#' connectivityPlot,gData-method
#' @param x a valid \linkS4class{gGraph} object.
#' @param \dots other arguments passed to other methods.
#' @param seed an optional integer giving the seed to be used when randomizing
#' colors. One given seed will always give the same set of colors. NULL by
#' default, meaning colors are randomized each time a plot is drawn.
#' @param col.gGraph a character string or a number indicating the color of the
#' nodes to be used when plotting the \linkS4class{gGraph} object. Defaults to
#' '0', meaning that nodes are invisible.
#' @return - \code{areNeighbours}: a vector of logical, having one value for
#' each couple of nodes.\cr
#'
#' - \code{areConnected}: a single logical value, being TRUE if nodes form a
#' connected set.\cr
#'
#' - \code{isConnected}: a single logical value, being TRUE if nodes of the
#' object form a connected set.\cr
#' @include classes.R
#' @keywords utilities methods
#' @name connectivity
#' @examples
#'
#' connectivityPlot(rawgraph.10k)
#' connectivityPlot(worldgraph.10k)
#'
NULL



















#####################
## connectivityPlot
#####################
#' @rdname connectivity
#' @export
setGeneric("connectivityPlot", function(x, ...) {
  standardGeneric("connectivityPlot")
})


##################
## gGraph method
##################
#' @rdname connectivity
#' @export
setMethod("connectivityPlot", "gGraph", function(x, ..., seed = NULL) {
  ## some checks ##
  if (!is.gGraph(x)) stop("x is not a valid gGraph object")

  ## create the .geoGraphEnv if it does not exist
  # am315 This should not be necessary, as .geoGraphEnv should always exist
  # if(!exists(".geoGraphEnv", envir=.GlobalEnv)) {
  #     assign(".geoGraphEnv",  new.env(parent=.GlobalEnv), envir=.GlobalEnv)
  #     warning(".geoGraphEnv was not present, which may indicate a problem in loading geoGraph.")
  # }

  # env <- get(".geoGraphEnv", envir=.GlobalEnv) # env is our target environnement


  ## get connected sets ##
  connected.sets <- RBGL::connectedComp(getGraph(x))

  ## just keep sets > 1 node
  temp <- sapply(connected.sets, length)
  reOrd <- order(temp, decreasing = TRUE) # sets ordered in decreasing size
  temp <- temp[reOrd]
  if (min(temp) == 1) {
    connected.sets <- connected.sets[reOrd][1:(which.min(temp) - 1)]
  }

  names(connected.sets) <- paste("set", 1:length(connected.sets))


  ## define colors ##
  nbSets <- length(connected.sets)
  if (!is.null(seed) && is.numeric(seed)) {
    set.seed(seed)
  }

  colSets <- sample(grDevices::rainbow(nbSets))

  myNodes <- getNodes(x)
  col <- rep("lightgray", length(myNodes))
  names(col) <- myNodes

  for (i in 1:nbSets) {
    e <- connected.sets[[i]] # 'e' is a vector of connected nodes
    col[e] <- colSets[i]
  }


  ## call to plot ##
  plot(x, col = col, ...)

  ## save plot param ## (will be used by plot gGraph
  dots <- list(...)
  temp <- get("last.plot.param", envir = .geoGraphEnv)
  if (!is.null(dots$psize)) {
    temp$psize <- dots$psize
  }
  if (!is.null(dots$pch)) {
    temp$pch <- dots$pch
  }
  temp$col <- col
  assign("last.plot.param", temp, envir = .geoGraphEnv)

  ## fix last call ##
  curCall <- sys.call(-1)
  assign("last.plot", curCall, envir = .geoGraphEnv)

  return(invisible(col))
}) # end connectivityPlot gGraph


#################
## gData method
#################
#' @rdname connectivity
#' @export
setMethod("connectivityPlot", "gData", function(x, col.gGraph = 0, ..., seed = NULL) {
  ## some checks ##
  if (!is.gData(x)) stop("x is not a valid gData object")

  env <- get(".geoGraphEnv", envir = .GlobalEnv) # env is our target environnement

  ## get connected sets ##
  connected.sets <- RBGL::connectedComp(getGraph(x))

  ## just keep sets > 1 node
  temp <- sapply(connected.sets, length)
  reOrd <- order(temp, decreasing = TRUE) # sets ordered in decreasing size
  temp <- temp[reOrd]
  if (min(temp) == 1) {
    connected.sets <- connected.sets[reOrd][1:(which.min(temp) - 1)]
  }

  names(connected.sets) <- paste("set", 1:length(connected.sets))


  ## define colors ##
  nbSets <- length(connected.sets)
  ## find the number of relevant sets
  nbRelSets <- 0
  myNodes <- getNodes(x)

  for (i in 1:nbSets) {
    if (any(myNodes %in% connected.sets[[i]])) {
      nbRelSets <- nbRelSets + 1
    }
  }

  if (!is.null(seed) && is.numeric(seed)) {
    set.seed(seed)
  }
  colSets <- sample(grDevices::rainbow(nbRelSets))

  col <- rep("lightgray", length(myNodes))
  names(col) <- myNodes

  for (i in 1:nbSets) {
    e <- connected.sets[[i]] # 'e' is a vector of connected nodes
    col[names(col) %in% e] <- colSets[i]
  }


  ## call to plot ##
  plot(x, col.ori = col, col.nodes = col, col.gGraph = col.gGraph, ...)


  ## fix last call ##
  curCall <- sys.call(-1)
  assign("last.plot", curCall, envir = .geoGraphEnv)

  return(invisible(col))
}) # end connectivityPlot gData
