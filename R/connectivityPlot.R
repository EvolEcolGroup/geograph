#' Plot connected sets of a gGraph object
#'
#' The function `connectivityPlot` plots the connected sets of a [`gGraph`] or
#' [`gData`] object with different colors. Isolated nodes (i.e. belonging to
#' no connected set of size > 1) are plotted in light gray.
#'
#' @param x a valid [`gGraph`] or [`gData`] object.
#' @param ... other arguments passed to other methods.
#' @param seed an optional integer giving the seed to be used when randomizing
#'   colors. A given seed will always produce the same set of colors. `NULL`
#'   by default, meaning colors are randomized each time a plot is drawn.
#' @return A named character vector of colors, one per node, returned
#'   invisibly.
#' @include classes.R
#' @examples
#' # plot connected sets of a gGraph object
#' connectivityPlot(worldgraph.10k)
#'
#' # plot connected sets of a gData object
#' connectivityPlot(hgdp)
#' @export
setGeneric("connectivityPlot", function(x, ...) {
  standardGeneric("connectivityPlot")
})


##################
## gGraph method
##################
#' @describeIn connectivityPlot Method for gGraph objects
#' @export
setMethod("connectivityPlot", "gGraph", function(x, ..., seed = NULL) {
  ## some checks ##
  if (!is.gGraph(x)) stop("x is not a valid gGraph object")

  ## get connected sets (with more than one node) ##
  connected.sets <- .connectedSets(getGraph(x))

  ## define colors ##
  nbSets <- length(connected.sets)
  if (!is.null(seed) && is.numeric(seed)) {
    set.seed(seed)
  }

  colSets <- sample(grDevices::rainbow(nbSets))

  myNodes <- getNodes(x)
  col <- rep("lightgray", length(myNodes))
  names(col) <- myNodes

  ## set of each node (NA if in no set), from one lookup instead of one
  ## named assignment per set
  set.id <- rep(seq_len(nbSets), lengths(connected.sets))[match(myNodes, unlist(connected.sets, use.names = FALSE))]
  inSet <- !is.na(set.id)
  col[inSet] <- colSets[set.id[inSet]]

  ## call to plot ##
  plot(x, col = col, ...)

  ## save plot param ## (will be used by plot gGraph)
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
#' @describeIn connectivityPlot Method for gData objects
#' @export
setMethod("connectivityPlot", "gData", function(x, ..., seed = NULL) {
  ## some checks ##
  if (!is.gData(x)) stop("x is not a valid gData object")

  ## get connected sets (with more than one node) ##
  connected.sets <- .connectedSets(getGraph(x))

  ## define colors ##
  nbSets <- length(connected.sets)
  myNodes <- getNodes(x)
  ## set of each location (NA if in no set), and the relevant sets (those
  ## with a location), in the order of the sets
  set.id <- rep(seq_len(nbSets), lengths(connected.sets))[match(myNodes, unlist(connected.sets, use.names = FALSE))]
  relSets <- sort(unique(set.id[!is.na(set.id)]))
  nbRelSets <- length(relSets)

  if (!is.null(seed) && is.numeric(seed)) {
    set.seed(seed)
  }
  colSets <- sample(grDevices::rainbow(nbRelSets))

  col <- rep("lightgray", length(myNodes))
  names(col) <- myNodes

  ## colour of each location: that of its set
  toColor <- !is.na(set.id)
  col[toColor] <- colSets[match(set.id[toColor], relSets)]

  ## call to plot ##
  plot(x,
    col.gGraph = NA,
    col.nodes  = "black",
    pch.nodes  = 21,
    bg         = col,
    type       = "nodes",
    ...
  )

  ## fix last call ##
  curCall <- sys.call(-1)
  assign("last.plot", curCall, envir = .geoGraphEnv)

  return(invisible(col))
}) # end connectivityPlot gData
