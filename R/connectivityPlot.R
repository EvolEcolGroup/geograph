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

  ## get connected sets ##
  connected.sets <- RBGL::connectedComp(getGraph(x))

  ## just keep sets > 1 node
  temp <- sapply(connected.sets, length)
  reOrd <- order(temp, decreasing = TRUE) # sets ordered in decreasing size
  temp <- temp[reOrd]
  if (min(temp) == 1) {
    connected.sets <- connected.sets[reOrd][seq_len(which.min(temp) - 1)]
  }

  names(connected.sets) <- paste("set", seq_along(connected.sets), recycle0 = TRUE)

  ## define colors ##
  nbSets <- length(connected.sets)
  if (!is.null(seed) && is.numeric(seed)) {
    set.seed(seed)
  }

  colSets <- sample(grDevices::rainbow(nbSets))

  myNodes <- getNodes(x)
  col <- rep("lightgray", length(myNodes))
  names(col) <- myNodes

  for (i in seq_len(nbSets)) {
    e <- connected.sets[[i]] # 'e' is a vector of connected nodes
    col[e] <- colSets[i]
  }

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

  ## get connected sets ##
  connected.sets <- RBGL::connectedComp(getGraph(x))

  ## just keep sets > 1 node
  temp <- sapply(connected.sets, length)
  reOrd <- order(temp, decreasing = TRUE) # sets ordered in decreasing size
  temp <- temp[reOrd]
  if (min(temp) == 1) {
    connected.sets <- connected.sets[reOrd][seq_len(which.min(temp) - 1)]
  }

  names(connected.sets) <- paste("set", seq_along(connected.sets), recycle0 = TRUE)

  ## define colors ##
  nbSets <- length(connected.sets)
  ## find the number of relevant sets
  nbRelSets <- 0
  myNodes <- getNodes(x)

  for (i in seq_len(nbSets)) {
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

  color.idx <- 0
  for (i in seq_len(nbSets)) {
    e <- connected.sets[[i]]
    col[names(col) %in% e] <- colSets[i]
    nodes.in.set <- names(col)[names(col) %in% e]
    if (length(nodes.in.set) > 0) {
      color.idx <- color.idx + 1
      col[nodes.in.set] <- colSets[color.idx]
    }
  }

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
