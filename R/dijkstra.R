#' Shortest path using Dijkstra algorithm
#'
#' The methods \code{dijkstraFrom} and \code{dijkstraBetween} are wrappers of
#' procedures implemented in RBGL package, designed for \linkS4class{gGraph}
#' and \linkS4class{gData} object.\cr
#'
#' \code{dijkstraFrom} finds minimum costs paths to nodes from a given 'source'
#' node.\cr
#'
#' \code{dijkstraBetween} finds minimum costs paths between all possible pairs
#' of nodes given two sets of nodes.\cr
#'
#' All these functions return objects with S3 class "gPath". These objects can
#' be plotted using \code{plot.gPath}.
#'
#' \code{gPath2dist} extracts the pairwise distances from the \code{gPath}
#' returned by \code{dijkstraBetween} and returns a \code{dist} object. Note
#' that if the \code{gPath} does not contain pairwise information, a warning
#' will be issued, but the resulting output will likely be meaningless.\cr
#'
#' In 'dijkstraBetween', paths are sought all possible pairs of nodes between
#' 'from' and 'to'.
#'
#' @name dijkstra-methods
#' @aliases dijkstraFrom dijkstraFrom-methods dijkstraFrom,gData-method
#' dijkstraFrom,gGraph-method dijkstraBetween dijkstraBetween-methods
#' dijkstraBetween,gData-method dijkstraBetween,gGraph-method gPath2dist gPath
#' plot.gPath
#' @docType methods
#' @param x a \linkS4class{gGraph} or a \linkS4class{gData} object. For
#' plotting method of \code{gPath} objects, a \code{gPath} object.
#' @param start a character string naming the 'source' node.
#' @param from a vector of character strings giving node names.
#' @param to a vector of character strings giving node names.
#' @param col a character string indicating a color or a palette of colors to
#' be used for plotting edges.
#' @param lwd a numeric value indicating the width of edges.
#' @param m a \code{gPath} object obtained by \code{dijkstraBetween}.
#' @param diag,upper unused parameters added for consistency with
#' \code{as.dist}.
#' @param res.type a character string indicating what type of result should be
#' returned: a \code{dist} object ('dist'), or a vector of distances
#' ('vector'). Note that 'dist' should only be required for pairwise data, as
#' output by dijkstraBetween (as opposed to dijkstraFrom).
#' @param \dots further arguments passed to the \code{segments} method.
#' @return A "gPath" object. These are basically the outputs of RBGL's
#' \code{sp.between} function (see \code{?sp.between}), with a class attribute
#' set to "gPath", and an additional slot 'xy' containing geographic
#' coordinates of the nodes involved in the paths.\cr

#' @keywords methods spatial
#' @examples
#' \dontrun{
#'
#' ## plotting
#' world <- worldgraph.40k
#' par(mar = rep(.1, 4))
#' plot(world, reset = TRUE)
#'
#' ## check connectivity
#' isConnected(hgdp) # must be ok
#'
#' ## Lowest cost path from an hypothetical origin
#' ori.coord <- list(33, 10) # one given location long/lat
#' points(data.frame(ori.coord), pch = "x", col = "black", cex = 3) # an 'x' shows the putative origin
#' ori <- closestNode(world, ori.coord) # assign it the closest node
#'
#' myPath <- dijkstraFrom(hgdp, ori) # compute shortest path
#'
#' ## plotting
#' plot(world, pch = "") # plot the world
#' points(hgdp, lwd = 3) # plot populations
#' points(data.frame(ori.coord), pch = "x", col = "black", cex = 3) # add origin
#' plot(myPath) # plot the path
#' }
#'
NULL



###################
## dijkstraBetween
###################
#' @rdname dijkstra-methods
#' @export
setGeneric("dijkstraBetween", function(x, ...) {
  standardGeneric("dijkstraBetween")
})






#####################
## method for gGraph
#####################
#' @rdname dijkstra-methods
#' @export
setMethod("dijkstraBetween", "gGraph", function(x, from, to) {
  ## some checks ##
  if (!require(RBGL)) stop("RBGL is required.")
  if (!is.gGraph(x)) stop("x is not a valid gGraph object")
  if (!all(from %in% getNodes(x))) stop("Some starting nodes are not in x.")
  if (!all(to %in% getNodes(x))) stop("Some ending nodes are not in x.")

  ## check connectivity ##
  if (!areConnected(x, unique(c(from, to)))) stop("Not all nodes are connected by the graph.")

  ## build the wrapper ##
  myGraph <- getGraph(x)

  ## recycle from and to
  maxLength <- max(length(from), length(to))
  from <- rep(from, length = maxLength)
  to <- rep(to, length = maxLength)

  ## build indices of all pairwise combinations ##
  if (maxLength > 1) {
    pairIdStart <- integer()
    pairIdStop <- integer()

    for (i in 1:maxLength) {
      j <- i
      while ((j <- j + 1) < (maxLength + 1)) {
        pairIdStart <- c(pairIdStart, i)
        pairIdStop <- c(pairIdStop, j)
      }
    }
  } else {
    pairIdStart <- pairIdStop <- 1
  }

  ## wrap ##
  ## ! sp.between does not return duplicated paths
  res <- RBGL::sp.between(myGraph, start = from[pairIdStart], finish = to[pairIdStop])


  ## handle duplicated paths ##
  if (length(res) < maxLength) { # res should have length = laxLength
    fromTo <- paste(from[pairIdStart], to[pairIdStop], sep = ":") # all different paths
    res <- res[fromTo]
  }


  ## make it a class "gPath" (output + xy coords) ##
  allNodes <- unique(unlist(lapply(res, function(e) e$path_detail)))
  ## res$xy <- getCoords(x)[allNodes,]
  attr(res, "xy") <- getCoords(x)[allNodes, ]
  class(res) <- "gPath"

  return(res)
}) # end dijkstraBetween for gGraph






#####################
## method for gData
#####################
#' @rdname dijkstra-methods
#' @export
setMethod("dijkstraBetween", "gData", function(x) {
  ## temp <- function(x){ # for debugging

  ## some checks ##
  if (!require(RBGL)) stop("RBGL is required.")
  if (!is.gData(x)) stop("x is not a valid gData object")
  if (!exists(x@gGraph.name, envir = .GlobalEnv)) stop(paste("gGraph object", x@gGraph.name, "not found."))
  if (length(x@nodes.id) == 0) stop("No assigned nodes (x@nodes.id is empty).")
  if (!isConnected(x)) stop("Not all locations are connected by the graph.")

  ## build the wrapper ##
  myGraph <- get(x@gGraph.name, envir = .GlobalEnv)
  coords <- getCoords(myGraph) # store xy coords for later
  myGraph <- getGraph(myGraph) # don't do this before getCoords

  ## build indices of all pairwise combinations ##
  pairIdStart <- integer()
  pairIdStop <- integer()

  for (i in 1:(length(getNodes(x)) + 1)) {
    j <- i
    while ((j <- j + 1) < length(getNodes(x)) + 1) {
      pairIdStart <- c(pairIdStart, i)
      pairIdStop <- c(pairIdStop, j)
    }
  }

  ## wrap ##
  ## ! sp.between does not return duplicated paths
  res <- RBGL::sp.between(myGraph, start = x@nodes.id[pairIdStart], finish = x@nodes.id[pairIdStop])


  ## handle duplicated paths ##
  if (length(res) < length(pairIdStart)) { # res should have length = pairIdStart
    fromTo <- paste(x@nodes.id[pairIdStart], x@nodes.id[pairIdStop], sep = ":") # all different paths
    res <- res[fromTo]
  }


  ## make it a class "gPath" (output + xy coords) ##
  allNodes <- unique(unlist(lapply(res, function(e) e$path_detail)))
  ## res$xy <- getCoords(x)[allNodes,]
  attr(res, "xy") <- coords[allNodes, ]
  class(res) <- "gPath"

  return(res)
}) # end dijkstraBetween for gData






######################################
######################################






################
## dijkstraFrom
################
#' @rdname dijkstra-methods
#' @export
setGeneric("dijkstraFrom", function(x, ...) {
  standardGeneric("dijkstraFrom")
})






#####################
## method for gGraph
#####################
#' @rdname dijkstra-methods
#' @export
setMethod("dijkstraFrom", "gGraph", function(x, start) {
  ## some checks ##
  if (!require(RBGL)) stop("RBGL is required.")
  if (!is.gGraph(x)) stop("x is not a valid gGraph object")
  if (!all(start %in% getNodes(x))) stop("Starting node is not in x.")

  ## check connectivity ##
  if (!areConnected(x, getNodes(myGraph))) stop("Not all nodes are connected by the graph.")

  ## build the wrapper ##
  myGraph <- getGraph(x)
  ##  if(is.character(costs) && costs=="default"){
  ##         costs <- unlist(edgeWeights(myGraph))
  ##     }

  ## wrap ##
  res <- RBGL::dijkstra.sp(myGraph, start = start)

  ## sp.between uses unique(x@nodes.id) ##
  ## eventually have to duplicate paths ##
  temp <- gsub(".*:", "", names(res))
  res <- res[match(getNodes(x), temp)]


  ## make it a class "gPath" (output + xy coords) ##
  allNodes <- unique(unlist(lapply(res, function(e) e$path_detail)))
  ## res$xy <- getCoords(x)[allNodes,]
  attr(res, "xy") <- getCoords(x)[allNodes, ]
  class(res) <- "gPath"

  return(res)
}) # end dijkstraFrom for gGraph






####################
## method for gData
####################
#' @rdname dijkstra-methods
#' @export
setMethod("dijkstraFrom", "gData", function(x, start) {
  ## some checks ##
  if (!require(RBGL)) stop("RBGL is required.")
  if (!is.gData(x)) stop("x is not a valid gData object")
  if (!exists(x@gGraph.name, envir = .GlobalEnv)) stop(paste("gGraph object", x@gGraph.name, "not found."))
  if (length(x@nodes.id) == 0) stop("No assigned nodes (x@nodes.id is empty).")
  if (!isConnected(x)) stop("Not all locations are connected by the graph")


  ## build the wrapper ##
  myGraph <- get(x@gGraph.name, envir = .GlobalEnv) # myGraph is a gGraph object
  coords <- getCoords(myGraph) # store xy for later
  myGraph <- getGraph(myGraph)

  ##  if(is.character(weights) && weights=="default"){ # no longer used
  ##         weights <- unlist(edgeWeights(myGraph))
  ##     }


  ## wrap ##
  res <- RBGL::sp.between(myGraph, start = start, finish = x@nodes.id)


  ## sp.between uses unique(x@nodes.id) ##
  ## eventually have to duplicate paths ##
  temp <- gsub(".*:", "", names(res))
  res <- res[match(getNodes(x), temp)]


  ## make it a class "gPath" (output + xy coords) ##
  allNodes <- unique(unlist(lapply(res, function(e) e$path_detail)))
  ## res$xy <- getCoords(x)[allNodes,]
  attr(res, "xy") <- coords[allNodes, ]
  class(res) <- "gPath"

  return(res)
}) # end dijkstraFrom for gData







######################################
######################################






#################
## plot methods
#################
#' @rdname dijkstra-methods
#' @method plot gPath
#' @export
plot.gPath <- function(x, col = "rainbow", lwd = 3, ...) {
  ## listNodes <- lapply(x[-length(x)], function(e) e$path_detail)
  listNodes <- lapply(x, function(e) e$path_detail)

  ## xy <- x$xy
  xy <- attr(x, "xy")
  Npath <- length(listNodes)

  ## handle color ##
  if (is.character(col) && col[1] == "rainbow") {
    col <- sample(grDevices::rainbow(length(x)))
  }
  col <- rep(col, length = Npath)
  lwd <- rep(lwd, length = Npath)


  ## function plotting one gPath
  f1 <- function(vecNodes, col, lwd, ...) {
    N <- length(vecNodes)
    if (N < 2) {
      return()
    } # escape if a path is a single vertex
    from <- vecNodes[1:(N - 1)]
    to <- vecNodes[2:N]
    ## segments(xy[from,1], xy[from,2], xy[to,1], xy[to,2], col=col, lwd=lwd, ...)
    geo.segments(xy[from, 1], xy[from, 2], xy[to, 1], xy[to, 2], col = col, lwd = lwd, ...)
  }


  ## plot all gPaths
  lapply(1:length(listNodes), function(i) f1(listNodes[[i]], col = col[i], lwd = lwd[i], ...))

  return(invisible())
} # end plot.gPath










######################################
######################################

##
## CONVERSION gPath -> distance
##

#' @rdname dijkstra-methods
#' @export
gPath2dist <- function(m, diag = FALSE, upper = FALSE, res.type = c("dist", "vector")) {
  ## find the size of the dist object ##
  x <- m
  res.type <- match.arg(res.type)
  L <- length(x)
  x.names <- sub(":.*", "", names(x))
  i <- 1
  while (x.names[i] == x.names[i + 1] && i < L) {
    i <- i + 1
  }

  resSize <- i + 1

  ## check size consistency
  if (L != (resSize * (resSize - 1) * 0.5)) {
    if (res.type == "dist") {
      warning("Length of x does not match a number of pairwise comparisons.")
    }
  }


  ## GET DISTANCES ##
  resDist <- sapply(x, function(e) sum(e$length_detail[[1]], na.rm = TRUE))


  ## BUILD RESULT ##
  ## type == dist
  if (res.type == "dist") {
    res <- stats::dist(1:resSize)
    res[] <- resDist
  } else {
    ## type == vector (no change)
    res <- resDist
  }

  return(res)
} # end gPath2dist
