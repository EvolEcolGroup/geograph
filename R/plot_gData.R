#####################
## plot for gData
#####################
#' Plot a gData object.
#'
#' Various functions to plot a \linkS4class{gData} object: \code{plot} opens a
#' device and plots the object, while \code{points} plots the object on the
#' existing device. Plotting of \linkS4class{gData} object relies on plotting
#' the \linkS4class{gGraph} object to which it is linked, and then represent
#' the locations of the \linkS4class{gData} and/or the associated nodes.
#'
#' When \code{sticky.points} is set to TRUE, all operations performed on the
#' graphics like zooming or sliding the window can be performed without loosing
#' the \code{gData} plot.\cr
#'
#' @name plot-gData
#' @aliases plot,gData,missing-method plot.gData plot_gData
#' @param x a valid \linkS4class{gData} object. The \linkS4class{gData} object
#' to which it is linked must exist in the global environment.
#' @param type a character string indicating which information should be
#' plotted: original locations ('original'), associated nodes ('nodes',
#' default), or both ('both'). In the latter case, an arrow goes from locations
#' to nodes.
#' @param pch.ori a numeric or a character indicating the type of point for
#' locations.
#' @param pch.nodes a numeric or a character indicating the type of point for
#' nodes.
#' @param col.ori a character string indicating the color to be used for
#' locations.
#' @param col.nodes a character string indicating the color to be used for
#' nodes.
#' @param col.gGraph a (recycled) color vector for the associated
#' \linkS4class{gGraph} object. If NULL, default color is used. Set to
#' \code{NA} or "transparent" to avoid plotting the \linkS4class{gGraph}.
#' @param reset a logical stating whether the plotting area should be reset to
#' fit the \code{gData} object (TRUE), or should conserve previous plotting and
#' settings (FALSE, default).
#' @param sticky.points a logical indicating if added points should be kept
#' when replotting (TRUE, default), or not (FALSE). In any case,
#' \code{reset=TRUE} will prevent points to be redrawn.
#' @param \dots further arguments passed to \code{points}.
#' @return NULL.
#' @family plotting_methods


#' @seealso - Different functions to explore these plots:\cr
#' \code{\link{geo.zoomin}}, \code{\link{geo.zoomout}},
#' \code{\link{geo.slide}}, \code{\link{geo.back}}, \code{\link{geo.bookmark}},
#' \code{\link{geo.goto}}.\cr
#' @keywords methods hplot spatial
#' @export
#' @examples
#'
#'
#' myLoc <- list(x = c(3, -8, 11, 28), y = c(50, 57, 71, 67)) # some locations
#' obj <- new("gData", coords = myLoc) # new gData object
#' obj
#'
#' obj@gGraph.name <- "worldgraph.10k"
#' obj <- closestNode(obj, attr.name = "habitat", attr.value = "land")
#'
#' ## plot the result (original location -> assigned node)
#' plot(obj, type = "both", reset = TRUE)
#' title("'x'=location, 'o'=assigned node")
#'
#' ## using different parameters
#' points(obj, type = "both", pch.ori = 2, col.ori = "red", pch.nodes = 20, col.nodes = "pink")
#'
#' ## only nodes, fancy plot
#' plot(obj, col.nodes = "red", cex = 1, pch.node = 20)
#' points(obj, col.nodes = "red", cex = 2)
#' points(obj, col.nodes = "orange", cex = 3)
#' points(obj, col.nodes = "yellow", cex = 4)
#'
setMethod(
  "plot", signature(x = "gData", y = "missing"),
  function(x, type = c("nodes", "original", "both"),
           pch.ori = 4, pch.nodes = 1,
           col.ori = "black", col.nodes = "red",
           col.gGraph = NULL,
           reset = FALSE, sticky.points = TRUE, ...) {
    # TODO check if we need y = missing , or can we remove it (same for plotting for gGraph)


    ## some checks
    if (!is.gData(x)) stop("x is not a valid gData object")
    type <- match.arg(type)

    ## get the environment
    #    env <- get(".geoGraphEnv", envir=.GlobalEnv)
    env <- .geoGraphEnv

    if (!exists(x@gGraph.name, envir = .GlobalEnv)) { # if the gGraph is missing, stop
      stop(paste("The gGraph object", x@gGraph.name, "is missing."))
    }

    myGraph <- get(x@gGraph.name, envir = .GlobalEnv) # get the gGraph object

    if ((type %in% c("nodes", "both")) & (length(x@nodes.id) == 0)) { # no nodes assigned
      stop("Locations are not assigned to nodes (x@nodes.id is empty).")
    }


    ## cleaning if required ##
    if (reset) {
      assign("sticky.points", FALSE, envir = .geoGraphEnv) # remove possible sticky points
      assign("last.points", expression(), envir = .geoGraphEnv) # remove possible sticky points
    }

    ## define visible area if reset ##
    if ((!exists("zoom.log", envir = .geoGraphEnv)) | reset) {
      loc <- getCoords(x)
      coords.nodes <- getCoords(myGraph)[x@nodes.id, , drop = FALSE]
      temp <- rbind(loc, coords.nodes)
      myRegion <- as.vector(apply(temp, 2, range)) # return xmin, xmax, ymin, ymax
      .zoomlog.up(myRegion) # define new window limits
    }

    zoomlog <- get("zoom.log", envir = .geoGraphEnv)
    zoomlog <- zoomlog[1, ]

    xlim <- zoomlog[1:2]
    ylim <- zoomlog[3:4]


    ## plot the gGraph object ##
    plot(myGraph, col = col.gGraph)


    ## call to points ##
    ## store previous last.points in envir (is erased by points)
    if (exists("last.points", envir = .geoGraphEnv)) {
      last.points <- get("last.points", envir = .geoGraphEnv)
    } else {
      last.points <- expression()
    }

    points(x,
      type = type,
      pch.ori = pch.ori, pch.nodes = pch.nodes, col.ori = col.ori,
      col.nodes = col.nodes, sticky.points = sticky.points, ...
    )


    ## some assignments
    curCall <- sys.call(-1)
    assign("last.plot", curCall, envir = .geoGraphEnv)
    ## must re-assign the last call to points in envir.
    assign("last.points", last.points, envir = .geoGraphEnv)

    ## add previously added points if needed ##
    sticky.points <- get("sticky.points", envir = .geoGraphEnv)
    if (sticky.points) {
      temp <- get("last.points", envir = .geoGraphEnv) # this may be a list of calls
      invisible(lapply(temp, eval))
    }

    return(invisible())
  }
) # end plot method


#####################
## points for gData
#####################
#' @export
#' @describeIn plot-gData Plot as points
setMethod("points", signature(x = "gData"), function(x, type = c("nodes", "original", "both"),
                                                     pch.ori = 4, pch.nodes = 1,
                                                     col.ori = "black", col.nodes = "red",
                                                     sticky.points = TRUE, ...) {
  ## some checks
  if (!is.gData(x)) stop("x is not a valid gData object")
  type <- match.arg(type)

  ## subset data to visible area ##
  coords.ori <- getCoords(x)
  if (type %in% c("nodes", "both")) { # need to get coords of nodes
    if (!exists(x@gGraph.name, envir = .GlobalEnv)) { # if the gGraph is missing, stop
      stop(paste("The gGraph object", x@gGraph.name, "is missing."))
    }

    if (length(x@nodes.id) == 0) { # if nodes have not been assigned, stop
      stop("No nodes are assigned (@nodes.id empty); nothing to plot.")
    }

    myGraph <- get(x@gGraph.name, envir = .GlobalEnv)
    coords.nodes <- getCoords(myGraph)[x@nodes.id, , drop = FALSE]
  }

  ## add points ##
  if (type == "original" | type == "both") { # plot original coordinates
    points(coords.ori[, 1], coords.ori[, 2], pch = pch.ori, col = col.ori, ...)
  }

  if (type == "nodes" | type == "both") { # plot assigned nodes
    points(coords.nodes[, 1], coords.nodes[, 2], pch = pch.nodes, col = col.nodes, ...)
  }

  if (type == "both") { # add arrows from original location to assigned node
    graphics::arrows(coords.ori[, 1], coords.ori[, 2], coords.nodes[, 1], coords.nodes[, 2], angle = 15, length = .1)
  }

  ## if sticky points are used, store info in env ##
  if (sticky.points) {
    curCall <- sys.call(-1)
    temp <- get("last.points", envir = .geoGraphEnv) # might be a single expression or a list of expressions
    if (!is.list(temp)) {
      temp <- list(temp) # make sure it is a list
    }
    ## do not add an existing expression ##
    existExp <- any(sapply(temp, identical, curCall))
    if (!existExp) {
      temp[[length(temp) + 1]] <- curCall
      assign("last.points", temp, envir = .geoGraphEnv)
    }
    assign("sticky.points", TRUE, envir = .geoGraphEnv)
  }

  return(invisible())
}) # end points for gData
