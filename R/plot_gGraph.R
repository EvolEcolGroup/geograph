#' Plot a gGraph object.
#'
#' Various functions to plot a \linkS4class{gGraph} object: \code{plot} opens a
#' device and plot the object, while \code{points} plots the object on the
#' existing device. \code{plotEdges} only plots the edges of the graph: it can
#' be called directly, or via arguments passed to \code{plot} and
#' \code{points}.\cr
#'
#' Plotting of a gGraph object stores some parameters in R; see details for
#' more information.
#'
#' To be able to zoom in and out, or slide the window, previous plotting
#' information are stored in a particular environment (.geoGraphEnv), which is
#' created when loading \code{geoGraph}. Users should not have to interact
#' directly with objects in this environment.\cr
#'
#' The resulting plotting behavior is that when plotting a \code{gGraph}
#' object, last plotting parameters are re-used. To override this behavior,
#' specify \code{reset=TRUE} as argument to \code{plot}.
#' @name plot-gGraph
#' @aliases plot,gGraph-method plot,gGraph,missing-method plot.gGraph plot_gGraph
#' @docType methods
#' @param x a \linkS4class{gGraph} object.
#' @param shape a shapefile of the class `sf` (see
#' [sf::st_read()] to import a GIS
#' shapefile). Alternatively, a character string indicating one shapefile
#' released with geoGraph; currently, only 'world' is available.
#' @param psize a numeric giving the size of points.
#' @param pch a numeric or a character indicating the type of point.
#' @param col a character string indicating the color to be used.
#' @param edges a logical indicating if edges should be plotted (TRUE) or not
#' (FALSE).
#' @param reset a logical indicating if plotting parameters should be reset
#' (TRUE) or not (FALSE).
#' @param bg.col a character string indicating the color of the polygons of the
#' shapefile used as background.
#' @param border.col a character string indicating the color of the polygon
#' borders.
#' @param lwd a numeric indicating the width of line (used for edges).
#' @param useCosts a logical indicating if edge width should be inversely
#' proportional to edge cost (TRUE) or not (FALSE).
#' @param maxLwd a numeric indicating the maximum edge width (corresponding to
#' the maximum weight).
#' @param col.rules a data.frame with two named columns, the first one giving
#' values of a node attribute, and the second one stating colors to be used for
#' each value. If not provided, this is sought from the \code{@meta\$color}
#' slot of the object.
#' @param sticky.points a logical indicating if added points should be kept
#' when replotting (TRUE), or not (FALSE). In any case, \code{reset=TRUE} will
#' prevent points to be redrawn.
#' @param lty the type of line (for the edges).
#' @param pcol a character indicating the color to be used for points.
#' @param sticky.edges a logical indicating whether added edges should be kept
#' when replotting (TRUE), or not (FALSE, default). In any case,
#' \code{reset=TRUE} will prevent points to be redrawn.
#' @param \dots further arguments passed to the generic methods (plot, points,
#' and segments, respectively).
#' @return NULL.
#' @family plotting_methods


#' @seealso - Different functions to explore these plots:\cr
#' \code{\link{geo.zoomin}}, \code{\link{geo.zoomout}},
#' \code{\link{geo.slide}}, \code{\link{geo.back}}.\cr
#'
#' - \code{\link{isInArea}}, to retain a set of visible data.\cr
#' @keywords methods hplot spatial
#' @importFrom rnaturalearth ne_countries
#' @examples
#'
#'
#' ## just the background
#' plot(worldgraph.10k, reset = TRUE, type = "n")
#'
#' ## basic plot
#' plot(worldgraph.10k)
#'
#' ## zooming and adding edges
#' geo.zoomin(list(x = c(90, 150), y = c(0, -50)))
#' plot(worldgraph.10k, edges = TRUE)
#'
#'
#' ## display edges differently
#' plotEdges(worldgraph.10k, col = "red", lwd = 2)
#'
#'
#' ## replot points with different color
#' points(worldgraph.10k, col = "orange")
#'
#' ## mask points in the sea
#' inSea <- unlist(getNodesAttr(worldgraph.10k, attr.name = "habitat")) == "sea"
#' head(inSea)
#' points(worldgraph.10k[inSea], col = "white", sticky = TRUE) # this will stay
#'
#' ## but better, only draw those on land, and use a fancy setup
#' par(bg = "blue")
#' plot(worldgraph.10k[!inSea], bg.col = "darkgreen", col = "purple", edges = TRUE)
#'
#' @export
#' @import sf
setMethod(
  "plot", signature(x = "gGraph", y = "missing"),
  function(x, shape = "world", psize = NULL, pch = 19,
           col = NULL, edges = FALSE, reset = FALSE, bg.col = "gray",
           border.col = "dark gray", lwd = 1, useCosts = NULL,
           maxLwd = 3, col.rules = NULL, ...) {
    env <- .geoGraphEnv

    coords <- getCoords(x)


    ## store original parameters to be passed to last.plot.param ##
    pch.ori <- pch
    col.ori <- col

    ## handle reset ##
    if (reset) {
      assign("sticky.points", FALSE, envir = .geoGraphEnv)
      assign("last.points", expression(), envir = .geoGraphEnv)
    }

    ## handle xlim and ylim
    if ((!exists("zoom.log", envir = .geoGraphEnv)) | reset) { # if xlim absent or if reset
      temp <- c(range(coords[, 1]), range(coords[, 2]))
      .zoomlog.up(temp)
    }

    zoomlog <- get("zoom.log", envir = .geoGraphEnv)
    zoomlog <- zoomlog[1, ]

    xlim <- zoomlog[1:2]
    ylim <- zoomlog[3:4]

    ## handle zoom and psize
    if (is.null(psize)) {
      psize <- get("psize", envir = .geoGraphEnv)
    }

    ## handle color from attribute
    useAttrCol <- FALSE
    if (is.null(col.rules)) {
      if (!is.null(x@meta$colors)) {
        col.rules <- x@meta$colors
        useAttrCol <- TRUE
      }
    } else {
      useAttrCol <- TRUE
    }

    if (!is.null(col)) { # col overrides rules
      useAttrCol <- FALSE
    }


    toKeep <- isInArea(x, res.type = "integer", quiet = TRUE)
    coords <- coords[toKeep, ]


    ## store previous last.points in envir (is erased by plotEdges)
    if (exists("last.points", envir = .geoGraphEnv)) {
      last.points <- get("last.points", envir = .geoGraphEnv)
    } else {
      last.points <- expression()
    }


    ## handle colors
    if (useAttrCol) {
      col <- getColors(x, nodes = toKeep, attr.name = colnames(col.rules)[1], col.rules = col.rules)
    } else if (is.null(col.ori)) {
      col <- "red"
    } else {
      col <- rep(col.ori, length = length(getNodes(x)))
      names(col) <- getNodes(x)
      col <- col[toKeep]
    }


    ## handle shape
    if (!is.null(shape) && is.character(shape) && shape == "world") {
      # shape <- sf::st_read(system.file("files/shapefiles/world-countries.shp", package = "geoGraph"))
      shape <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
      sf::sf_use_s2(FALSE)
    }

    ## TODO if the shape is null, we should throw an error!!!
    if (!is.null(shape)) {
      if (!inherits(shape, "sf")) {
        if (inherits(shape, "SpatialPolygonsDataFrame")) {
          shape <- sf::st_as_sf(shape)
        } else {
          stop("shape must be a sf object \n(see st_read in sf to import such data from a GIS shapefile).")
        }
      }

      ## plot background
      plot(sf::st_geometry(shape), col = bg.col, border = border.col, xlim = xlim, ylim = ylim)

      ## subset of points in area
      toKeep <- isInArea(x, reg = "current", res.type = "character", quiet = TRUE)
      coords <- getCoords(x)[toKeep, ]

      ## define colors for these points
      if (useAttrCol) {
        col <- getColors(x, nodes = toKeep, attr.name = colnames(col.rules)[1], col.rules = col.rules)
      } else if (is.null(col.ori)) {
        col <- "red"
      } else {
        col <- rep(col.ori, length = length(getNodes(x)))
        names(col) <- getNodes(x)
        col <- col[toKeep]
      }


      if (edges) {
        ## plotEdges(x, replot=FALSE, lwd=lwd, useCosts=useCosts, maxLwd=maxLwd)
        plotEdges(x, lwd = lwd, useCosts = useCosts, maxLwd = maxLwd)
      }
      points(coords, cex = psize, pch = pch, col = col, ...)
    } else { ## plot only points ##
      plot(coords,
        xlab = "longitude", ylab = "latitude", xlim = xlim, ylim = ylim,
        cex = psize, pch = pch, col = col, ...
      )
      if (edges) {
        ##       plotEdges(x, replot=TRUE, psize=psize, pch=pch, pcol=col, lwd=lwd,
        ##            useCosts=useCosts, maxLwd=maxLwd)
        plotEdges(x,
          psize = psize, pch = pch, pcol = col, lwd = lwd,
          useCosts = useCosts, maxLwd = maxLwd
        )
      }
    }


    ## misc assignements in our dedicated environment
    assign("usr", graphics::par("usr"), envir = .geoGraphEnv)

    curCall <- sys.call(-1)
    assign("last.plot", curCall, envir = .geoGraphEnv)
    temp <- get("last.plot.param", envir = .geoGraphEnv)
    temp$psize <- psize
    temp$pch <- pch.ori
    temp$col <- col.ori
    assign("last.plot.param", temp, envir = .geoGraphEnv)

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
## points for gGraph
#####################
#' @rdname plot-gGraph
#' @export
setMethod("points", signature("gGraph"), function(x, psize = NULL, pch = NULL, col = NULL,
                                                  edges = FALSE, lwd = 1, useCosts = NULL, maxLwd = 3, col.rules = NULL,
                                                  sticky.points = FALSE, ...) {
  ## some checks
  if (!is.gGraph(x)) stop("x is not a valid gGraph object")

  ## create the .geoGraphEnv if it does not exist
  # if(!exists(".geoGraphEnv", envir=.GlobalEnv)) {
  #     assign(".geoGraphEnv",  new.env(parent=.GlobalEnv), envir=.GlobalEnv)
  #     warning(".geoGraphEnv was not present, which may indicate a problem in loading geoGraph.")
  # }

  # env <- get(".geoGraphEnv", envir=.GlobalEnv) # env is our target environnement

  zoomlog <- get("zoom.log", envir = .geoGraphEnv)
  zoomlog <- zoomlog[1, ]

  xlim <- zoomlog[1:2]
  ylim <- zoomlog[3:4]

  ## store original parameters to be passed to last.plot.param ##
  pch.ori <- pch
  col.ori <- col


  ## subset data to visible area ##
  coords <- getCoords(x)
  toKeep <- isInArea(x, reg = "current", res.type = "integer", quiet = TRUE)
  coords <- coords[toKeep, , drop = FALSE]

  ## handle plot param
  last.plot.param <- get("last.plot.param", envir = .geoGraphEnv)
  if (is.null(psize)) psize <- last.plot.param$psize
  if (is.null(pch)) pch <- last.plot.param$pch


  ## handle color from attribute
  useAttrCol <- FALSE
  if (is.null(col.rules)) {
    if (!is.null(x@meta$colors)) {
      col.rules <- x@meta$colors
      useAttrCol <- TRUE
    }
  } else {
    useAttrCol <- TRUE
  }

  if (!is.null(col)) { # col overrides rules
    useAttrCol <- FALSE
  }


  ## handle color
  if (useAttrCol) {
    col <- getColors(x, nodes = toKeep, attr.name = colnames(col.rules)[1], col.rules = col.rules)
  } else if (is.null(col.ori)) {
    col <- "red"
  } else {
    col <- rep(col.ori, length = length(getNodes(x)))
    names(col) <- getNodes(x)
    col <- col[toKeep]
  }


  ## define colors for these points
  if (useAttrCol) {
    col <- getColors(x, nodes = toKeep, attr.name = colnames(col.rules)[1], col.rules = col.rules)
  } else if (is.null(col)) {
    col <- "red"
  } else {
    col <- rep(col, length = length(getNodes(x)))
    names(col) <- getNodes(x)
    col <- col[toKeep]
  } # end handle color


  ## handle zoom and psize
  if (is.null(psize)) {
    psize <- get("psize", envir = .geoGraphEnv)
  }


  ## add only points and optionally edges
  if (edges) {
    ## plotEdges(x, replot=FALSE, lwd=lwd, useCosts=useCosts, maxLwd=maxLwd)
    plotEdges(x, lwd = lwd, useCosts = useCosts, maxLwd = maxLwd)
  }
  points(coords,
    xlab = "longitude", ylab = "latitude", xlim = xlim, ylim = ylim,
    cex = psize, pch = pch, col = col, ...
  )


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
}) # end points method gGraph


############
## plotEdges
############
#' @rdname plot-gGraph
#' @export
plotEdges <- function(x, useCosts = NULL, col = "black", lwd = 1,
                      lty = 1, pch = NULL, psize = NULL, pcol = NULL, maxLwd = 3, col.rules = NULL,
                      sticky.edges = FALSE, ...) {
  ## some checks
  if (!is.gGraph(x)) stop("x is not a valid gGraph object.")

  ## handle weights for edges
  if (is.null(useCosts)) {
    useCosts <- hasCosts(x)
  }

  ## get the environment
  # env <- get(".geoGraphEnv", envir=.GlobalEnv)


  if (exists("last.points", envir = .geoGraphEnv)) {
    last.points <- get("last.points", envir = .geoGraphEnv)
  } else {
    last.points <- expression()
  }

  ## retained coords (those within plotting area)
  coords <- getCoords(x)
  toKeep <- isInArea(x, reg = "current", res.type = "integer", quiet = TRUE)
  keptCoords <- coords[toKeep, , drop = FALSE]

  ## adjust pcol to subset of points in area

  if (is.null(pcol)) {
    ## handle color from attribute
    useAttrCol <- FALSE
    if (is.null(col.rules)) {
      if (!is.null(x@meta$colors)) {
        col.rules <- x@meta$colors
        useAttrCol <- TRUE
      }
    } else {
      useAttrCol <- TRUE
    }

    if (!is.null(pcol)) { # pcol overrides color by attribute
      useAttrCol <- FALSE
      pcol <- pcol[toKeep]
    }

    if (useAttrCol) {
      if (is.null(col.rules)) {
        col.rules <- colnames(x@meta$colors)[1] # default attribute used for colors
      }

      pcol <- getColors(x, nodes = toKeep, attr.name = colnames(col.rules)[1], col.rules = col.rules)
    } else {
      pcol <- "black"
    } # end handle pcol
  }


  edges <- getEdges(x, res.type = "matNames", unique = TRUE) # retrieve (unique) edges
  temp <- (edges[, 1] %in% rownames(keptCoords)) & (edges[, 2] %in% rownames(keptCoords))
  keptEdges <- edges[temp, ]

  if (nrow(keptEdges) < 1) {
    cat("\nNo edge to plot.\n")
    return(invisible())
  }

  ## handle costs
  if (useCosts) {
    edges.w <- getCosts(x, res.type = "vector", unique = TRUE)
    edges.w <- edges.w[temp]
    lwd <- edges.w / max(edges.w) # max lwd = 1
    lwd <- 1 - lwd # invert scale (to have thiner edges for larger costs)
    lwd <- lwd * maxLwd # max lwd = maxLwd
    lty <- rep(1, length(lwd)) # make a lty vector
    lty[lwd < 1e-5] <- 3 # assign 3 (doted line) to dead edges.
  }

  ## plot segments
  idx1 <- match(as.character(keptEdges[, 1]), rownames(keptCoords))
  idx2 <- match(as.character(keptEdges[, 2]), rownames(keptCoords))

  graphics::segments(keptCoords[idx1, 1], keptCoords[idx1, 2],
    keptCoords[idx2, 1], keptCoords[idx2, 2],
    col = col, lwd = lwd, lty = lty, ...
  )


  ## replot points
  eval(last.points)


  ## if sticky edges are used, store info in env ##
  if (sticky.edges) {
    ## curCall <- sys.call(-1) # does not work as plotEdges is not a S4 method
    curCall <- match.call()
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
} # end plotEdges
