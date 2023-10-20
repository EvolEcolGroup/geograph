#' Navigate in the plot of a gGraph object
#'
#' The functions \code{geo.zoomin}, \code{geo.zoomout}, \code{geo.slide},
#' \code{geo.back}, \code{geo.bookmark} and \code{geo.goto} are used to
#' navigate interactively in the plot of a \linkS4class{gGraph} object.
#'
#' \code{geo.zoomin} and \code{geo.zoomout} are used to zoom in and out. For
#' zooming in, the user has to delimit the opposite corner of the new plotting
#' area; alternatively, a set of coordinates can be provided. For zooming out,
#' each click on the screen will zoom out further.\cr
#'
#' \code{geo.slide} moves the window toward the direction indicated by clicking
#' in the screen.\cr
#'
#' \code{geo.back} redraws previous plots each time screen is clicked.\cr
#'
#' \code{geo.bookmark} sets a bookmark for the current area. If the name for
#' the bookmark is left to NULL, then the list of currently available bookmarks
#' is returned.\cr
#'
#' \code{geo.goto} allows the user to get back to a bookmarked area.\cr
#'
#' \code{.zoomlog.up} is an auxiliary function used to update the zoom log, by
#' providing new sets of coordinates.
#'
#' Whenever clicking is needed, a right-click will stop the function.
#'
#'
#' @aliases geo.zoomin geo.zoomout geo.slide geo.back geo.bookmark geo.goto
#' .zoomlog.up
#' @param reg a list of length 2, with its first component being the new x
#' (longitude) boundaries (a vector of length 2), and its second being new y
#' (latitude) boundaries (a vector of length 2).
#' @param vec a numeric vector of length 4 giving the new coordinates of the
#' plotting window, in the order: xmin, xmax, ymin, ymax.
#' @param name a character string giving the name of the bookmark to create (in
#' \code{geo.bookmark}) or to get back to (in \code{geo.goto}).
#' @seealso \code{\link{plot.gGraph}} for plotting of a \linkS4class{gGraph}
#' object.
#' @keywords utilities hplot
#' @name zoom
#' @examples
#'
#' plot(worldgraph.10k, reset = TRUE)
#'
#' ## zooming in
#' x.ini <- c(-100, -60)
#' y.ini <- c(-30, 30)
#' for (i in 0:3) {
#'   geo.zoomin(list(x = x.ini + i * 60, y = y.ini))
#' }
#'
#' \dontrun{
#' ## going back
#' geo.back() # you have to click !
#'
#' ## zooming in interactively
#' geo.zoomin() # you have to click !
#'
#' ## zooming out
#' geo.zoomout() # you have to click !
#'
#' ## moving window
#' geo.slide() # you have to click !
#' }
#'
NULL




###############
## .zoomlog.up
###############
#' @export
.zoomlog.up <- function(vec) { # vec is xmin, xmax, ymin, ymax
  if (!is.vector(vec) || length(vec) != 4 || !is.numeric(vec)) stop("Updating zoomlog using a wrong value.")

  #    geoEnv <- get(".geoGraphEnv", envir=.GlobalEnv)
  oldZoomLog <- get("zoom.log", envir = .geoGraphEnv)
  newZoomLog <- rbind(vec, oldZoomLog)
  colnames(newZoomLog) <- colnames(oldZoomLog)

  if (nrow(newZoomLog) > 100) {
    newZoomLog <- newZoomLog[1:100, ]
  }
  assign("zoom.log", newZoomLog, envir = .geoGraphEnv)


  return(invisible())
}





##############
## geo.zoomin
##############


#' @export
geo.zoomin <- function(reg = NULL) { # reg should be a list as returned by locator()
  ## a few checks
  if (is.list(reg)) {
    names(reg) <- c("x", "y")
  }

  if (is.numeric(reg) && length(reg) == 4) { # is reg is a vector: x1, x2, y1, y2
    temp <- reg
    reg <- list(x = temp[1:2], y = temp[3:4])
  }

  ## get environment
  # geoEnv <- get(".geoGraphEnv", envir=.GlobalEnv)


  ## get last plot
  last.plot.call <- get("last.plot", envir = .geoGraphEnv)


  ## reg provided => no loop ##
  if (!is.null(reg)) {
    ## define new xlim and ylim
    if (!is.list(reg) || length(reg) != 2) stop("Wrong reg specified.")
    reg <- lapply(reg, sort)

    ## make it a square
    reg.size <- max(diff(reg[[1]]), diff(reg[[2]])) # largest edge of rectangle
    reg.cen <- unlist(lapply(reg, mean)) # center of the rectangle
    reg[[1]][1] <- reg.cen[1] - reg.size / 2 # new x1
    reg[[1]][2] <- reg.cen[1] + reg.size / 2 # new x2
    reg[[2]][1] <- reg.cen[2] - reg.size / 2 # new y1
    reg[[2]][2] <- reg.cen[2] + reg.size / 2 # new y2

    .zoomlog.up(c(reg$x, reg$y))

    ## reconstruct a valid call to plot
    temp <- deparse(last.plot.call)
    temp <- sub("reset[^,]*,", "", temp) # remove subset if provided
    temp <- sub(",[[:blank:]]*reset[^)]*", "", temp) # same thing, if last arg

    ##     temp <- sub("ylim[^,]*,","",temp) # idem, ylim
    ##     temp <- sub(")$","",temp) # idem, ylim
    ##     temp <- paste(temp, ", xlim = c(", reg$x[1], ",", reg$x[2],")")
    ##     temp <- paste(temp, ", ylim = c(", reg$y[1], ",", reg$y[2],")")
    ##     temp <- paste(temp, ")")

    newCall <- parse(text = temp)
    eval(newCall, envir = .GlobalEnv)
  } else { ## reg not provided => looping ##

    reg <- data.frame(x = 1:2, y = 1:2)

    ## getting input from the user
    while (nrow(reg) > 1) {
      reg <- reg[integer(0), ]
      reg <- data.frame(locator(2))

      if (nrow(reg) > 1) {
        ## define new xlim and ylim
        reg <- as.list(reg)
        reg <- lapply(reg, sort)

        .zoomlog.up(c(reg$x, reg$y))

        ## reconstruct a valid call to plot
        temp <- deparse(last.plot.call)
        temp <- sub("res..[^,]*,", "", temp) # remove 'reset' if provided
        temp <- sub(",[[:blank:]]*res..[^)]*", "", temp) # same thing, if last arg

        newCall <- parse(text = temp)
        eval(newCall, envir = .GlobalEnv)

        reg <- data.frame(reg)
      } # end if nrow(reg) > 1
    } # end while
  } # end else

  return(invisible())
} # end geo.zoomin





###############
## geo.zoomout
###############
#' @export
geo.zoomout <- function() {
  ## get environment
  # geoEnv <- get(".geoGraphEnv", envir=.GlobalEnv)

  ## loop ##
  while (!is.null(locator(1))) {
    ## get last plot
    last.plot.call <- get("last.plot", envir = .geoGraphEnv)

    ## get former coordinates and go one step back
    zoomLog <- get("zoom.log", envir = .geoGraphEnv)
    if (nrow(zoomLog) < 2) {
      cat("\nNo previous zoom coordinates in zoom history.\n")
      return(invisible())
    }

    ## find center of the current frame
    size.x <- abs(diff(zoomLog[1, 1:2]))
    size.y <- abs(diff(zoomLog[1, 3:4]))

    newReg <- zoomLog[1, , drop = TRUE]
    newReg[1:2] <- newReg[1:2] + c(-size.x * 0.5, size.x * 0.5) # new region
    newReg[3:4] <- newReg[3:4] + c(-size.y * 0.5, size.y * 0.5) # new region

    ## make sure we are not going to far
    fullSize <- 0L
    if (newReg[1] < -180) {
      newReg[1] <- -180
      fullSize <- fullSize + 1L
    }
    if (newReg[2] > 180) {
      newReg[2] <- 180
      fullSize <- fullSize + 1L
    }
    if (newReg[3] < -90) {
      newReg[3] <- -90
      fullSize <- fullSize + 1L
    }
    if (newReg[4] > 90) {
      newReg[4] <- 90
      fullSize <- fullSize + 1L
    }

    if (fullSize == 4) {
      cat("\nFull area already displayed.\n")
      return(invisible())
    }

    ## update zoom log
    .zoomlog.up(newReg)

    ## reconstruct a valid call to plot
    temp <- deparse(last.plot.call)

    newCall <- parse(text = temp)

    eval(newCall, envir = .GlobalEnv)
  }

  return(invisible())
} # end geo.zoomout





############
## geo.back
############
#' @export
geo.back <- function() {
  ## get environment
  # geoEnv <- get(".geoGraphEnv", envir=.GlobalEnv)

  ## loop ##
  while (!is.null(locator(1))) {
    ## get last plot
    last.plot.call <- get("last.plot", envir = .geoGraphEnv)

    ## get former coordinates and go one step back
    zoomLog <- get("zoom.log", envir = .geoGraphEnv)
    if (nrow(zoomLog) < 2) {
      cat("\nNo previous zoom coordinates in zoom history.\n")
      return(invisible())
    }

    zoomLog <- zoomLog[-1, , drop = FALSE]
    assign("zoom.log", zoomLog, envir = .geoGraphEnv)

    ## reconstruct a valid call to plot
    temp <- deparse(last.plot.call)

    newCall <- parse(text = temp)

    eval(newCall, envir = .GlobalEnv)
  }

  return(invisible())
} # end geo.back





#############
## geo.slide
#############
#' @export
geo.slide <- function() {
  ## get environment
  # geoEnv <- get(".geoGraphEnv", envir=.GlobalEnv)

  ## loop ##
  while (!is.null(spoint <- locator(1))) {
    ## get last plot
    last.plot.call <- get("last.plot", envir = .geoGraphEnv)

    ## get former coordinates and go one step back
    zoomLog <- get("zoom.log", envir = .geoGraphEnv)

    ## find center of the current frame
    size.x <- abs(diff(zoomLog[1, 1:2]))
    size.y <- abs(diff(zoomLog[1, 3:4]))

    newReg <- zoomLog[1, , drop = TRUE]
    newReg[c(1, 3)] <- c(spoint$x - size.x / 2, spoint$y - size.y / 2)
    newReg[c(2, 4)] <- c(spoint$x + size.x / 2, spoint$y + size.y / 2)

    .zoomlog.up(newReg)

    ## reconstruct a valid call to plot
    temp <- deparse(last.plot.call)

    newCall <- parse(text = temp)

    eval(newCall, envir = .GlobalEnv)
  }

  return(invisible())
} # end geo.slide






############
## geo.bookmark
############
#' @export
geo.bookmark <- function(name = NULL) {
  ## get environment
  # geoEnv <- get(".geoGraphEnv", envir=.GlobalEnv)

  if (is.null(name)) {
    cat("\nAvailable bookmarks:\n")
    return(get("bookmarks", envir = .geoGraphEnv))
  }


  ## get current zoom coords
  zoomLog <- get("zoom.log", envir = .geoGraphEnv)
  new.book <- zoomLog[1, ]

  ## update bookmarks
  bookmarks <- get("bookmarks", envir = .geoGraphEnv)
  if (name %in% rownames(bookmarks)) { # erase previous bookmark if it exists
    bookmarks[name, ] <- new.book
    warning("This bookmark already existed; removing previous bookmark.")
  } else {
    onames <- rownames(bookmarks)
    bookmarks <- rbind(bookmarks, as.vector(new.book))
    rownames(bookmarks) <- c(onames, name)
    cat("\nBookmark '", name, " 'saved.\n")
  }

  assign("bookmarks", bookmarks, envir = .geoGraphEnv)

  return(invisible())
} # end geo.bookmark





############
## geo.goto
############
#' @export
geo.goto <- function(name) {
  ## get environment
  # geoEnv <- get(".geoGraphEnv", envir=.GlobalEnv)

  ## get next zoom coords
  bookmarks <- get("bookmarks", envir = .geoGraphEnv)
  zoomLog <- get("zoom.log", envir = .geoGraphEnv)
  last.plot.call <- get("last.plot", envir = .geoGraphEnv)

  if (!name %in% rownames(bookmarks)) {
    cat("\nUnknown bookmark\n")
    return(geo.bookmark(NULL))
  }

  zoomLog <- rbind(as.vector(bookmarks[name, ]), zoomLog)
  assign("zoom.log", zoomLog, envir = .geoGraphEnv)

  ## reconstruct a valid call to plot
  temp <- deparse(last.plot.call)
  newCall <- parse(text = temp)
  eval(newCall, envir = .GlobalEnv)

  return(invisible())
} # end geo.goto
