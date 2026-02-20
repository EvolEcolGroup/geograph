#' Plot segments correctly when crossing the antimeridian
#'
#' A substitute to \code{segments} which correctly draws
#' segments between locations distant by more than 90 degrees of longitude 
#' (i.e. from one hemisphere to the other). It is used instead of segments, but
#' it is slower.
#' 
#' This low-level function is designed to be called  by other procedures of
#' [geoGraph]. However, it can sometimes be useful by itself. Note that
#' unlike other functions in \code{geoGraph}, this functions does not
#' test for the validity of the provided arguments (for speed purposes).
#'
#' @aliases hasCosts rebuild geo.segments
#' @param x0,y0 coordinates of points *from* which to draw.
#' @param x1,y1 coordinates of points *to* which to draw.
#' @param col a character string or an integer indicating the color of the
#' segments.
#' @param lty a character string or an integer indicating the type of line.
#' @param lwd an integer indicating the line width.
#' @param \dots further graphical parameters (from 'par') passed to the
#' \code{segments} function.
#' @return NULL.
#' 
#' @keywords utilities methods
#' @name auxiliary
#' @export

geo.segments <- function(x0, y0, x1, y1,
                         col = graphics::par("fg"), lty = graphics::par("lty"), lwd = graphics::par("lwd"), ...) {
  ## some declarations ##
  THRES <- 90
  XMIN <- graphics::par("usr")[1]
  XMAX <- graphics::par("usr")[2]
  
  ## pin down problematic segments ##
  toChange <- abs(x0 - x1) > THRES
  if (sum(toChange) == 0) { # exit here if everything is ok.
    graphics::segments(x0, y0, x1, y1,
                       col = col, lty = lty, lwd = lwd, ...
    )
    return(invisible())
  }
  
  ## isolate problematic segments ##
  x0.ok <- x0[!toChange] # these are ok
  x1.ok <- x1[!toChange]
  y0.ok <- y0[!toChange]
  y1.ok <- y1[!toChange]
  
  x0 <- x0[toChange] # problematic
  x1 <- x1[toChange]
  y0 <- y0[toChange]
  y1 <- y1[toChange]
  
  
  ## sort x and y coordinates so that x0 < x1 ##
  toInvert <- (x0 > x1)
  temp <- x0[toInvert] # x coords
  x0[toInvert] <- x1[toInvert]
  x1[toInvert] <- temp
  
  temp <- y0[toInvert] # y coords
  y0[toInvert] <- y1[toInvert]
  y1[toInvert] <- temp
  
  
  ## define new segments ##
  ## notations:
  ## - x0: x coord, left point
  ## - x1: x coord, right point
  ## - d0: distance x0 -
  XMIN
  ## - d1: distance XMAX - x1
  ## - h0, h1: differential of y coord for new coord
  ## (h0/d0 = h1/d1)
  ## - H: distance between y0 and y1
  
  
  d0 <- x0 - XMIN
  d1 <- XMAX - x1
  H <- abs(y1 - y0)
  h0 <- H * (d0 / d1) / (1 + (d0 / d1))
  h1 <- H - h0
  
  x0.new <- rep(XMIN, length(x0))
  x1.new <- rep(XMAX, length(x1))
  ## for y coords, h0 (resp. h1) can be added or subtracted, depending on yo < y1
  facMod.0 <- rep(-1, length(x0))
  facMod.0[y0 < y1] <- 1
  facMod.1 <- facMod.0 * -1
  h0 <- h0 * facMod.0
  h1 <- h1 * facMod.1
  
  y0.new <- y0 + h0
  y1.new <- y1 + h1
  
  
  ## add new segments to old segments ##
  ## order: old segments, new segments
  ## new segments: x0=original coords
  ## x1=new coords
  x0.out <- c(x0, x1)
  y0.out <- c(y0, y1)
  x1.out <- c(x0.new, x1.new)
  y1.out <- c(y0.new, y1.new)
  
  
  ## final call to segments ##
  ## non-modified segments
  oxpd <- graphics::par("xpd")
  graphics::par(xpd = TRUE)
  graphics::segments(x0.ok, y0.ok, x1.ok, y1.ok,
                     col = col, lty = lty, lwd = lwd, ...
  )
  
  ## modified segments
  graphics::segments(x0.out, y0.out, x1.out, y1.out,
                     col = col, lty = 3, lwd = lwd, ...
  )
  
  graphics::par(xpd = oxpd)
  return(invisible())
} # end geo.segments
