#' Make a new gGraph object from a custom square grid
#'
#' The function [`makeSquareGrid`] builds a [`gGraph`] using a regular square grid for a given area. 
#' If no area is specified, currently plotted area is used. Note that such grid is only valid for 
#' small scales, for cases in which curvature of the surface of the earth can be neglected.
#'
#'
#' @param size an integer giving the approximate number of nodes of the grid.
#' The function will attempt to make a square grid of (approximately) this
#' size.
#' @param n.lon the number of longitude coordinates of the grid (i.e., width of
#' the grid, in number of cells)
#' @param n.lat the number of latitude coordinates of the grid (i.e., height of
#' the grid, in number of cells)
#' @param lon.range,lat.range vectors of length two giving the range covered by
#' the grid, in longitude and latitude, respectively.
#' @return A \linkS4class{gGraph} object.
#' @seealso [`makeHexGrid`] for a hexagonal grid that avoids projection
#'   distortions over large areas. [`findLand`] to classify nodes as land
#'   or sea after creating the grid.
#' @keywords utilities methods
#' @examples
#' # by defining the range covered by the grid
#' squareGraph <- makeSquareGrid(
#'   size      = 10000,
#'   lon.range = c(-12, 2),
#'   lat.range = c(49, 61)
#' )
#' squareGraph <- findLand(squareGraph)
#' squareGraph <- setColors(
#'   squareGraph,
#'   col.rules = data.frame(
#'     habitat = c("sea", "land"),
#'     color = c("blue", "green")
#'   )
#' )
#' plot(squareGraph, reset = TRUE)
#' 
#' # If no area is specified, currently plotted area is used
#' geo.zoomin(c(8, 13, 54, 58))
#' newGraph <- makeSquareGrid(1e3)
#' newGraph <- findLand(newGraph)
#' newGraph <- setColors(
#'   newGraph,
#'   col.rules = data.frame(
#'     habitat = c("sea", "land"),
#'     color = c("blue", "green")
#'   )
#' )
#'
#' ## plot the new gGraph
#' plot(newGraph, reset = TRUE, edge = TRUE)
#' 
#' @export
#' 
makeSquareGrid <- function(size = NULL, n.lon = NULL, n.lat = NULL,
                           lon.range = NULL, lat.range = NULL) {
  
  ## validate size if provided
  if (!is.null(size) && (!is.numeric(size) || length(size) != 1 ||
                         is.na(size) || !is.finite(size) || size <= 0)) {
    stop("size must be a single positive numeric value.")
  }
  
  ## validate n.lon and n.lat if provided
  if (!is.null(n.lon) && (!is.numeric(n.lon) || length(n.lon) != 1 ||
                          is.na(n.lon) || !is.finite(n.lon) ||
                          n.lon < 1 || n.lon %% 1 != 0)) {
    stop("n.lon must be a positive integer.")
  }
  if (!is.null(n.lat) && (!is.numeric(n.lat) || length(n.lat) != 1 ||
                          is.na(n.lat) || !is.finite(n.lat) ||
                          n.lat < 1 || n.lat %% 1 != 0)) {
    stop("n.lat must be a positive integer.")
  }
  
  if (is.null(n.lon)) {
    if (is.null(size)) stop("Please provide either size or n.lon/n.lat.")
    n.lon <- round(sqrt(size))
  }
  
  if (is.null(n.lat)) {
    if (is.null(size)) stop("Please provide either size or n.lon/n.lat.")
    n.lat <- round(sqrt(size))
  }
  
  size <- n.lon * n.lat
  
  ## enforce minimum grid size — update n.lon/n.lat to keep xy consistent
  if (size < 4) {
    n.lon <- 2
    n.lat <- 2
    size  <- 4
    warning("Minimum grid size is 4; using a 2x2 grid.")
  }

  ## GET LON/LAT FROM ZOOM LOG ##
  ## get zoom log info
  zoomLog <- get("zoom.log", envir = .geoGraphEnv)
  if (nrow(zoomLog) < 1) {
    curZoom <- c(-180, 180, -90, 90)
  } else {
    curZoom <- zoomLog[1, ]
  }
  
  if (is.null(lon.range)) {
    lon.range <- curZoom[1:2]
  }
  
  if (is.null(lat.range)) {
    lat.range <- curZoom[3:4]
  }
  
  lon.range <- sort(lon.range)
  lat.range <- sort(lat.range)
  
  
  ## CORRECT LON/LAT ##
  if (lon.range[1] < -180) {
    lon.range[1] <- -180
    warning("Setting lowest longitude to -180 (i.e. 180 W)")
  }
  
  if (lon.range[2] > 180) {
    lon.range[2] <- 180
    warning("Setting largest longitude to 180 (i.e. 180 E)")
  }
  
  if (lat.range[1] < -90) {
    lat.range[1] <- -90
    warning("Setting lowest latitude to -90 (i.e. 90 S)")
  }
  
  if (lat.range[2] > 90) {
    lat.range[2] <- 90
    warning("Setting largest latitude to 90 (i.e. 90 N)")
  }
  
  
  ## BUILD GRID AND FROM/TO MATRIX ##
  x.vec <- seq(lon.range[1], lon.range[2], length = n.lon)
  y.vec <- seq(lat.range[2], lat.range[1], length = n.lat)
  x <- rep(x.vec, each = n.lat)
  y <- rep(y.vec, n.lon)
  xy <- cbind(x, y)
  colnames(xy) <- c("x", "y")
  
  
  ## lateral connections
  from <- seq_len(size - n.lat)
  to <- n.lat + (seq_len(size - n.lat))
  
  ## vertical connections
  temp <- setdiff(seq_len(size - 1), seq(n.lat, size, length = n.lon))
  from <- c(from, temp)
  to <- c(to, temp + 1)
  ft <- cbind(from, to)
  
  ## CREATE graphNEL ##
  myGraph <- ftM2graphNEL(ft, V = as.character(seq_len(size)), edgemode = "undirected")
  
  
  ## CREATE gGraph ##
  res <- new("gGraph", coords = xy, graph = myGraph)
  
  return(res)
} # end makeSquareGrid


#####################
## makeGrid (deprecated)
#####################

#' Deprecated: use makeSquareGrid instead
#'
#' @description `r lifecycle::badge("deprecated")`
#'
#' `makeGrid` has been renamed to [`makeSquareGrid`]. Please update your
#' code accordingly.
#'
#' @inheritParams makeSquareGrid
#' @seealso [`makeSquareGrid`]
#' @export
makeGrid <- function(size = NULL, n.lon = NULL, n.lat = NULL,
                     lon.range = NULL, lat.range = NULL) {
  .Deprecated(
    new     = "makeSquareGrid",
    package = "geoGraph",
    msg     = paste(
      "'makeGrid' has been renamed to 'makeSquareGrid'.",
      "Please update your code to use 'makeSquareGrid' instead."
    )
  )
  makeSquareGrid(size = size, n.lon = n.lon, n.lat = n.lat,
                 lon.range = lon.range, lat.range = lat.range)
}
