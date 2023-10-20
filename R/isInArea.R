############
## isInArea
############
#' Find which nodes fall in a given area
#'
#' The generic function \code{isInArea} finds which nodes fall in a given area.
#' Nodes can be specified in different ways, including by providing a
#' \linkS4class{gGraph} or a \linkS4class{gData} object. Different format for
#' the output are also available.
#'
#'
#' @aliases isInArea isInArea-methods isInArea,matrix-method
#' isInArea,data.frame-method isInArea,gGraph-method isInArea,gData-method
#' @param x a matrix, a data.frame, a valid \linkS4class{gGraph}, or a valid
#' \linkS4class{gData} object. For matrix and data.frame, input must have two
#' columns giving longitudes and latitudes of locations being considered.
#' @param \dots further arguments passed to specific methods.
#' @param reg a character string or a list indicating the area ('reg' stands
#' for 'region'). Character strings can be "current" (current user window,
#' default) or "zoom" (current zoom). If the argument is a list, is has to have
#' two components, both being numeric vectors of length two, giving x and y
#' limits of the area. Note that such list can be produced by \code{locator},
#' so \code{locator(1)} is a valid value for \code{reg}.
#' @param res.type a character string indicating what kind of output should be
#' produced. See value.
#' @param buffer a numeric value giving a buffer adding extra space aroung the
#' area, as a proportion of current area's dimensions.
#' @return The output depends on the value of the argument \code{res.type}:\cr
#' - \code{logical}: a vector of logicals having one value for each node of the
#' input.\cr
#'
#' - \code{integer}: a vector of integers corresponding to the indices of nodes
#' falling within the area.\cr
#'
#' - \code{character}: a vector of characters corresponding to the names of the
#' nodes falling within the area.\cr
#' @author Thibaut Jombart (\email{t.jombart@@imperial.ac.uk})
#' @keywords utilities methods
#' @export
#' @examples
#'
#' plot(worldgraph.10k, reset = TRUE)
#'
#' ## zooming in
#' geo.zoomin(list(x = c(-6, 38), y = c(35, 73)))
#' title("Europe")
#'
#'
#' ## different outputs of isInArea
#' head(isInArea(worldgraph.10k)) # logical
#' length(isInArea(worldgraph.10k))
#' sum(isInArea(worldgraph.10k))
#' head(which(isInArea(worldgraph.10k))) # which nodes are TRUE ?
#'
#' head(isInArea(worldgraph.10k, res.type = "integer")) # node indices
#'
#' head(isInArea(worldgraph.10k, res.type = "character")) # node names
#'
#'
#' ## use isInArea to have a subset of visible nodes
#' x <- worldgraph.10k[isInArea(worldgraph.10k)]
#' plot(x, reset = TRUE)
#'
setGeneric("isInArea", function(x, ...) {
  standardGeneric("isInArea")
})




################
## method for matrix
################
#' @export
#' @describeIn isInArea Method for matrix
setMethod("isInArea", "matrix", function(x, reg = "current", res.type = c("logical", "integer", "character"), buffer = 0) {
  ## some checks / definitiona
  res.type <- match.arg(res.type)
  # env <- get(".geoGraphEnv", envir=.GlobalEnv) # env is our target environnement
  coords <- x

  ## get xlim and ylim
  if (exists("zoom.log", envir = .geoGraphEnv) && length(reg) == 1 && reg == "zoom") { # xlim/ylim taken from log
    zoomlog <- get("zoom.log", envir = .geoGraphEnv)
    zoomlog <- zoomlog[1, ]

    xlim <- zoomlog[1:2]
    ylim <- zoomlog[3:4]
  } else if (length(reg) == 1 && reg == "current") { # xlim/ylim taken from par("usr")
    xlim <- sort(graphics::par("usr")[1:2])
    ylim <- sort(graphics::par("usr")[3:4])
  } else if (is.list(reg)) { # xlim/ylim user-provided (reg)
    if (length(reg) != 2) stop("reg is not a list of length 2.")
    xlim <- sort(reg[[1]])[1:2]
    ylim <- sort(reg[[2]])[1:2]
  } else {
    return(NA)
  }


  ## main computations ##

  ## handle a buffer around area
  bufferx <- (xlim[2] - xlim[1]) * buffer
  buffery <- (ylim[2] - ylim[1]) * buffer

  xlim <- xlim + c(-bufferx, bufferx)
  ylim <- ylim + c(-buffery, buffery)

  toKeep <- ((coords[, 1] >= xlim[1]) & (coords[, 1] <= xlim[2]) # matching longitude
  & (coords[, 2] >= ylim[1]) & (coords[, 2] <= ylim[2])) # matching latitude

  names(toKeep) <- rownames(coords)

  if (res.type == "logical") { # return a named vector of logicals
    return(toKeep)
  }

  if (res.type == "integer") { # return a named vector of node numbers
    return(which(toKeep))
  }

  if (res.type == "character") { # return names of nodes in the area
    res <- names(toKeep)[toKeep]
    return(res)
  }
}) # end isInArea for matrix






################
## method for data.frame
################
#' @export
#' @describeIn isInArea Method for data.frame
setMethod("isInArea", "data.frame", function(x, reg = "current", res.type = c("logical", "integer", "character"), buffer = 0) {
  ## preliminary stuff
  x <- as.data.frame(x)

  res <- isInArea(x = x, reg = reg, res.type = res.type, buffer = buffer)
  return(res)
}) # end isInArea for data.frame






################
## method for gGraph
################
#' @export
#' @describeIn isInArea Method for gGraph object
setMethod("isInArea", "gGraph", function(x, reg = "current", res.type = c("logical", "integer", "character"), buffer = 0) {
  ## preliminary stuff
  if (!is.gGraph(x)) stop("x is not a valid gGraph object")
  coords <- getCoords(x)

  res <- isInArea(x = coords, reg = reg, res.type = res.type, buffer = buffer)
  return(res)
}) # end isInArea for gGraph






################
## method for gData
################
#' @export
#' @describeIn isInArea Method for gData object
setMethod("isInArea", "gData", function(x, reg = "current", res.type = c("logical", "integer", "character"), buffer = 0) {
  ## preliminary stuff
  if (!is.gData(x)) stop("x is not a valid gGraph object")
  coords <- getCoords(x)

  res <- isInArea(x = coords, reg = reg, res.type = res.type, buffer = buffer)
  return(res)
}) # end isInArea for gData
