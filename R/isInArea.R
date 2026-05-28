############
## isInArea
############
#' Find which nodes fall in a given area
#'
#' The generic function `isInArea` finds which nodes fall in a given area.
#' Nodes can be specified in different ways, including by providing a
#' [`gGraph`] or a [`gData`] object. Different formats for the output are
#' also available. The area can be defined interactively (current plot
#' window), from the zoom log, or explicitly by providing a bounding box.
#'
#' @param x a matrix, `data.frame`, valid [`gGraph`], or valid [`gData`]
#'   object. For matrix and data.frame, input must have two columns giving
#'   longitudes and latitudes of locations being considered.
#' @param ... further arguments passed to specific methods.
#' @param reg a character string or a list indicating the area. Character
#'   strings can be `"current"` (current user window, default) or `"zoom"`
#'   (current zoom). If a list, it must have two components, both being
#'   numeric vectors of length two, giving x and y limits of the area, e.g.
#'   `list(x = c(-10, 30), y = c(35, 70))`. A list produced by `locator()`
#'   is also a valid value.
#' @param res.type a character string indicating what kind of output should
#'   be produced. See value.
#' @param buffer a numeric value giving a buffer adding extra space around
#'   the area, as a proportion of current area's dimensions.
#' @param quiet logical. If `TRUE`, prints the bounding box coordinates
#'   and a reproducible `reg` argument that can be copy-pasted into scripts
#'   for exact reproducibility. Defaults to `FALSE`.
#' @return The output depends on the value of the argument `res.type`:
#'   - `"logical"`: a vector of logicals having one value for each node.
#'   - `"integer"`: a vector of integers corresponding to the indices of
#'     nodes falling within the area.
#'   - `"character"`: a vector of characters corresponding to the names of
#'     the nodes falling within the area.
#' @seealso [`geo.zoomin`] to zoom into an area. [`gGraph-class`] and
#'   [`gData-class`] for the input object classes.
#' @examples
#'
#' ## Zoom into Europe and get the nodes in the current plot
#' plot(worldgraph.10k, reset = TRUE)
#' geo.zoomin(list(x = c(-6, 38), y = c(35, 73)))
#' 
#' ## Different output formats of the current nodes
#' head(isInArea(worldgraph.10k, quiet = TRUE))
#' head(isInArea(worldgraph.10k, res.type = "integer", quiet = TRUE))
#' head(isInArea(worldgraph.10k, res.type = "character", quiet = TRUE))
#'
#' ## subset the gGraph just to visible nodes
#' x <- worldgraph.10k[isInArea(worldgraph.10k)]
#' plot(x, reset = TRUE)
#' 
#' ## Instead of the current plotted area we can use an explicit bounding box
#' y <- worldgraph.10k[(isInArea(worldgraph.10k, 
#'                               reg = list(x = c(113, 154), y = c(-44, -10)), 
#'                               quiet = TRUE))]#' plot(y, reset = TRUE)
#' 
#' @export
setGeneric("isInArea", function(x, ...) {
  standardGeneric("isInArea")
})


################
## method for matrix
################
#' @export
#' @describeIn isInArea Method for matrix
setMethod("isInArea", "matrix", function(x, reg = "current",
                                         res.type = c("logical", "integer", "character"),
                                         buffer = 0,
                                         quiet = FALSE) {
  res.type <- match.arg(res.type)
  coords   <- x
  
  ## get xlim and ylim
  if (exists("zoom.log", envir = .geoGraphEnv) &&
      length(reg) == 1 && reg == "zoom") {
    zoomlog <- get("zoom.log", envir = .geoGraphEnv)
    zoomlog <- zoomlog[1, ]
    xlim    <- zoomlog[1:2]
    ylim    <- zoomlog[3:4]
  } else if (length(reg) == 1 && reg == "current") {
    xlim <- sort(graphics::par("usr")[1:2])
    ylim <- sort(graphics::par("usr")[3:4])
  } else if (is.list(reg)) {
    if (length(reg) != 2) stop("reg is not a list of length 2.")
    xlim <- sort(reg[[1]])[1:2]
    ylim <- sort(reg[[2]])[1:2]
  } else {
    return(NA)
  }
  
  ## handle a buffer around area
  bufferx <- (xlim[2] - xlim[1]) * buffer
  buffery <- (ylim[2] - ylim[1]) * buffer
  xlim    <- xlim + c(-bufferx, bufferx)
  ylim    <- ylim + c(-buffery, buffery)
  
  ## print reproducible call if requested
  if (quiet == FALSE) {
    message(sprintf(
      "Area: lon = [%.4f, %.4f], lat = [%.4f, %.4f]\n  Reproducible call: reg = list(x = c(%.4f, %.4f), y = c(%.4f, %.4f))",
      xlim[1], xlim[2], ylim[1], ylim[2],
      xlim[1], xlim[2], ylim[1], ylim[2]
    ))
  }
  
  toKeep <- ((coords[, 1] >= xlim[1]) & (coords[, 1] <= xlim[2]) &
               (coords[, 2] >= ylim[1]) & (coords[, 2] <= ylim[2]))
  names(toKeep) <- rownames(coords)
  
  if (res.type == "logical")   return(toKeep)
  if (res.type == "integer")   return(which(toKeep))
  if (res.type == "character") return(names(toKeep)[toKeep])
}) # end isInArea for matrix


################
## method for data.frame
################
#' @export
#' @describeIn isInArea Method for data.frame
setMethod("isInArea", "data.frame", function(x, reg = "current",
                                             res.type = c("logical", "integer", "character"),
                                             buffer = 0,
                                             quiet = FALSE) {
  x <- as.matrix(x)
  res <- isInArea(x = x, reg = reg, res.type = res.type,
                  buffer = buffer, quiet = quiet)
  return(res)
}) # end isInArea for data.frame


################
## method for gGraph
################
#' @export
#' @describeIn isInArea Method for gGraph object
setMethod("isInArea", "gGraph", function(x, reg = "current",
                                         res.type = c("logical", "integer", "character"),
                                         buffer = 0,
                                         quiet = FALSE) {
  if (!is.gGraph(x)) stop("x is not a valid gGraph object")
  coords <- getCoords(x)
  res <- isInArea(x = coords, reg = reg, res.type = res.type,
                  buffer = buffer, quiet = quiet)
  return(res)
}) # end isInArea for gGraph


################
## method for gData
################
#' @export
#' @describeIn isInArea Method for gData object
setMethod("isInArea", "gData", function(x, reg = "current",
                                        res.type = c("logical", "integer", "character"),
                                        buffer = 0,
                                        quiet = FALSE) {
  if (!is.gData(x)) stop("x is not a valid gData object")
  coords <- getCoords(x)
  res <- isInArea(x = coords, reg = reg, res.type = res.type,
                  buffer = buffer, quiet = quiet)
  return(res)
}) # end isInArea for gData