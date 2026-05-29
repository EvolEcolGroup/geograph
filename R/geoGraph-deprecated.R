# nocov start

#' @title Deprecated functions in package geoGraph
#' @description Functions listed below are deprecated and will be removed in
#'   a future version. Where possible, alternatives are mentioned.
#'   Help pages for deprecated functions are available at
#'   `help("<function>-deprecated")`.
#' @name geoGraph-deprecated
#' @return NULL.
#' @keywords internal
#' @examples
#' \dontrun{
#' # these functions are deprecated — use the replacements instead:
#' # makeGrid()       -> makeSquareGrid()
#' # extractFromLayer() -> assignByPolygon()
#' }
NULL


##########################
## makeGrid (deprecated)
##########################

#' @title Make a new gGraph object from a custom square grid
#' @description Deprecated. Use [`makeSquareGrid`] instead.
#' @param size an integer giving the approximate number of nodes.
#' @param n.lon the number of longitude coordinates.
#' @param n.lat the number of latitude coordinates.
#' @param lon.range,lat.range vectors of length two giving the range.
#' @return A [`gGraph`] object. See [`makeSquareGrid`] for details.
#' @name makeGrid-deprecated
#' @usage makeGrid(size = NULL, n.lon = NULL, n.lat = NULL,
#'   lon.range = NULL, lat.range = NULL)
#' @seealso [`geoGraph-deprecated`]
#' @keywords internal
#' @examples
#' \dontrun{
#' # by defining the range covered by the grid
#' squareGraph <- makeGrid(
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
#' newGraph <- makeGrid(1e3)
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
#' }
NULL

#' @rdname geoGraph-deprecated
#' @section `makeGrid`:
#' For `makeGrid`, use [`makeSquareGrid`].
#' @export
makeGrid <- function(size = NULL, n.lon = NULL, n.lat = NULL,
                     lon.range = NULL, lat.range = NULL) {
  .Deprecated("makeSquareGrid",
    package = "geoGraph",
    msg = "'makeGrid' has been renamed to 'makeSquareGrid'. Please update your code."
  )
  makeSquareGrid(
    size = size, n.lon = n.lon, n.lat = n.lat,
    lon.range = lon.range, lat.range = lat.range
  )
}


##########################
## extractFromLayer (deprecated)
##########################

#' @title Assign node attributes from a polygon layer
#' @description Deprecated. Use [`assignByPolygon`] instead.
#' @param x a matrix, `data.frame`, list, valid [`gGraph`], or valid [`gData`]
#'   object.
#' @param ... further arguments passed to other methods.
#' @return See [`assignByPolygon`] for details.
#' @name extractFromLayer-deprecated
#' @seealso [`geoGraph-deprecated`]
#' @keywords internal
#' @examples
#' \dontrun{
#' plot(worldgraph.10k, reset = TRUE)
#'
#' ## retrieve continent info for all nodes
#' ## (might take a few seconds)
#' x <- extractFromLayer(worldgraph.10k, layer = "world", attr = "continent")
#' x
#' table(getNodesAttr(x, attr.name = "continent"))
#'
#'
#' ## subset Africa
#' temp <- getNodesAttr(x, attr.name = "continent") == "Africa"
#' temp[is.na(temp)] <- FALSE
#' x <- x[temp]
#' plot(x, reset = TRUE)
#' }
NULL

#' @rdname geoGraph-deprecated
#' @section `extractFromLayer`:
#' For `extractFromLayer`, use [`assignByPolygon`].
#' @aliases extractFromLayer
#' @export
setGeneric("extractFromLayer", function(x, ...) {
  standardGeneric("extractFromLayer")
})

#' @rdname extractFromLayer-deprecated
#' @export
setMethod("extractFromLayer", "ANY", function(x, ...) {
  .Deprecated("assignByPolygon",
    package = "geoGraph",
    msg = "'extractFromLayer' has been renamed to 'assignByPolygon'. Please update your code."
  )
  assignByPolygon(x, ...)
})

# nocov end
