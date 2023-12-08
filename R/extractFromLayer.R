#' Retrieves node attributes from a layer
#'
#' The generic function \code{extractFromLayer} uses information from a GIS
#' shapefile to define node attributes. For each node, information is retrieved
#' from the layer and assigned to that node.\cr
#'
#' Nodes can be specified in different ways, including by providing a
#' \linkS4class{gGraph} or a \linkS4class{gData} object. Outputs match the
#' input formats.
#'
#'
#' @aliases extractFromLayer extractFromLayer-methods
#' extractFromLayer,matrix-method extractFromLayer,data.frame-method
#' extractFromLayer,list-method extractFromLayer,gGraph-method
#' extractFromLayer,gData-method
#' @param x a matrix, a data.frame, a list, a valid \linkS4class{gGraph}, or a
#' valid \linkS4class{gData} object. For matrix and data.frame, input must have
#' two columns giving longitudes and latitudes of locations being considered.
#' For list, input must have two components being vectors giving longitudes and
#' latitudes of locations.
#' @param layer a shapefile of the class \code{SpatialPolygonsDataFrame} (see
#' \code{readShapePoly} in maptools package to import such data from a GIS
#' shapefile). Alternatively, a character string indicating one shapefile
#' released with geoGraph; currently, only 'world' is available (see
#' \code{?data(worldshape)}).
#' @param attr a character vector giving names of the variables to be extracted
#' from the layer. If 'all', all available variables are extracted. In case of
#' problem, available names are displayed with the error message. Available
#' data are also stored in \code{layer@data}.
#' @param \dots further arguments to be passed to other methds. Currently not
#' used.
#' @return The output depends on the nature of the input:\cr - \code{matrix,
#' data.frame, list}: a data.frame with one row per location, and as many
#' columns as requested variables ('attributes').\cr
#'
#' - \code{gGraph}: a \linkS4class{gGraph} object with new node attributes
#' (\code{@nodes.attr} slot). If nodes attributes already existed, new
#' attributes are added as new columns.\cr
#'
#' - \code{gData}: a \linkS4class{gData} object with new data associated to
#' locations (\code{@data} slot). New information is merge to older information
#' according to the type of data being stored. \cr

#' @seealso \code{\link{findLand}}, to find which locations are on land.
#' @keywords utilities methods
#' @name extractFromLayer
#' @examples
#' \dontrun{
#'
#' plot(worldgraph.10k, reset = TRUE)
#'
#'
#' ## see what info is available
#' names(worldshape@data)
#' unique(worldshape@data$CONTINENT)
#'
#'
#' ## retrieve continent info for all nodes
#' ## (might take a few seconds)
#' x <- extractFromLayer(worldgraph.10k, layer = worldshape, attr = "CONTINENT")
#' x
#' table(getNodesAttr(x, attr.name = "CONTINENT"))
#'
#'
#' ## subset Africa
#' temp <- getNodesAttr(x, attr.name = "CONTINENT") == "Africa"
#' temp[is.na(temp)] <- FALSE
#' x <- x[temp]
#' plot(x, reset = TRUE)
#' }
#'
NULL


###############
## extractFromLayer
###############
#' @rdname extractFromLayer
#' @export
setGeneric("extractFromLayer", function(x, ...) {
  standardGeneric("extractFromLayer")
})




################
## for matrices (of long/lat)
################
#' @rdname extractFromLayer
#' @export
setMethod("extractFromLayer", "matrix", function(x, layer = "world", attr = "all", ...) {
  ## This functions automatically assigns to land all points overlapping the country polygons
  # if(!require(maptools)) stop("maptools package is required.")

  ## Load default shapefile ##
  if (is.character(layer) && layer[1] == "world") {
    layer <- worldshape
  }

  ## TODO if the layer is null, we should throw an error!!!
  if (!is.null(layer)) {
    if (!inherits(layer, "SpatialPolygonsDataFrame")) {
      stop("Layer must be a SpatialPolygonsDataFrame object \n(see st_read and as_Spatial in sf to import such data from a GIS shapefile).")
    }
  }

  ## search attr in data ##
  if (attr[1] == "all") {
    selAttr <- 1:ncol(layer@data)
  } else {
    selAttr <- match(attr, colnames(layer@data)) # selected attributes
    if (any(is.na(selAttr))) { # attribute not found in layer@data
      cat("\nSome requested attribute (attr) not found in the layer.\n")
      cat("\nAvailable data are:\n")
      print(utils::head(layer@data))
      return(NULL) # return NULL if attr not found, not generate an error
    }
  }

  ## variables and initialization ##
  long <- unlist(x[, 1]) # unlist needed when nrow==1
  lat <- unlist(x[, 2])
  n.poly.list <- length(layer@polygons) # number of lists of Polygons obj.
  res <- NULL
  dat <- layer@data
  layerId <- rep(NA, length(long)) # stores the id of matching polygon for each location


  ## main computations ##

  ## browsing elements of @polygons
  ## each is a list with a @Polygons slot
  for (i in 1:n.poly.list) {
    this.poly.list <- layer@polygons[[i]]
    n.polys <- length(this.poly.list@Polygons)
    points.in.this.poly <- rep(0, length(long))

    ## browsing elements of @Polygons
    for (j in 1:n.polys) { ##
      this.poly <- this.poly.list@Polygons[[j]]
      points.in.this.poly <- points.in.this.poly +
        sp::point.in.polygon(long, lat, this.poly@coords[, 1], this.poly@coords[, 2])

      points.in.this.poly <- as.logical(points.in.this.poly)

      if (any(points.in.this.poly)) {
        layerId[points.in.this.poly] <- this.poly.list@ID
      }
    } # end for j
  } # end for i

  res <- dat[layerId, selAttr, drop = FALSE]
  row.names(res) <- rownames(x)

  return(res)
}) # end extractFromLayer for matrices






################
## for data.frames (of long/lat)
################
#' @rdname extractFromLayer
#' @export
setMethod("extractFromLayer", "data.frame", function(x, layer = "world", attr = "all", ...) {
  x <- as.matrix(x)
  return(extractFromLayer(x, layer = layer, attr = attr, ...))
}) # end extractFromLayer






################
## for list (of long/lat)
################
#' @rdname extractFromLayer
#' @export
setMethod("extractFromLayer", "list", function(x, layer = "world", attr = "all", ...) {
  x <- data.frame(x)
  return(extractFromLayer(x, layer = layer, attr = attr, ...))
}) # end extractFromLayer






##############
## for gGraph # should be carefully used, output is going to be heavy
##############
#' @rdname extractFromLayer
#' @export
setMethod("extractFromLayer", "gGraph", function(x, layer = "world", attr = "all", ...) {
  coords <- getCoords(x)
  res <- extractFromLayer(x = coords, layer = layer, attr = attr, ...)

  if (nrow(x@nodes.attr) > 1) {
    x@nodes.attr <- cbind.data.frame(x@nodes.attr, res)
  } else {
    x@nodes.attr <- res
  }

  return(x)
}) # end findLand






##############
## for gData
##############
#' @rdname extractFromLayer
#' @export
setMethod("extractFromLayer", "gData", function(x, layer = "world", attr = "all", ...) {
  coords <- getCoords(x)
  res <- extractFromLayer(x = coords, layer = layer, attr = attr, ...)

  if (is.null(x@data)) {
    x@data <- res
  } else if (length(nrow(x@data)) > 0 && nrow(x@data) > 1) { # if data are non-empty data.frame
    x@data <- cbind.data.frame(x@data, res)
  } else if (is.list(x@data)) { # if data is a list
    x@data$layerInfo <- res
  } else if (is.vector(x@data) & length(x@data) == nrow(res)) { # if data is a 'mergeable' vector
    x@data <- cbind.data.frame(x@data, res)
  } else { # else, build a list
    warning("x@data has been transformed into a list to include layer data.")
    x@data <- list(x@data, layerInfo = res)
  }

  return(x)
}) # end findLand
