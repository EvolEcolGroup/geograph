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
#' @param layer a shapefile of the class [`sf`] (see
#' [sf::st_read()] to import a GIS
#' shapefile). Alternatively, a character string indicating one shapefile
#' released with geoGraph; currently, only 'world' is available.
#' @param attr a character vector giving names of the variables to be extracted
#' from the layer. If 'all', all available variables are extracted. In case of
#' problem, available names are displayed with the error message.
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
#' ## retrieve continent info for all nodes
#' ## (might take a few seconds)
#' x <- extractFromLayer(worldgraph.10k, layer = "world", attr = "CONTINENT")
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

  ## Load default shapefile ##
  if (is.character(layer) && layer[1] == "world") {
    # use rnaturalearth instead of the inbuilt dataset
    # layer <- rnaturalearth::ne_countries(scale="medium", returnclass = "sf")
    # sf::sf_use_s2(FALSE)
    layer <- sf::st_read(system.file("files/shapefiles/world-countries.shp", package = "geoGraph"))
  }

  ## TODO if the layer is null, we should throw an error!!!
  if (!is.null(layer)) {
    if (!inherits(layer, "sf")) {
      if (inherits(layer, "SpatialPolygonsDataFrame")){
        layer <- sf::st_as_sf(layer)
      } else {
        stop("Layer must be a sf object \n(see st_read in sf to import such data from a GIS shapefile).")
      }
    }
  }


  ## search attr in data ##
  if (attr[1] == "all") {
    selAttr <- 1:ncol(layer)
  } else {
    selAttr <- match(attr, colnames(layer)) # selected attributes
    if (any(is.na(selAttr))) { # attribute not found in layer@data
      cat("\nSome requested attribute (attr) not found in the layer.\n")
      cat("\nAvailable data are:\n")
      print(utils::head(layer))
      return(NULL) # return NULL if attr not found, not generate an error
    }
  }


  # create an sf point object from the coordinates
  locations_st <- x %>% as.data.frame %>% 
    sf::st_as_sf(coords=c(1,2)) %>%
    sf::st_set_crs(sf::st_crs(layer))
  # now find points in polygons
  points_within <- sf::st_intersects(layer, locations_st)
  points_within <- data.frame(x = unlist(points_within), 
                              polygon = rep(seq_along(lengths(points_within)), lengths(points_within)))
  points_assignment <- data.frame(x=seq(1, nrow(x)), polygon = NA)
  # add missing points for which we have no information
  points_assignment[points_within$x,"polygon"]<-points_within$polygon

  dat <- layer %>% sf::st_drop_geometry()
  # @TOFIX the line below will fail if layerId is all NAs (i.e. no points were assigned to a polygon)
  res <- dat[points_assignment$polygon, selAttr, drop = FALSE]

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
})
