#' Assign node attributes from a polygon layer
#'
#' The function `assignByPolygon` uses information from a GIS polygon
#' shapefile to define node attributes. For each node, information is
#' retrieved from the layer and assigned to that node.
#'
#' Nodes can be specified as a matrix, `data.frame`, list, [`gGraph`], or
#' [`gData`] object. Outputs match the input format.
#'
#' @param x a matrix, `data.frame`, list, valid [`gGraph`], or valid [`gData`]
#'   object. For matrix and data.frame, input must have two columns giving
#'   longitudes and latitudes. For list, input must have two components being
#'   vectors of longitudes and latitudes.
#' @param layer a shapefile of class `sf` (see [`sf::st_read`] to import a
#'   GIS shapefile). Alternatively, `"world"` to use the built-in world
#'   shapefile.
#' @param attr a character vector of variable names to extract from the layer.
#'   Use `"all"` to extract all available variables.
#' @param ... further arguments passed to other methods (currently unused).
#' @return For matrix, data.frame, or list input: a `data.frame` with one row
#'   per location and one column per requested variable. For [`gGraph`] input:
#'   a [`gGraph`] object with new node attributes added to `@nodes.attr`. For
#'   [`gData`] input: a [`gData`] object with new data added to `@data`.
#' @seealso [`findLand`] to find which locations are on land.
#'   [`assignByRaster`] to assign attributes from raster data.
#' @examples
#'
#' plot(worldgraph.10k, reset = TRUE)
#'
#' ## retrieve continent info for all nodes
#' ## (might take a few seconds)
#' x <- assignByPolygon(worldgraph.10k, layer = "world", attr = "continent")
#' x
#' table(getNodesAttr(x, attr.name = "continent"))
#'
#'
#' ## subset Africa
#' temp <- getNodesAttr(x, attr.name = "continent") == "Africa"
#' temp[is.na(temp)] <- FALSE
#' x <- x[temp]
#' plot(x, reset = TRUE)
#'
#' @export
setGeneric("assignByPolygon", function(x, ...) {
  standardGeneric("assignByPolygon")
})


################
## for matrices (of long/lat)
################
#' @describeIn assignByPolygon Method for matrix input
#' @export
setMethod("assignByPolygon", "matrix", function(x, layer = "world", attr = "all", ...) {
  ## Load default shapefile ##
  if (is.character(layer) && layer[1] == "world") {
    # use rnaturalearth instead of the inbuilt dataset
    layer <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
    # layer <- sf::st_read(system.file("files/shapefiles/world-countries.shp", package = "geoGraph"))
  }
  old_s2 <- sf::sf_use_s2()
  on.exit(sf::sf_use_s2(old_s2), add = TRUE)
  sf::sf_use_s2(FALSE)
  ## TODO if the layer is null, we should throw an error!!!
  if (!is.null(layer)) {
    if (!inherits(layer, "sf")) {
      if (inherits(layer, "SpatialPolygonsDataFrame")) {
        layer <- sf::st_as_sf(layer)
      } else {
        stop("Layer must be a sf object \n(see st_read in sf to import such data from a GIS shapefile).")
      }
    }
  }


  ## search attr in data ##
  if (attr[1] == "all") {
    # selAttr <- 1:ncol(layer)
    selAttr <- seq_len(ncol(layer)) - 1
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
  locations.st <- x %>%
    as.data.frame() %>%
    sf::st_as_sf(coords = c(1, 2)) %>%
    sf::st_set_crs(sf::st_crs(layer))
  # now find points in polygons
  points.within <- sf::st_intersects(layer, locations.st)
  points.within <- data.frame(
    x = unlist(points.within),
    polygon = rep(seq_along(lengths(points.within)), lengths(points.within))
  )
  points.assignment <- data.frame(x = seq(1, nrow(x)), polygon = NA)
  # add missing points for which we have no information
  points.assignment[points.within$x, "polygon"] <- points.within$polygon

  dat <- layer %>% sf::st_drop_geometry()
  # @TOFIX the line below will fail if layerId is all NAs (i.e. no points were assigned to a polygon)
  res <- dat[points.assignment$polygon, selAttr, drop = FALSE]

  row.names(res) <- rownames(x)

  return(res)
}) # end assignByPolygon for matrices


################
## for data.frames (of long/lat)
################
#' @describeIn assignByPolygon Method for data.frames input
#' @export
setMethod("assignByPolygon", "data.frame", function(x, layer = "world", attr = "all", ...) {
  x <- as.matrix(x)
  return(assignByPolygon(x, layer = layer, attr = attr, ...))
}) # end assignByPolygon


################
## for numeric vector (of long/lat)
################
#' @describeIn assignByPolygon Method for numeric vector input
#' @export
setMethod("assignByPolygon", "numeric", function(x, layer = "world", attr = "all", ...) {
  if (isTRUE(length(x) %% 2 == 0)) {
    x <- matrix(x, ncol = 2, byrow = TRUE)
    return(assignByPolygon(x, layer = layer, attr = attr, ...))
  } else {
    stop("Vector must have even number of longitude and latitude entries")
  }
})


################
## for list (of long/lat)
################
#' @describeIn assignByPolygon Method for numeric list input
#' @export
setMethod("assignByPolygon", "list", function(x, layer = "world", attr = "all", ...) {
  x <- data.frame(x)
  return(assignByPolygon(x, layer = layer, attr = attr, ...))
}) # end assignByPolygon


##############
## for gGraph # should be carefully used, output is going to be heavy
##############
#' @describeIn assignByPolygon Method for numeric gGraph objects
#' @note The gGraph method should be carefully used, output is going to be heavy.
#' @export
setMethod("assignByPolygon", "gGraph", function(x, layer = "world", attr = "all", ...) {
  coords <- getCoords(x)
  res <- assignByPolygon(x = coords, layer = layer, attr = attr, ...)

  if (nrow(x@nodes.attr) > 1) {
    x@nodes.attr <- cbind.data.frame(x@nodes.attr, res)
  } else {
    x@nodes.attr <- res
  }

  return(x)
}) # end assignByPolygon


##############
## for gData
##############
#' @describeIn assignByPolygon Method for numeric gData objects
#' @export
setMethod("assignByPolygon", "gData", function(x, layer = "world", attr = "all", ...) {
  coords <- getCoords(x)
  res <- assignByPolygon(x = coords, layer = layer, attr = attr, ...)

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
