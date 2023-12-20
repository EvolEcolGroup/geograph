#' Find which nodes are on land
#'
#' The generic function \code{findLand} uses information from a GIS shapefile
#' to define which nodes are on land, and which are not. Strickly speaking,
#' being 'on land' is in fact being inside a polygon of the shapefile.
#'
#' Nodes can be specified either as a matrix of geographic coordinates, or as a
#' \linkS4class{gGraph} object.
#'
#'
#' @aliases findLand findLand-methods findLand,matrix-method
#' findLand,data.frame-method findLand,gGraph-method
#' @param x a matrix, a data.frame, or a valid \linkS4class{gGraph} object. For
#' matrix and data.frame, input must have two columns giving longitudes and
#' latitudes of locations being considered.
#' @param shape a shapefile of the class [`sf`] (see
#' [sf::st_read()] to import a GIS
#' shapefile). Alternatively, a character string indicating one shapefile
#' released with geoGraph; currently, only 'world' is available.
#' @param \dots further arguments to be passed to other methods. Currently not
#' used.
#' @param attr.name a character string giving the name of the node attribute in
#' which the output is to be stored.
#' @return The output depends on the nature of the input:\cr - \code{matrix,
#' data.frame}: a factor with two levels being 'land' and 'sea'.\cr
#'
#' - \code{gGraph}: a \linkS4class{gGraph} object with a new node attribute,
#' possibly added to previously existing node attributes (\code{@nodes.attr}
#' slot).\cr

#' @seealso \code{\link{extractFromLayer}}, to retrieve any information from a
#' GIS shapefile.
#' @keywords utilities methods
#' @name findLand
#' @examples
#'
#'
#' ## create a new gGraph with random coordinates
#' myCoords <- data.frame(long = runif(1000, -180, 180), lat = runif(1000, -90, 90))
#' obj <- new("gGraph", coords = myCoords)
#' obj # note: no node attribute
#' plot(obj)
#'
#' ## find which points are on land
#' obj <- findLand(obj)
#' obj # note: new node attribute
#'
#' ## define rules for colors
#' temp <- data.frame(habitat = c("land", "sea"), color = c("green", "blue"))
#' temp
#' obj@meta$colors <- temp
#'
#' ## plot object with new colors
#' plot(obj)
#'
NULL


############
## findLand
############
#' @rdname findLand
#' @export
setGeneric("findLand", function(x, ...) {
  standardGeneric("findLand")
})






################
## for matrices (of long/lat)
################
#' @rdname findLand
#' @export
setMethod("findLand", "matrix", function(x, shape = "world", ...) {
  ## This functions automatically assigns to land all points overlapping the country polygons
  #    if(!require(maptools)) stop("maptools package is required.")

  ## Load default shapefile ##
  if (is.character(shape) && shape[1] == "world") {
    # use rnaturalearth instead of the inbuilt dataset
    shape <- rnaturalearth::ne_countries(scale="medium", returnclass = "sf")
    sf::sf_use_s2(FALSE)
    #shape <- sf::st_read(system.file("files/shapefiles/world-countries.shp", package = "geoGraph"))
  }


  ## TODO if the shape is null, we should throw an error!!!
  if (!is.null(shape)) {
    if (!inherits(shape, "sf")) {
      if (inherits(shape, "SpatialPolygonsDataFrame")){
        shape <- sf::st_as_sf(shape)
      } else {
        stop("shape must be a sf object \n(see st_read in sf to import such data from a GIS shapefile).")
      }
    }
  }
  
  
  if (any(is.na(x))) {
    stop("Matrix contains NA values.")
  }
 
  # create an sf point object from the coordinates
  locations_st <- x %>% as.data.frame %>% 
    sf::st_as_sf(coords=c(1,2)) %>%
    sf::st_set_crs(sf::st_crs(shape))
  # now find points in polygons
  points_within <- sf::st_intersects(shape, locations_st)
  points_within <- data.frame(x = unlist(points_within), 
                              polygon = rep(seq_along(lengths(points_within)), lengths(points_within)))
 
  land<-rep("sea",nrow(x))
  land[points_within$x]<-"land"

  return(factor(land))
})



################
## for data.frames (of long/lat)
################
#' @rdname findLand
#' @export
setMethod("findLand", "data.frame", function(x, shape = "world", ...) {
  x <- as.matrix(x)
  return(findLand(x, shape = shape, ...))
}) # end findLand






##############
## for gGraph
##############
#' @rdname findLand
#' @export
setMethod("findLand", "gGraph", function(x, shape = "world", attr.name = "habitat", ...) {
  coords <- getCoords(x)
  res <- findLand(coords, shape = shape, ...)
  if (nrow(x@nodes.attr) > 1) {
    x@nodes.attr <- cbind.data.frame(x@nodes.attr, res)
    names(x@nodes.attr)[ncol(x@nodes.attr)] <- attr.name
  } else {
    x@nodes.attr <- data.frame(res)
    names(x@nodes.attr) <- attr.name
  }
  return(x)
}) # end findLand
