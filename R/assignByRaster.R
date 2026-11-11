#' Assign raster values to graph nodes
#'
#' The function `assignByRaster` takes a [`gGraph`] object and a
#' `SpatRaster`, assigns each raster cell to the nearest graph node, and
#' collapses the values to a single scalar per node using a summary function.
#'
#' @param graph a [`gGraph`] object.
#' @param raster a `SpatRaster` object (from the `terra` package).
#' @param layer.name a character string giving the name of the new node
#'   attribute. Defaults to `"raster_points"`.
#' @param fun a function or character string specifying how to summarize
#'   raster values within each node. Built-in options are `"mean"`, `"max"`,
#'   `"min"`, `"median"`, `"sd"`, `"any"`, and `"all"`. Alternatively, pass
#'   any function that takes a numeric vector and returns a scalar (`NA`s are
#'   removed beforehand when `na.rm = TRUE`). Defaults to `"mean"`.
#' @param na.rm logical. Whether to remove `NA` values before summarizing.
#'   Defaults to `TRUE`.
#' @param ... additional arguments passed to `fun`.
#' @return A [`gGraph`] object with a new scalar node attribute named
#'   `layer.name`.
#' @details Each cell is assigned to the node closest to its center, by
#'   great-circle distance (rasters in another CRS are projected to WGS84
#'   first). A cell center exactly equidistant from several nodes goes to one
#'   of them, chosen arbitrarily. Nodes to which no cell is closest get `NA`;
#'   this happens when the raster is coarser than the graph.
#'
#'   This function is memory-intensive for high-resolution rasters or
#'   large graphs. Once values are assigned, further node attributes can be
#'   derived using [`setNodesAttr`].
#' @seealso [`setNodesAttr`] to set node attributes manually.
#'   [`assignByPolygon`] to assign attributes from GIS shapefiles.
#' @examples
#'
#' ## Make a new gGraph without any nodes attribute
#' geo.box <- c(xmin = -10, xmax = 30, ymin = 35, ymax = 60)
#' ggraph <- makeHexGrid(geo.box, spacing = 1000)
#'
#' ## Create a synthetic raster of random elevation values over the same region.
#' set.seed(42)
#' r <- terra::rast(
#'   xmin = geo.box["xmin"], xmax = geo.box["xmax"],
#'   ymin = geo.box["ymin"], ymax = geo.box["ymax"],
#'   resolution = 5, crs = "EPSG:4326"
#' )
#' terra::values(r) <- runif(terra::ncell(r))
#'
#' ## assign mean raster value per node
#' ggraph <- assignByRaster(ggraph, r, layer.name = "elevation", fun = "mean")
#'
#' ## assign standard deviation per node (useful for ruggedness)
#' ggraph <- assignByRaster(ggraph, r, layer.name = "ruggedness", fun = "sd")
#'
#' @export
assignByRaster <- function(graph, raster, layer.name = "raster_points",
                           fun = "mean", na.rm = TRUE, ...) {
  if (!inherits(graph, "gGraph")) stop("graph must be a gGraph object.")

  if (!inherits(raster, "SpatRaster")) stop("raster must be a SpatRaster object.")
  if (terra::nlyr(raster) != 1L) {
    stop("raster must have exactly one layer.")
  }

  ## values and centers of the non-NA cells (as as.data.frame(raster, xy =
  ## TRUE), keeping factor and logical values)
  value <- terra::values(raster, dataframe = TRUE)[[1]]
  cells <- which(!is.na(value))
  value <- value[cells]
  xy <- terra::xyFromCell(raster, cells)

  ## cell centers in lon/lat (WGS84)
  raster.crs <- terra::crs(raster, proj = TRUE)
  if (sf::st_crs(raster.crs) != sf::st_crs(4326)) {
    pts <- sf::st_as_sf(as.data.frame(xy), coords = c(1, 2), crs = raster.crs)
    xy <- sf::st_coordinates(sf::st_transform(pts, 4326))
  }

  ## nearest node for each cell: Euclidean distance on lon/lat, as
  ## sf::st_nearest_feature() with s2 switched off, but with a k-d tree
  n.nodes <- nrow(getCoords(graph))
  node.id <- integer(0)
  if (length(cells) > 0 && n.nodes > 0) {
    node.id <- FNN::get.knnx(.buildNnIndex(getCoords(graph)), .buildNnIndex(xy), k = 1)$nn.index[, 1]
    }

  ## values per node (nodes without cells get an empty vector), collapsed to
  ## a scalar per node using internal function
  collapsed <- .collapseNodeAttribute(
    x      = unname(split(value, factor(node.id, levels = seq_len(n.nodes)))),
    fun    = fun,
    na.rm  = na.rm,
    ...
  )

  graph <- setNodesAttr(graph, attr.name = layer.name, values = collapsed)
  return(graph)
}


#' Collapse a list-based node attribute to a scalar per node
#'
#' Internal function used by [`assignByRaster`] to summarize a list of
#' per-node vectors of values into a single value per node.
#'
#' @param x a list of vectors, one per node (empty for nodes without values).
#' @param fun a function or character string, see [`assignByRaster`].
#' @param na.rm logical. Whether to remove `NA` values. Defaults to `TRUE`.
#' @param ... additional arguments passed to `fun`.
#' @return a numeric vector of length equal to `length(x)`.
#' @noRd
.collapseNodeAttribute <- function(x, fun = "mean", na.rm = TRUE, ...) {
  ## resolve fun
  if (is.character(fun)) {
    fun <- match.arg(fun, c("mean", "max", "min", "median", "sd", "any", "all"))
    fun <- switch(fun,
                  mean   = mean,
                  max    = max,
                  min    = min,
                  median = stats::median,
                  sd     = stats::sd,
                  any    = any,
                  all    = all
    )
  }
  if (!is.function(fun)) {
    stop("`fun` must be a function or a supported character string.")
  }
  
  ## collapse one node safely
  collapseOneNode <- function(vals, fun, na.rm, ...) {
    if (length(vals) == 0 || all(is.na(vals))) {
      return(NA_real_)
    }
    if (identical(fun, any) || identical(fun, all)) {
      vals <- as.logical(vals)
    }
    ## remove NAs here, so that fun does not need an na.rm argument
    if (na.rm) {
      vals <- vals[!is.na(vals)]
    }
    out <- fun(vals, ...)
    if (length(out) != 1L) stop("`fun` must return a scalar per node.")
    return(out)
  }
  
  sapply(x, collapseOneNode, fun = fun, na.rm = na.rm, ...)
}
