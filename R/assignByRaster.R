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
#'   any function that takes a numeric vector and returns a scalar. Defaults
#'   to `"mean"`.
#' @param na.rm logical. Whether to remove `NA` values before summarizing.
#'   Defaults to `TRUE`.
#' @param ... additional arguments passed to `fun`.
#' @return A [`gGraph`] object with a new scalar node attribute named
#'   `layer.name`.
#' @details This function is memory-intensive for high-resolution rasters or
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

  ## convert raster to data.frame
  raster.df <- as.data.frame(raster, xy = TRUE)
  colnames(raster.df) <- c("lon", "lat", "value")

  ## convert to sf
  raster.sf <- sf::st_as_sf(
    raster.df,
    coords = c("lon", "lat"),
    crs = terra::crs(raster, proj = TRUE)
  )
  node.coords <- as.data.frame(getCoords(graph))
  node.coords$node.id <- seq_len(nrow(node.coords))
  nodes.sf <- sf::st_as_sf(node.coords, coords = c("lon", "lat"), crs = 4326)
  raster.sf <- sf::st_transform(raster.sf, sf::st_crs(nodes.sf))

  ## find nearest node for each raster point
  old_s2 <- sf::sf_use_s2()
  on.exit(sf::sf_use_s2(old_s2), add = TRUE)
  sf::sf_use_s2(FALSE)
  raster.sf$node.id <- sf::st_nearest_feature(raster.sf, nodes.sf)

  ## build nested tibble of values per node
  attribute <- raster.sf %>%
    sf::st_drop_geometry() %>%
    dplyr::group_by(.data$node.id) %>%
    tidyr::nest()

  all.nodes <- tibble::tibble(node.id = seq_len(nrow(nodes.sf)))
  attribute.full <- all.nodes %>%
    dplyr::left_join(attribute, by = "node.id")

  ## collapse to scalar per node using internal function
  collapsed <- .collapseNodeAttribute(
    x      = attribute.full$data,
    fun    = fun,
    na.rm  = na.rm,
    ...
  )

  graph@nodes.attr[[layer.name]] <- collapsed
  return(graph)
}


#' Collapse a list-based node attribute to a scalar per node
#'
#' Internal function used by [`assignByRaster`] to summarize a list of
#' per-node data frames into a single numeric value per node.
#'
#' @param x a list of data frames, one per node (as produced by
#'   `assignByRaster` internally).
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
  collapseOneNode <- function(df, fun, na.rm, ...) {
    if (is.null(df) || length(df) == 0 || nrow(df) == 0) {
      return(NA_real_)
    }
    if (ncol(df) != 1) stop("Node data has multiple columns; cannot infer value column.")
    vals <- df[[1]]
    if (length(vals) == 0 || all(is.na(vals))) {
      return(NA_real_)
    }
    if (identical(fun, any) || identical(fun, all)) {
      vals <- as.logical(vals)
    }
    out <- fun(vals, na.rm = na.rm, ...)
    if (length(out) != 1L) stop("`fun` must return a scalar per node.")
    return(out)
  }

  sapply(x, collapseOneNode, fun = fun, na.rm = na.rm, ...)
}
