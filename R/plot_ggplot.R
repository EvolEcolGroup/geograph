#' ggplot layer for a [`gGraph`]
#'
#' Adds nodes (and optionally edges) of a [`gGraph`] to a ggplot as
#' [ggplot2::geom_sf()] layers.
#'
#' @param mapping aesthetics. Variables from the gGraph's node attributes may
#'   be used (e.g. `aes(colour = habitat)`).
#' @param data a [`gGraph`] object.
#' @param edges logical; whether to draw edges. Defaults to `FALSE`.
#' @param stat,position,na.rm,show.legend,... forwarded to [ggplot2::geom_sf()].
#' @return one or two ggplot layers (edges under nodes).
#' @examplesIf requireNamespace("ggplot2", quietly = TRUE)
#' library(ggplot2)
#' ggplot() +
#'  geom_ggraph(data = worldgraph.10k, aes(colour = habitat), edges = TRUE, size = 0.3) +
#'  scale_colour_manual(values = c(land = "grey70", sea = "lightblue", coast = "grey70")) +
#'  coord_sf(crs = "+proj=ortho +lat_0=40 +lon_0=30") +
#'  theme_void()
#' @export
#' @family ggplot_methods
geom_ggraph <- function(mapping = ggplot2::aes(), data = NULL, edges = FALSE,
                        stat = "sf", position = "identity",
                        na.rm = FALSE, show.legend = NA, ...) {
  if (is.null(data))            stop("data (a gGraph) must be specified")
  if (!inherits(data, "gGraph")) stop("data must be a gGraph object")
  
  layers <- list()
  if (edges) {
    layers <- c(layers, list(
      ggplot2::geom_sf(data = .dfToSfLines(.ggraphEdgesDf(data)),
                       colour = "grey65", linewidth = 0.4,
                       inherit.aes = FALSE)
    ))
  }
  layers <- c(layers, list(
    ggplot2::geom_sf(mapping = mapping,
                     data = .dfToSfPoints(.ggraphNodesDf(data)),
                     stat = stat, position = position,
                     na.rm = na.rm, show.legend = show.legend,
                     inherit.aes = FALSE, ...)
  ))
  layers
}


#' ggplot layer for a [`gData`]
#'
#' Adds sample localities of a [`gData`] to a ggplot as a [ggplot2::geom_sf()]
#' layer.
#'
#' @param mapping aesthetics.
#' @param data a [`gData`] object.
#' @param stat,position,na.rm,show.legend,... forwarded to [ggplot2::geom_sf()].
#' @param original logical. If TRUE, plot at the original sample locations;
#'   if FALSE (default), plot at the assigned-node coordinates on the linked
#'   gGraph.
#' @return a ggplot layer.
#' @examplesIf requireNamespace("ggplot2", quietly = TRUE)
#' library(ggplot2)
#' ggplot() +
#'  geom_gdata(data = hgdp, colour = "black", size = 1.5) +
#'  theme_void()
#' @export
#' @family ggplot_methods
geom_gdata <- function(mapping = ggplot2::aes(), data = NULL,
                       original = FALSE,
                       stat = "sf", position = "identity",
                       na.rm = FALSE, show.legend = NA, ...) {
  if (is.null(data))            stop("data (a gData) must be specified")
  if (!inherits(data, "gData")) stop("data must be a gData object")
  
  ggplot2::geom_sf(mapping = mapping,
                   data = .dfToSfPoints(.gdataDf(data, original = original)),
                   stat = stat, position = position,
                   na.rm = na.rm, show.legend = show.legend,
                   inherit.aes = FALSE, ...)
}


#' ggplot layer for a gPath
#'
#' Adds a [`gPath`] (from [dijkstraBetween()] or [dijkstraFrom()]) to a ggplot
#' as [ggplot2::geom_sf()] linestrings.
#'
#' @param mapping aesthetics.
#' @param data a `gPath` object.
#' @param stat,position,na.rm,show.legend,... forwarded to [ggplot2::geom_sf()].
#' @return a ggplot layer.
#' @examplesIf requireNamespace("ggplot2", quietly = TRUE)
#' library(ggplot2)
#' addis <- list(lon = 38.74, lat = 9.03)
#' addis_node <- closestNode(worldgraph.40k, addis)
#' myPath <- dijkstraFrom(hgdp, addis_node)
#' ggplot() +
#'   geom_ggraph(data = worldgraph.40k, aes(colour = habitat),
#'               edges = FALSE, size = 1, show.legend = FALSE) +
#'   scale_colour_manual(values = c(land = "grey70", sea = "lightblue", coast = "grey70")) +
#'   geom_gpath(data = myPath, colour = "firebrick", linewidth = 0.4) +
#'   geom_gdata(data = hgdp, colour = "black", size = 1.5) +
#'   coord_sf(crs = "+proj=ortho +lat_0=40 +lon_0=30") +
#'   theme_void()
#' @export
#' @family ggplot_methods
geom_gpath <- function(mapping = ggplot2::aes(), data = NULL,
                       stat = "sf", position = "identity",
                       na.rm = FALSE, show.legend = NA, ...) {
  if (is.null(data))         stop("data (a gPath) must be specified")
  if (!inherits(data, "gPath")) stop("data must be a gPath object")
  
  ggplot2::geom_sf(mapping = mapping,
                   data = .dfToSfPaths(.gpathDf(data)),
                   stat = stat, position = position,
                   na.rm = na.rm, show.legend = show.legend,
                   inherit.aes = FALSE, ...)
}


## autoplot methods

#' Default ggplot for a [`gGraph`]
#'
#' @param object a [`gGraph`].
#' @param mode `"flat"` (default) or `"orthographic"`.
#' @param lon0,lat0 view centre (orthographic only).
#' @param edges logical; whether to draw edges.
#' @param ... unused.
#' @return a ggplot.
#' @importFrom ggplot2 autoplot
#' @examples
#' autoplot(worldgraph.10k)
#' @export
#' @family ggplot_methods
autoplot.gGraph <- function(object, mode = c("flat", "orthographic"),
                            lon0 = 0, lat0 = 20, edges = TRUE, ...) {
  mode <- match.arg(mode)
  crs  <- if (mode == "orthographic")
    sprintf("+proj=ortho +lat_0=%s +lon_0=%s", lat0, lon0) else 4326
  
  color_col <- if (!is.null(object@meta$colors)) colnames(object@meta$colors)[1]
  mapping   <- if (!is.null(color_col))
    ggplot2::aes(colour = .data[[color_col]]) else ggplot2::aes()
  
  gg <- ggplot2::ggplot() +
    geom_ggraph(mapping = mapping, data = object, edges = edges, size = 0.3)
  
  if (!is.null(color_col)) {
    rules <- getColors(object, res.type = "rules")
    gg <- gg + ggplot2::scale_colour_manual(
      values = stats::setNames(rules$color, rules[[color_col]])
    )
  }
  gg + ggplot2::coord_sf(crs = crs) + ggplot2::theme_minimal()
}


#' Default ggplot for a [`gData`]
#'
#' @param object a [`gData`].
#' @param mode,lon0,lat0 as in [autoplot.gGraph()].
#' @param show.gGraph logical; overlay the linked gGraph if available.
#' @param node.size point size for the gData localities.
#' @param node.color point colour for the gData localities.
#' @param ... unused.
#' @return a ggplot.
#' @importFrom ggplot2 autoplot
#' @examples
#' autoplot(hgdp)
#' @export
#' @family ggplot_methods
autoplot.gData <- function(object, mode = c("flat", "orthographic"),
                           lon0 = 0, lat0 = 20, show.gGraph = TRUE,
                           node.size = 2, node.color = "darkred", ...) {
  mode <- match.arg(mode)
  crs  <- if (mode == "orthographic")
    sprintf("+proj=ortho +lat_0=%s +lon_0=%s", lat0, lon0) else 4326
  
  gg <- ggplot2::ggplot()
  
  if (show.gGraph && exists(object@gGraph.name, envir = .GlobalEnv)) {
    parent_graph <- get(object@gGraph.name, envir = .GlobalEnv)
    
    color_col <- if (!is.null(parent_graph@meta$colors))
      colnames(parent_graph@meta$colors)[1]
    
    if (!is.null(color_col)) {
      rules <- getColors(parent_graph, res.type = "rules")
      gg <- gg +
        geom_ggraph(mapping = ggplot2::aes(colour = .data[[color_col]]),
                    data = parent_graph, edges = FALSE, size = 0.2) +
        ggplot2::scale_colour_manual(
          values = stats::setNames(rules$color, rules[[color_col]])
        )
    } else {
      gg <- gg + geom_ggraph(data = parent_graph, edges = FALSE, size = 0.2)
    }
  }
  
  gg + geom_gdata(data = object, colour = node.color, size = node.size) +
    ggplot2::coord_sf(crs = crs) + ggplot2::theme_minimal()
}


#' Internal function to convert a gGraph to a data.frame for ggplot
#' @noRd
.ggraphNodesDf <- function(g) {
  cbind(data.frame(node_id = getNodes(g)),
        as.data.frame(getCoords(g)),
        getNodesAttr(g))
}

#' Internal function to convert a gGraph's edges to a data.frame for ggplot
#' @noRd
.ggraphEdgesDf <- function(g) {
  E <- getEdges(g, res.type = "matNames", unique = TRUE)
  co <- getCoords(g)
  data.frame(from = E[, 1], to = E[, 2],
             x    = co[E[, 1], 1], y    = co[E[, 1], 2],
             xend = co[E[, 2], 1], yend = co[E[, 2], 2])
}

#' Internal function to convert a gData to a data.frame for ggplot
#' @noRd
.gdataDf <- function(gd, original = FALSE) {
  co <- as.data.frame(getCoords(gd, original = original))
  colnames(co) <- c("lon", "lat")
  
  df <- cbind(data.frame(node_id = gd@nodes.id), co)
  if (!is.null(gd@data) && length(gd@data) > 0 && nrow(gd@data) > 0) {
    df <- cbind(df, gd@data)
  }
  df
}

#' Internal function to convert a gPath to a data.frame for ggplot
#' @noRd
.gpathDf <- function(gp) {
  xy <- attr(gp, "xy")
  do.call(rbind, lapply(seq_along(gp), function(k) {
    nd <- gp[[k]]$path_detail
    if (length(nd) < 2) return(NULL)
    data.frame(path_id = k, order = seq_along(nd),
               lon = xy[nd, 1], lat = xy[nd, 2])
  }))
}


#' Internal functions to convert data.frames to sf objects for ggplot
#' @noRd
.dfToSfPoints <- function(df) {
  sf::st_as_sf(df, coords = c("lon", "lat"), crs = 4326)
}

#' Internal function to convert a data.frame of edges to sf linestrings for ggplot
#' @noRd
.dfToSfLines <- function(df) {
  lines <- lapply(seq_len(nrow(df)), function(i) {
    sf::st_linestring(rbind(c(df$x[i], df$y[i]),
                            c(df$xend[i], df$yend[i])))
  })
  sf::st_sf(from = df$from, to = df$to,
            geometry = sf::st_sfc(lines, crs = 4326))
}

#' Internal function to convert a data.frame of paths to sf linestrings for ggplot
#' @noRd
.dfToSfPaths <- function(df) {
  by_path <- split(df, df$path_id)
  lines <- lapply(by_path, function(p) sf::st_linestring(cbind(p$lon, p$lat)))
  sf::st_sf(path_id = names(by_path),
            geometry = sf::st_sfc(lines, crs = 4326))
}

#' @importFrom ggplot2 autoplot
#' @export
ggplot2::autoplot