###############################################################################
## R/plot_ggplot.R
##
## ggplot2 support for geoGraph classes.
##
## Idea:
##   S4 object -> tidy data.frame -> sf object -> geom_sf layer
##
###############################################################################


## convert gGraph/gData/gPath to data.frame for ggplot

.ggraphNodesDf <- function(g) {
  cbind(data.frame(node_id = getNodes(g)),
        as.data.frame(getCoords(g)),
        getNodesAttr(g))
}

.ggraphEdgesDf <- function(g) {
  E <- getEdges(g, res.type = "matNames", unique = TRUE)
  co <- getCoords(g)
  data.frame(from = E[, 1], to = E[, 2],
             x    = co[E[, 1], 1], y    = co[E[, 1], 2],
             xend = co[E[, 2], 1], yend = co[E[, 2], 2])
}

.gdataDf <- function(gd, original = FALSE) {
  co <- as.data.frame(getCoords(gd, original = original))
  colnames(co) <- c("lon", "lat")
  
  df <- cbind(data.frame(node_id = gd@nodes.id), co)
  if (!is.null(gd@data) && length(gd@data) > 0 && nrow(gd@data) > 0) {
    df <- cbind(df, gd@data)
  }
  df
}

.gpathDf <- function(gp) {
  xy <- attr(gp, "xy")
  do.call(rbind, lapply(seq_along(gp), function(k) {
    nd <- gp[[k]]$path_detail
    if (length(nd) < 2) return(NULL)
    data.frame(path_id = k, order = seq_along(nd),
               lon = xy[nd, 1], lat = xy[nd, 2])
  }))
}


## convert data.frame to sf object

.dfToSfPoints <- function(df) {
  sf::st_as_sf(df, coords = c("lon", "lat"), crs = 4326)
}

.dfToSfLines <- function(df) {
  lines <- lapply(seq_len(nrow(df)), function(i) {
    sf::st_linestring(rbind(c(df$x[i], df$y[i]),
                            c(df$xend[i], df$yend[i])))
  })
  sf::st_sf(from = df$from, to = df$to,
            geometry = sf::st_sfc(lines, crs = 4326))
}

.dfToSfPaths <- function(df) {
  by_path <- split(df, df$path_id)
  lines <- lapply(by_path, function(p) sf::st_linestring(cbind(p$lon, p$lat)))
  sf::st_sf(path_id = names(by_path),
            geometry = sf::st_sfc(lines, crs = 4326))
}


## exported ggplot layers for gGraph/gData/gPath 

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
#' @export
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
#' @param coords whether to use the original coordinates of the gData ("original") or the
#'   coordinates of the assigned nodes in the linked gGraph. Defaults to "nodes".
#' @export
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
#' @export
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
#' @export
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
#' @export
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