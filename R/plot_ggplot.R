#' ggplot layer for a [`gGraph`]
#'
#' Adds nodes (and optionally edges) of a [`gGraph`] to a ggplot as
#' [ggplot2::geom_sf()] layers.
#'
#' @param data a [`gGraph`] object.
#' @param mapping aesthetics. Variables from the `gGraph's` node attributes may
#'   be used (e.g. `aes(color = habitat)`).
#' @param edges logical; whether to draw edges. Defaults to `FALSE`.
#' @param stat,position,na.rm,show.legend,... forwarded to [ggplot2::geom_sf()].
#' @return one or two ggplot layers (edges under nodes).
#' @examplesIf requireNamespace("ggplot2", quietly = TRUE)
#' library(ggplot2)
#' ggplot() +
#'   geom_ggraph(data = worldgraph.10k, aes(color = habitat), edges = TRUE, size = 0.3) +
#'   scale_color_manual(values = c(land = "grey70", sea = "lightblue", coast = "grey70")) +
#'   coord_sf(crs = "+proj=ortho +lat_0=40 +lon_0=-80") +
#'   theme_void()
#' @export
#' @family ggplot_methods
geom_ggraph <- function(data = NULL, mapping = ggplot2::aes(), edges = FALSE,
                        stat = "sf", position = "identity",
                        na.rm = FALSE, show.legend = NA, ...) {
  if (is.null(data)) stop("data (a gGraph) must be specified")
  if (!inherits(data, "gGraph")) stop("data must be a gGraph object")

  layers <- list()

  # handle the edges layer
  if (edges) {
    layers <- c(layers, list(
      ggplot2::geom_sf(
        data = .dfToSfLines(.ggraphEdgesDf(data)),
        color = "grey65", linewidth = 0.4,
        inherit.aes = FALSE
      )
    ))
  }

  # make the node layer
  layers <- c(layers, list(
    ggplot2::geom_sf(
      mapping = mapping,
      data = .dfToSfPoints(.ggraphNodesDf(data)),
      stat = stat, position = position,
      na.rm = na.rm, show.legend = show.legend,
      inherit.aes = FALSE, ...
    )
  ))
  layers
}


#' ggplot layer for a [`gData`]
#'
#' Adds sample localities of a [`gData`] to a ggplot as a [ggplot2::geom_sf()]
#' layer.
#'
#' @param data a [`gData`] object.
#' @param mapping aesthetics.
#' @param stat,position,na.rm,show.legend,... forwarded to [ggplot2::geom_sf()].
#' @param original logical. If TRUE, plot at the original sample locations;
#'   if FALSE (default), plot at the assigned-node coordinates on the linked
#'   gGraph.
#' @return a ggplot layer.
#' @examplesIf requireNamespace("ggplot2", quietly = TRUE)
#' library(ggplot2)
#' ggplot() +
#'   geom_gdata(data = hgdp, color = "black", size = 1.5) +
#'   theme_void()
#' @export
#' @family ggplot_methods
geom_gdata <- function(data = NULL, mapping = ggplot2::aes(),
                       original = FALSE,
                       stat = "sf", position = "identity",
                       na.rm = FALSE, show.legend = NA, ...) {
  if (is.null(data)) stop("data (a gData) must be specified")
  if (!inherits(data, "gData")) stop("data must be a gData object")

  ggplot2::geom_sf(
    mapping = mapping,
    data = .dfToSfPoints(.gdataDf(data, original = original)),
    stat = stat, position = position,
    na.rm = na.rm, show.legend = show.legend,
    inherit.aes = FALSE, ...
  )
}


#' ggplot layer for a gPath
#'
#' Adds a [`gPath`] (from [dijkstraBetween()] or [dijkstraFrom()]) to a ggplot
#' as [ggplot2::geom_sf()] linestrings.
#'
#' @param data a `gPath` object.
#' @param mapping aesthetics.
#' @param stat,position,na.rm,show.legend,... forwarded to [ggplot2::geom_sf()].
#' @return a ggplot layer.
#' @examplesIf requireNamespace("ggplot2", quietly = TRUE)
#' library(ggplot2)
#' addis <- list(lon = 38.74, lat = 9.03)
#' addis_node <- closestNode(worldgraph.40k, addis)
#' myPath <- dijkstraFrom(hgdp, addis_node)
#' ggplot() +
#'   geom_ggraph(
#'     data = worldgraph.40k, aes(color = habitat),
#'     edges = FALSE, size = 1, show.legend = FALSE
#'   ) +
#'   scale_color_manual(values = c(land = "grey70", sea = "lightblue", coast = "grey70")) +
#'   geom_gpath(data = myPath, color = "firebrick", linewidth = 0.4) +
#'   geom_gdata(data = hgdp, color = "black", size = 1.5) +
#'   coord_sf(crs = "+proj=ortho +lat_0=40 +lon_0=30") +
#'   theme_void()
#' @export
#' @family ggplot_methods
geom_gpath <- function(data = NULL, mapping = ggplot2::aes(),
                       stat = "sf", position = "identity",
                       na.rm = FALSE, show.legend = NA, ...) {
  if (is.null(data)) stop("data (a gPath) must be specified")
  if (!inherits(data, "gPath")) stop("data must be a gPath object")

  ggplot2::geom_sf(
    mapping = mapping,
    data = .dfToSfPaths(.gpathDf(data)),
    stat = stat, position = position,
    na.rm = na.rm, show.legend = show.legend,
    inherit.aes = FALSE, ...
  )
}


## autoplot methods

#' Default ggplot for a [`gGraph`]
#'
#' @param object a [`gGraph`].
#' @param edges logical; whether to draw edges.
#' @param ... unused.
#' @return a ggplot.
#' @importFrom ggplot2 autoplot
#' @examples
#' autoplot(worldgraph.10k)
#' @export
#' @family ggplot_methods
autoplot.gGraph <- function(object, edges = TRUE, ...) {
  color.col <- if (!is.null(object@meta$colors)) colnames(object@meta$colors)[1]

  if (!is.null(color.col)) {
    rules <- getColors(object, res.type = "rules")
    ggplot2::ggplot() +
      geom_ggraph(
        mapping = ggplot2::aes(color = .data[[color.col]]),
        data = object, edges = edges, size = 0.3
      ) +
      ggplot2::scale_color_manual(
        values = stats::setNames(rules$color, rules[[color.col]])
      ) +
      ggplot2::coord_sf() +
      ggplot2::theme_minimal()
  } else {
    ggplot2::ggplot() +
      geom_ggraph(data = object, edges = edges, size = 0.3) +
      ggplot2::coord_sf() +
      ggplot2::theme_minimal()
  }
}

#' Default ggplot for a [`gData`]
#'
#' @param object a [`gData`].
#' @param show.gGraph logical; overlay the linked gGraph if available.
#' @param edges logical; whether to draw edges.
#' @param ... unused.
#' @return a ggplot.
#' @importFrom ggplot2 autoplot
#' @examples
#' autoplot(hgdp)
#' @export
#' @family ggplot_methods
autoplot.gData <- function(object, show.gGraph = TRUE, edges = FALSE, ...) {
  gg <- ggplot2::ggplot()

  # overlay the linked gGraph with its colors, if available
  if (show.gGraph && exists(object@gGraph.name, envir = .GlobalEnv)) {
    parentGraph <- get(object@gGraph.name, envir = .GlobalEnv)
    color.col <- if (!is.null(parentGraph@meta$colors)) {
      colnames(parentGraph@meta$colors)[1]
    }

    if (!is.null(color.col)) {
      rules <- getColors(parentGraph, res.type = "rules")
      gg <- gg +
        geom_ggraph(
          mapping = ggplot2::aes(color = .data[[color.col]]),
          data = parentGraph, edges = edges, size = 0.2
        ) +
        ggplot2::scale_color_manual(
          values = stats::setNames(rules$color, rules[[color.col]])
        )
    } else {
      gg <- gg + geom_ggraph(data = parentGraph, edges = edges, size = 0.2)
    }
  }

  gg + geom_gdata(data = object) +
    ggplot2::coord_sf() + ggplot2::theme_minimal()
}

#' Internal function to convert a gGraph to a data.frame for ggplot
#' @noRd
.ggraphNodesDf <- function(g) {
  cbind(
    data.frame(node_id = getNodes(g)),
    as.data.frame(getCoords(g)),
    getNodesAttr(g)
  )
}

#' Internal function to convert a `gGraph's` edges to a data.frame for ggplot
#' @noRd
.ggraphEdgesDf <- function(g) {
  ## one direction per edge, chosen by comparing node names as
  ## getEdges(unique = TRUE) does
  et <- .edgeTable(getGraph(g), weights = FALSE)
  nN <- getNodes(g)
  keep <- nN[et$from] < nN[et$to]
  from <- et$from[keep]
  to <- et$to[keep]
  co <- getCoords(g)
  data.frame(
    from = nN[from], to = nN[to],
    x = co[from, 1], y = co[from, 2],
    xend = co[to, 1], yend = co[to, 2]
  )
}

#' Internal function to convert a gData to a data.frame for ggplot
#' @noRd
.gdataDf <- function(gd, original = FALSE) {
  co <- as.data.frame(getCoords(gd, original = original))
  colnames(co) <- c("lon", "lat")

  df <- cbind(data.frame(node_id = gd@nodes.id), co)
  dat <- gd@data
  if (length(dim(dat)) == 2 && nrow(dat) == nrow(df)) { # data.frame or matrix
    df <- cbind(df, dat)
  } else if (is.atomic(dat) && is.null(dim(dat)) && length(dat) == nrow(df)) { # vector
    df$data <- dat
  }
  df
}

#' Internal function to convert a gPath to a data.frame for ggplot
#' @noRd
.gpathDf <- function(gp) {
  xy <- attr(gp, "xy")
  nd <- lapply(unclass(gp), `[[`, "path_detail")
  len <- lengths(nd)
  keep <- len >= 2 # paths with at least one step
  if (!any(keep)) {
    return(NULL)
  }
  nodes <- unlist(nd[keep], use.names = FALSE)
  res <- data.frame(
    path_id = rep.int(which(keep), len[keep]), order = sequence(len[keep]),
    lon = unname(xy[nodes, 1]), lat = unname(xy[nodes, 2])
  )
  ## row names as given by rbind() of one data frame per path
  rownames(res) <- make.unique(nodes, sep = "")
  res
}


#' Internal functions to convert data.frames to sf objects for ggplot
#' @noRd
.dfToSfPoints <- function(df) {
  sf::st_as_sf(df, coords = c("lon", "lat"), crs = 4326)
}

#' Internal function to convert a data.frame of edges to sf linestrings for ggplot
#' @noRd
.dfToSfLines <- function(df) {
  ## build the 2 x 2 coordinate matrix of each segment and set its class
  ## directly (what sf::st_linestring() returns, without its checks)
  m <- cbind(df$x, df$xend, df$y, df$yend)
  lines <- lapply(seq_len(nrow(df)), function(i) {
    structure(matrix(m[i, ], 2, 2), class = c("XY", "LINESTRING", "sfg"))
  })
  sf::st_sf(
    from = df$from, to = df$to,
    geometry = sf::st_sfc(lines, crs = 4326)
  )
}

#' Internal function to convert a data.frame of paths to sf linestrings for ggplot
#' @noRd
.dfToSfPaths <- function(df) {
  co <- cbind(df$lon, df$lat)
  by.path <- split(seq_len(nrow(df)), df$path_id)
  lines <- lapply(by.path, function(i) sf::st_linestring(co[i, , drop = FALSE]))
  sf::st_sf(
    path_id = names(by.path),
    geometry = sf::st_sfc(lines, crs = 4326)
  )
}

#' @importFrom ggplot2 autoplot
#' @export
ggplot2::autoplot
