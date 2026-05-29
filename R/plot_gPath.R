#' Plot a gPath object
#'
#' This method plots a [`gPath`] object, which is the output of the [`dijkstraBetween`],
#' [`polygonBetween`] and [`dijkstraFrom`] functions.
#'
#' @param x a [`gPath`] object
#' @param col a character string indicating a color or a palette of colors to
#' be used for plotting edges.
#' @param lwd a numeric value indicating the width of edges.
#' @param seed an optional integer value to set the seed for random color generation
#' when `col = "rainbow"`.
#' @param ... further arguments passed to [`geo.segments`].
#' @return NULL.
#' @family plotting_methods
#' @examples
#' hgdp.sub <- hgdp[getData(hgdp)$Population %in%
#'   c("French", "Balochi", "BantuKenya", "Papuan", "Pima")]
#' hgdp.path <- dijkstraBetween(hgdp.sub) # compute shortest path
#'
#' ## plotting
#' plot(worldgraph.40k, reset = TRUE, pch = "")
#' points(hgdp.sub, lwd = 1) # plot populations
#' plot(hgdp.path) # plot the path
#'
#' ## printing
#' print(hgdp.path)
#' ################
#' # plot method
#' ################
#' @method plot gPath
#' @export
plot.gPath <- function(x, col = "rainbow", lwd = 3, seed = NULL, ...) {
  listNodes <- lapply(x, function(e) e$path_detail)

  ## xy <- x$xy
  xy <- attr(x, "xy")
  Npath <- length(listNodes)

  ## handle color ##
  if (is.character(col) && col[1] == "rainbow") {
    if (!is.null(seed)) set.seed(seed)
    col <- sample(grDevices::rainbow(length(x)))
  }
  col <- rep(col, length = Npath)
  lwd <- rep(lwd, length = Npath)

  ## function plotting one gPath
  f1 <- function(vecNodes, col, lwd, ...) {
    N <- length(vecNodes)
    if (N < 2) {
      return()
    } # escape if a path is a single vertex
    from <- vecNodes[seq_len(N - 1)]
    to <- vecNodes[2:N]
    ## segments(xy[from,1], xy[from,2], xy[to,1], xy[to,2], col=col, lwd=lwd, ...)
    geo.segments(xy[from, 1], xy[from, 2], xy[to, 1], xy[to, 2], col = col, lwd = lwd, ...)
  }


  ## plot all gPaths
  lapply(seq_along(listNodes), function(i) f1(listNodes[[i]], col = col[i], lwd = lwd[i], ...))

  return(invisible())
} # end plot.gPath


#################
## print method
#################
#' @describeIn plot.gPath Print a summary of a gPath object
#' @method print gPath
#' @export

print.gPath <- function(x, ...) {
  if (length(list(...))) {
    stop("additional parameters were passed through ... when none should be given")
  }
  ## printing
  cat("\n=== gPath object ===\n")
  cat("\n number of paths:", length(x), "\n")
  cat("\n available paths (id_origin:id_destination): ")
  cat(c(
    utils::head(names(x), n = 3L),
    ifelse(length(names(x)) > 3, "...\n", "\n")
  ))
  cat("\neach path, accessible with [[]] has elements 'length', 'path_detail' and 'length_detail'\n")
  cat("x and y coordinates of all nodes are stored as an attribute 'xy'; ")
  cat("see ?gPath for details")
}
