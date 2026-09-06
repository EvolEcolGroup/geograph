#' @include classes.R
NULL

#################
## BASIC METHODS
#################

############
## [ gGraph
############

#' Subset a gGraph object
#'
#' Select a subset of nodes from a [`gGraph`] object by index, logical vector,
#' or node name. The `@coords`, `@nodes.attr`, and `@graph` slots are all
#' subsetted consistently.
#'
#' @param x a valid [`gGraph`] object.
#' @param i indices for subsetting nodes — a logical vector, integer indices,
#'   or character node names. If missing, all nodes are kept.
#' @param j indices for subsetting node attributes (columns of `@nodes.attr`).
#'   If missing, all attributes are kept.
#' @param ... additional arguments (currently unused).
#' @param drop logical, currently unused.
#' @return A [`gGraph`] object containing only the selected nodes.
#' @seealso [`getNodes`] to retrieve node names. [`isInArea`] to select nodes
#'   within a geographic area.
#' @aliases [,gGraph-method [,gGraph,ANY,ANY-method [,gGraph,ANY,ANY,ANY-method
#' @exportMethod "["
#' @family basic_methods
#' @name subset-gGraph
#' @examples
#' ## subset to nodes in a geographic area
#' plot(worldgraph.10k, reset = TRUE)
#' geo.zoomin(list(x = c(-6, 38), y = c(35, 73)))
#' x <- worldgraph.10k[isInArea(worldgraph.10k, quiet = TRUE)]
#'
#' ## subset by node name
#' x <- worldgraph.10k[c("1", "2", "3")]
NULL

setMethod("[", "gGraph", function(x, i, j, ..., drop = TRUE) {
  if (missing(i)) {
    i <- TRUE
  }
  if (is.logical(i)) {
    i <- rep(i, length = nrow(getCoords(x)))
  }
  if (is.character(i)) {
    i <- match(i, getNodes(x))
    if (any(is.na(i))) stop("Some specified node labels were not found.")
  }
  if (missing(j)) {
    j <- TRUE
  }
  if (is.logical(i) && is.logical(j) && all(c(i, j))) {
    return(x)
  }

  ## do the subsetting ##
  res <- x
  res@coords <- res@coords[i, , drop = FALSE]
  if (nrow(res@nodes.attr) > 0) {
    res@nodes.attr <- res@nodes.attr[i, j, drop = FALSE]
  }
  res@graph <- subGraph(nodes(res@graph)[i], res@graph)

  return(res)
})


###########
## [ gData
###########

#' Subset a gData object
#'
#' Select a subset of locations from a [`gData`] object by index, logical
#' vector, or node name. The `@coords`, `@nodes.id`, and `@data` slots are all
#' subsetted consistently.
#'
#' @param x a valid [`gData`] object.
#' @param i indices for subsetting locations — a logical vector, integer
#'   indices, or character node names. If missing, all locations are kept.
#' @param j indices for subsetting data columns. If missing, all columns
#'   are kept.
#' @param ... additional arguments passed to the `[` method of `@data`.
#' @param drop logical passed to the `[` method of `@data`. Defaults to
#'   `FALSE`.
#' @return A [`gData`] object with `@coords`, `@nodes.id`, and `@data` all
#'   subsetted consistently.
#' @seealso [`getCoords`], [`getNodes`], [`getData`]
#' @aliases [,gData-method [,gData,ANY,ANY-method [,gData,ANY,ANY,ANY-method
#' @exportMethod "["
#' @family basic_methods
#' @name subset-gData
#' @examples
#' ## subset to northern hemisphere populations
#' north <- hgdp[hgdp@data$Latitude > 40]
#' plot(worldgraph.40k, reset = TRUE)
#' points(north)
NULL

setMethod("[", "gData", function(x, i, j, ..., drop = FALSE) {
  if (missing(i)) {
    i <- TRUE
  }
  if (is.logical(i)) {
    i <- rep(i, length = nrow(getCoords(x)))
  }
  if (is.character(i)) {
    i <- match(i, getNodes(x))
    if (any(is.na(i))) stop("Some specified node labels were not found.")
  }
  if (missing(j)) {
    j <- TRUE
  }
  if (is.logical(i) && is.logical(j) && all(c(i, j))) {
    return(x)
  }

  ## do the subsetting ##
  res <- x
  N <- nrow(res@coords)
  res@coords <- res@coords[i, , drop = FALSE]
  res@nodes.id <- res@nodes.id[i]

  if (!is.null(getData(x))) {
    if (nrow(getData(x)) == N) {
      res@data <- res@data[i, j, drop = FALSE]
    } else if (length(getData(x)) == N) {
      res@data <- res@data[i]
    } else if (existsMethod("[", class(res@data)[1])) {
      res@data <- res@data[i, j, ..., drop = drop]
    } else {
      warning("Don't know what to do with @data.")
    }
  }

  return(res)
})


################
## SHOW METHODS
################

###############
## show gGraph
###############

setMethod("show", "gGraph", function(object) {
  x <- object
  N <- nrow(x@coords)
  nDisp <- 3

  cat("\n=== gGraph object ===\n")
  cat("\n@coords: spatial coordinates of", nrow(x@coords), "nodes\n")
  print(utils::head(x@coords, nDisp))
  if (N > nDisp) cat("...\n")

  cat("\n@nodes.attr:", ncol(x@nodes.attr), "nodes attributes\n")
  print(utils::head(x@nodes.attr, nDisp))
  if (nrow(x@nodes.attr) > nDisp) cat("...\n")

  cat("\n@meta: list of meta information with", length(x@meta), "items\n")
  if (length(x@meta) > 0) print(paste("$", names(x@meta), sep = ""))

  cat("\n@graph:\n")
  print(x@graph)
})


###############
## show gData
###############

setMethod("show", "gData", function(object) {
  x <- object
  N <- nrow(x@coords)
  nDisp <- 3

  cat("\n=== gData object ===\n")
  cat("\n@coords: spatial coordinates of", nrow(x@coords), "nodes\n")
  print(utils::head(x@coords, nDisp))
  if (N > nDisp) cat("...\n")

  cat("\n@nodes.id:", length(x@nodes.id), "nodes identifiers\n")
  print(utils::head(x@nodes.id, nDisp))
  if (length(x@nodes.id) > nDisp) cat("...\n")

  cat("\n@data:", nrow(x@data), "data\n")
  print(utils::head(x@data, nDisp))
  if (N > nDisp) cat("...\n")

  cat("\nAssociated gGraph:", x@gGraph.name, "\n")
})
