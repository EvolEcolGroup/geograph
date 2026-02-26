#' Collapse a list-based node attribute into a scalar node attribute
#'
#' @description
#' Collapses a list-based node attribute (e.g. raster values assigned to each
#' node) into a single scalar value per node. Optionally replaces the original
#' attribute to reduce memory usage.
#'
#' @param graph A \linkS4class{gGraph} object.
#' @param attribute Character string giving the name of the node attribute
#'   to collapse.
#' @param fun A function or a character string specifying how to collapse values.
#'   Built-in options include \code{"min"}, \code{"max"}, \code{"mean"},
#'   \code{"median"}, \code{"sd"}, \code{"any"}, and \code{"all"}.
#' @param na.rm Logical; whether to remove \code{NA} values before collapsing.
#'   Defaults to \code{TRUE}.
#' @param replace Logical; if \code{TRUE} (default), replaces the original
#'   list-based node attribute with the collapsed vector. If \code{FALSE},
#'   the collapsed vector is returned but the graph is left unchanged.
#' @param ... Additional arguments passed to \code{fun}.
#'
#' @return
#' If \code{replace = TRUE}, a modified \linkS4class{gGraph} object with the
#' collapsed attribute replacing the original one.
#'
#' If \code{replace = FALSE}, a vector of collapsed values (one per node).
#'
#' @export
collapseNodeAttribute <- function(graph,
                                  attribute,
                                  fun = c("mean", "max", "min", "median", "sd", "any", "all"),
                                  na.rm = TRUE,
                                  replace = TRUE,
                                  ...) {
  
  if (!inherits(graph, "gGraph")) {
    stop("`graph` must be a gGraph object.")
  }
  
  if (!attribute %in% names(graph@nodes.attr)) {
    stop(sprintf("Node attribute '%s' not found.", attribute))
  }
  
  # Get the node attribute and check the structure
  x <- graph@nodes.attr[[attribute]]
  
  if (!is.list(x)) {
    stop("Selected node attribute must be a list (nested tibble structure).")
  }
  
  # Resolve built-in functions
  if (is.character(fun)) {
    fun <- match.arg(fun)
    
    fun <- switch(
      fun,
      min    = min,
      max    = max,
      mean   = mean,
      median = median,
      sd     = stats::sd,
      any    = any,
      all    = all
    )
  }
  
  if (!is.function(fun)) {
    stop("`fun` must be a function or a supported character string.")
  }
  
  # Determine return type once (logical or numeric)
  test_val <- fun(c(1, 2), na.rm = TRUE)
  FUN.VALUE <- if (is.logical(test_val)) logical(1) else numeric(1)
  
  # Collapse one node safely
  .collapse_one_node <- function(df, fun, na.rm, ...) {
    
    if (is.null(df) || length(df) == 0 || nrow(df) == 0) {
      return(NA)
    }
    
    if (ncol(df) == 1) {
      vals <- df[[1]]
    } else {
      stop("Node data has multiple columns; cannot infer value column.")
    }
    
    if (length(vals) == 0 || all(is.na(vals))) {
      return(NA)
    }
    
    fun(vals, na.rm = na.rm, ...)
  }
  
  # now collapse all nodes (node by node)
  collapsed <- vapply(
    x,
    .collapse_one_node,
    FUN.VALUE = FUN.VALUE,
    fun = fun,
    na.rm = na.rm,
    ...
  )
  
  # return either modified graph or collapsed vector
  if (replace) {
    graph@nodes.attr[[attribute]] <- collapsed
    return(graph)
  } else {
    return(collapsed)
  }
}
