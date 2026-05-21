#' Set friction in a gGraph object
#'
#' The function `setCosts` defines costs for the edges of a [`gGraph`] object
#' according to a node attribute and cost rules defined in `@meta$costs`. Each
#' node has a value for the chosen attribute which is associated to a cost. The
#' cost of an edge is computed as a function of the costs of its two nodes.
#'
#' Costs are inversely proportional to connectivity: the larger the cost of an
#' edge, the lower the connectivity between its two nodes. Costs in `geoGraph`
#' are equivalent to weights in the `graph` and `RBGL` packages.
#'
#' @param x a [`gGraph`] object with at least one node attribute and a
#'   `@meta$costs` component (see `worldgraph.10k` for an example).
#' @param attr.name the name of the node attribute used to compute costs.
#' @param node.values a numeric vector of costs for the nodes. If provided,
#'   overrides `attr.name`.
#' @param cost.rules a two-column `data.frame` to update `x@meta$costs`
#'   before computing edge costs. If `NULL`, existing `x@meta$costs` is used.
#' @param method how edge costs are computed from node costs: `"mean"`,
#'   `"product"`, or `"function"` (requires `FUN`).
#' @param FUN a function to compute edge cost from two node costs. Required
#'   when `method = "function"`.
#' @param ... additional arguments passed to `FUN`.
#' @return A [`gGraph`] object with the newly defined edge costs.
#' @seealso [`dropDeadEdges`], [`getCosts`], [`hasCosts`]
#' @family cost_functions
#' @examples
#' ## get and modify cost rules then set costs in one call
#' cost.rules <- getCosts(worldgraph.10k, res.type = "rules")
#' cost.rules
#'
#' ## make sea travel cheaper
#' cost.rules$cost[cost.rules$habitat == "sea"] <- 50
#'
#' ## update rules and set costs in one call
#' x <- setCosts(worldgraph.10k, attr.name = "habitat", cost.rules = cost.rules)
#' @export
setCosts <- function(x, attr.name = NULL, node.values = NULL, cost.rules = NULL,
                     method = c("mean", "product", "function"), FUN = NULL, ...) {
  ## some checks + argument handling
  if (!is.gGraph(x)) stop("x is not a valid gGraph object")
  method <- match.arg(method)
  if ((method == "function") && (is.null(FUN))) {
    stop("if method='function', FUN needs to be defined.")
  }
  
  if (!is.null(cost.rules)) {
    if (!is.data.frame(cost.rules)) stop("cost.rules must be a data.frame.")
    if (ncol(cost.rules) != 2) stop("cost.rules must have exactly two columns.")

    # Validate the second column is numeric with no NAs
    if (!is.numeric(cost.rules[[2]])) {
      stop("The second column of cost.rules must be numeric.")
    }
    if (any(is.na(cost.rules[[2]]))) {
      stop("The second column of cost.rules contains NA values.")
    }

    # Validate first column contains unique node identifiers
    if (any(duplicated(cost.rules[[1]]))) {
      stop("The first column of cost.rules must contain unique values.")
    }

    # Check that values in first column match existing node attribute values
    # Note: we can't validate against x@nodes$id directly since nodes.attr
    # uses the attribute name as the column. We'll validate during usage in setCosts.
    # For now, just ensure uniqueness and type validity.

    x@meta$costs <- cost.rules
  }
  
  ## assign costs to vertices
  if (is.null(node.values)) { # costs from a node attribute
    nodeAttr <- unlist(getNodesAttr(x, attr.name = attr.name))
    if (!is.null(x@meta$costs)) {
      if (!any(attr.name %in% colnames(x@meta$costs))) {
        stop("attr.name is not documented in x@meta$costs.")
      }
      nodeCosts <- as.character(nodeAttr)
      rules     <- x@meta$costs
      
      known.values <- as.character(rules[, attr.name])
      unmapped     <- unique(nodeCosts[!nodeCosts %in% known.values])
      if (length(unmapped) > 0) {
        stop(sprintf(
          "The following node attribute values have no cost rule defined: %s. Add them to x@meta$costs before calling setCosts().",
          paste(unmapped, collapse = ", ")
        ))
      }
      
      for (i in seq_len(nrow(rules))) {
        nodeCosts[nodeCosts == rules[i, attr.name]] <- rules[i, ncol(rules)]
      }
      
      nodeCosts <- as.numeric(nodeCosts)
    } else {
      stop("x@meta does not contain a 'costs' component.")
    }
  } else { # cost directly provided
    if (!is.numeric(node.values)) stop("Provided 'node.values' not numeric.")
    node.values <- rep(node.values, length = length(getNodes(x))) # recycling node costs
    nodeCosts <- node.values
  }

  ## find costs of edges as a function of terminating vertices
  EL <- getGraph(x)@edgeL

  ## method == mean ##
  if (method == "mean") {
    for (i in seq_along(EL)) {
      EL[[i]]$weights <- (nodeCosts[i] + nodeCosts[EL[[i]]$edges]) / 2
    }
  }

  ## method == product ##
  if (method == "product") {
    for (i in seq_along(EL)) {
      EL[[i]]$weights <- nodeCosts[i] * nodeCosts[EL[[i]]$edges]
    }
  }

  ## method == function ##
  if (method == "function") {
    for (i in seq_along(EL)) {
      EL[[i]]$weights <- FUN(nodeCosts[i], nodeCosts[EL[[i]]$edges], ...)
    }
  }

  ## return result
  newGraph <- new("graphNEL", nodes = getNodes(x), edgeL = EL)
  res <- x
  res@graph <- newGraph

  return(res)
} # end setCosts
