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
#' @param FUN a function of two node costs, `FUN(cost_from, cost_to)`, giving
#'   the cost of moving from a node to its neighbor. It is called once per
#'   node, with the cost of the node and the costs of its neighbors, and must
#'   return one value per neighbor. If `FUN` is asymmetric, the two directions
#'   of an edge get different costs: `dijkstraFrom()` and
#'   `dijkstraBetween(from, to)` follow the direction of travel, while
#'   `gPath2dist()`, `getCosts(unique = TRUE)` and plots use one cost per edge.
#'   Required when `method = "function"`.
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
#' x <- setCosts(worldgraph.10k, attr.name = "habitat", cost.rules = cost.rules, method = "mean")
#' @export
setCosts <- function(x, attr.name = NULL, node.values = NULL, cost.rules = NULL,
                     method = c("mean", "product", "function"), FUN = NULL, ...) {
  ## some checks + argument handling
  if (!is.gGraph(x)) stop("x is not a valid gGraph object.")
  method <- match.arg(method)
  if ((method == "function") && (is.null(FUN))) {
    stop("if method = 'function', FUN needs to be defined.")
  }

  ## update cost rules if provided
  if (!is.null(cost.rules)) {
    if (!is.data.frame(cost.rules)) {
      stop("cost.rules must be a data.frame.")
    }
    if (ncol(cost.rules) != 2) {
      stop("cost.rules must have exactly two columns.")
    }
    if (!is.null(attr.name) && !(attr.name %in% colnames(cost.rules))) {
      stop("cost.rules must include the column named by attr.name.")
    }

    ## identify columns by name, not position
    if (!is.null(attr.name) && attr.name %in% colnames(cost.rules)) {
      attr.col <- attr.name
      other.col <- setdiff(colnames(cost.rules), attr.col)
      if (length(other.col) != 1) {
        stop("cost.rules must contain exactly one cost column distinct from attr.name.")
      }
      cost.col <- other.col
    } else {
      cost.col <- if ("cost" %in% colnames(cost.rules)) "cost" else colnames(cost.rules)[2]
      attr.col <- setdiff(colnames(cost.rules), cost.col)[1]
    }

    if (anyDuplicated(cost.rules[[attr.col]]) > 0) {
      stop("The attribute column of cost.rules must contain unique values.")
    }
    if (!is.numeric(cost.rules[[cost.col]])) {
      stop("cost.rules cost column must be numeric.")
    }
    if (anyNA(cost.rules[[cost.col]])) {
      stop("cost.rules cost column must not contain NA.")
    }

    ## normalize to canonical column order: attr first, cost second
    cost.rules <- cost.rules[, c(attr.col, cost.col), drop = FALSE]
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
      rules <- x@meta$costs
      cost.col <- colnames(rules)[2] # safe after normalization

      known.values <- as.character(rules[[attr.name]])
      unmapped <- unique(nodeCosts[!nodeCosts %in% known.values])
      if (length(unmapped) > 0) {
        stop(sprintf(
          "The following node attribute values have no cost rule defined: %s.
          Add them to x@meta$costs before calling setCosts().",
          paste(unmapped, collapse = ", ")
        ))
      }

      ## use named indexing — safe regardless of column order; look up all
      ## values at once, as replacing rule by rule would chain replacements
      ## whenever a cost equals another attribute value
      nodeCosts <- as.numeric(rules[[cost.col]][match(nodeCosts, known.values)])
    } else {
      stop("x@meta does not contain a 'costs' component.")
    }
  } else { # cost directly provided
    if (!is.numeric(node.values)) stop("Provided 'node.values' not numeric.")
    node.values <- rep(node.values, length = length(getNodes(x)))
    nodeCosts <- node.values
  }

  ## find costs of edges as a function of terminating vertices
  et <- .edgeTable(getGraph(x), weights = FALSE)

  if (method == "mean") {
    w <- (nodeCosts[et$from] + nodeCosts[et$to]) / 2
  }

  if (method == "product") {
    w <- nodeCosts[et$from] * nodeCosts[et$to]
  }

  if (method == "function") {
    ## FUN is called once per node, with the cost of the node and the costs of
    ## its neighbours, so that it does not need to be vectorised
    nbrs <- split(et$to, factor(et$from, levels = seq_along(getNodes(x))))
    w <- lapply(seq_along(nbrs), function(i) FUN(nodeCosts[i], nodeCosts[nbrs[[i]]], ...))
    if (!identical(lengths(w), lengths(nbrs, use.names = FALSE))) {
      stop("FUN must return one value for each neighbour of a node.")
    }
    w <- unlist(w, use.names = FALSE) # the edge i -> j gets FUN(cost_i, cost_j)
    if (!is.numeric(w)) stop("costs computed by FUN must be numeric.")
    n <- length(getNodes(x))
    reverse <- match((et$to - 1) * n + et$from, (et$from - 1) * n + et$to)
    if (!isTRUE(all.equal(w, w[reverse]))) {
      warning(
        "FUN is asymmetric, so the two directions of some edges have different costs. ",
        "dijkstraFrom() and dijkstraBetween(from, to) follow the direction of travel; ",
        "gPath2dist(), getCosts(unique = TRUE) and plots use one cost per edge."
      )
    }
  }
  
  w <- as.double(w)
  newGraph <- .graphFromTable(getNodes(x), et$from, et$to, w)
  res <- x
  res@graph <- newGraph

  return(res)
}
