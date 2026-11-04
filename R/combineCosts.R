#' Combine the costs of two gGraph objects
#'
#' The function \code{combineCosts} combines the edge costs of two
#' \linkS4class{gGraph} objects. The first object is used as a template to generate
#' the objects with the combined costs. Two two \linkS4class{gGraph} objects must
#' have the same edges.
#'
#' Note that costs are inversely proportional to connectivity between edges:
#' the larger the cost associated to an edge, the lower the connectivity
#' between the two concerned nodes.\cr
#'
#' Also note that 'costs' defined in \code{geoGraph} are equivalent to
#' 'weights' as defined in \code{graph} and \code{RBGL} packages.
#'
#' @param x1 The first gGraph (which will be used as a template to build the combined gGraph)
#' @param x2 The second gGraph from which costs will be combined
#' @param method a character string indicating which method should be used to
#' combined edge cost from the two gGraph. Currently available options are 'sum',
#' 'prod' and 'function', where the combined costs are computed as the sum,
#' the product or a custom function (defined in \code{FUN}) of the costs of its nodes.
#' @param FUN a function used to compute the cost between two nodes (needed if \code{method="function"}).
#' @param \dots additional parameters to be passed to \code{FUN}.
#' @return A \linkS4class{gGraph} object with the newly defined costs, based on the combination of the
#' two gGraph objects, used as weightings of edges.
#' @family cost_functions
#' @seealso [setCosts] to set costs for a single gGraph object
#' @examples
#'
#' data("worldgraph.40k")
#' # new graph with custom cost function
#' exp.cost <- function(x1, x2, cost.coeff) {
#'   exp(-abs(x1 - x2) * cost.coeff)
#' }
#' # create a set of node costs
#' worldgraph.40k@nodes.attr$meanProd <- runif(n = 40962)
#' new_costs_graph <-
#'   setCosts(
#'     worldgraph.40k,
#'     node.values = worldgraph.40k@nodes.attr$meanProd,
#'     method = "function",
#'     FUN = exp.cost,
#'     cost.coeff = 0.5
#'   )
#' # combine costs from the original graph with the new costs
#' combine_costs_graph <- combineCosts(worldgraph.40k, new_costs_graph, method = "sum")
#' @export
###############
## combineCosts
###############
combineCosts <- function(x1, x2, method = c("sum", "product", "function"), FUN = NULL, ...) {
  ## some checks + argument handling
  if (!is.gGraph(x1)) stop("x1 is not a valid gGraph object")
  if (!is.gGraph(x2)) stop("x2 is not a valid gGraph object")
  if (length(getGraph(x1)@edgeData@data) == 0) stop("x1 does not have costs; use setCosts to set the costs first")
  if (length(getGraph(x2)@edgeData@data) == 0) stop("x2 does not have costs; use setCosts to set the costs first")
  method <- match.arg(method)

  if (method == "function" && is.null(FUN)) {
    stop("if method = 'function', FUN needs to be defined.")
  }

  ## get the edges and weights from the two graphs
  myGraph1 <- getGraph(x1)
  myGraph2 <- getGraph(x2)
  et1 <- .edgeTable(myGraph1)
  et2 <- .edgeTable(myGraph2)

  # test that the two graphs have the same nodes and edges (in the same order,
  # so that their weights can be combined element-wise)
  if (!identical(nodes(myGraph1), nodes(myGraph2)) ||
        !identical(et1$from, et2$from) || !identical(et1$to, et2$to)) {
    stop("the graphs differ in the edges they have")
  }
  myNodes <- nodes(myGraph1)

  if (method == "sum") {
    w <- et1$w + et2$w
  } else if (method == "product") {
    w <- et1$w * et2$w
  } else if (method == "function") {
    ## FUN is called once per node, with the weights of its edges in each graph,
    ## named by neighbour as in edgeWeights()
    byNode <- function(w) {
      names(w) <- myNodes[et1$to]
      split(w, factor(et1$from, levels = seq_along(myNodes)))
    }
    edgeW1 <- byNode(et1$w)
    edgeW2 <- byNode(et2$w)
    w <- lapply(seq_along(edgeW1), function(i) FUN(edgeW1[[i]], edgeW2[[i]], ...))
    if (!identical(lengths(w), lengths(edgeW1, use.names = FALSE))) {
      stop("FUN must return one value for each edge of a node.")
    }
    w <- unlist(w, use.names = FALSE)
  }

  ## each edge stores the combined weight of its reverse, as when the graph was
  ## rebuilt from its edge list (the same value, unless weights are asymmetric)
  w <- .reverseWeights(et1$from, et1$to, w, length(myNodes))
  w <- .nestedWeights(w, et1$from, length(myNodes))
  newGraph <- .graphFromTable(myNodes, et1$from, et1$to, w)
  res <- x1
  res@graph <- newGraph

  return(res)
} # end combineCosts
