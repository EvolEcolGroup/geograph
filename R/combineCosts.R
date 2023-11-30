#' Combine the costs of two gGraph objects
#'
#' The function \code{combineCosts} combines the edge costs of two
#' \linkS4class{gGraph} objects. The first object is used as a temlate to generate
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
#' @param x1 The firt gGraph (which will be used as a template to build the combined gGraph)
#' @param x2 The second gGraph from which costs will be combined
#' @param method a character string indicating which method should be used to
#' combined edge cost from the two gGraph. Currently available options are 'sum',
#' 'prod' and 'function', where the combined costs are computed as the sum,
#' the product or a custom function (defined in \code{FUN}) of the costs of its nodes.
#' @param FUN a function used to compute the cost between two nodes (needed if \code{method="function"}).
#' @param \dots additional parameters to be passed to \code{FUN}.
#' @return A \linkS4class{gGraph} object with the newly defined costs, basedd on the combination of the
#' two gGraph objects, used as weightings of edges.
#' @export
#' @examples
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
###############
## combineCosts
###############
combineCosts <- function(x1, x2, method = c("sum", "product", "function"), FUN = NULL, ...) {
  ## some checks + argument handling
  if (!is.gGraph(x1)) stop("x1 is not a valid gGraph object")
  if (!is.gGraph(x2)) stop("x2 is not a valid gGraph object")
  if (!hasCosts(x1)) stop("x1 is does not have costs; use setCosts to set the costs first")
  if (!hasCosts(x2)) stop("x1 is does not have costs; use setCosts to set the costs first")
  method <- match.arg(method)

  ## get the edges and weights from teh two graphs
  myGraph1 <- getGraph(x1)
  edgeW1 <- edgeWeights(myGraph1)
  edgeL1 <- edgeL(myGraph1)

  myGraph2 <- getGraph(x2)
  edgeW2 <- edgeWeights(myGraph2)
  edgeL2 <- edgeL(myGraph2)

  # test that the two graphs have the same edges
  if (!all(unlist(edgeL1) == unlist(edgeL2))) {
    stop("the graphs differ in the edges they have")
  }

  newEdgeL <- list()
  for (i in 1:length(edgeL1)) {
    newEdgeL[[i]] <- list()
    newEdgeL[[i]]$edges <- edgeL1[[i]]$edges
    if (method == "sum") {
      newEdgeL[[i]]$weights <- edgeW1[[i]] + edgeW2[[i]]
    } else if (method == "product") {
      newEdgeL[[i]]$weights <- edgeW1[[i]] * edgeW2[[i]]
    } else if (method == "function") {
      newEdgeL[[i]]$weights <- FUN(edgeW1[[i]], edgeW2[[i]], ...)
    }
  }

  names(newEdgeL) <- nodes(myGraph1) # items of the list must be named

  newGraph <- new("graphNEL", nodes = nodes(myGraph1), edgeL = newEdgeL)
  res <- x1
  res@graph <- newGraph

  return(res)
} # end combineCosts
