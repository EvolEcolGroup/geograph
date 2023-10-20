#' Set costs associated to edges based on geographic distances
#'
#' The function \code{setDistCosts} sets the costs of a \linkS4class{gGraph}
#' object using the geographic distance. The cost associated to an edge is
#' defined as the great circle distance between the two nodes of this edge.
#' \code{setDistCosts} actually relies on \code{\link[fields]{rdist.earth}} of
#' the \code{fields} package.
#'
#' The notion of 'costs' in the context of \linkS4class{gGraph} objects is
#' identical to the concept of 'weights' in \linkS4class{graph} (and thus
#' \linkS4class{graphNEL}) objects. The larger it is for an edge, the less
#' connectivity there is between the couple of concerned nodes.
#'
#' @aliases setDistCosts setDistCosts-methods setDistCosts,gGraph-method
#' @param x a valid \linkS4class{gGraph}.
#' @param \dots other arguments passed to other methods (currently unused).
#' @return For the \linkS4class{gGraph} method, a \linkS4class{gGraph} object
#' with appropriate weights. Note that former weights will be removed from the
#' object.
#' @author Thibaut Jombart (\email{t.jombart@@imperial.ac.uk})
#' @seealso The \code{\link{getCosts}} accessor, returning costs of the edges
#' of a \linkS4class{gGraph} object in different ways.\cr
#' @keywords utilities methods
#' @examples
#'
#' if (require(fields)) {
#'   ## load data
#'   plot(rawgraph.10k, reset = TRUE)
#'   geo.zoomin(list(x = c(110, 150), y = c(-10, -40)))
#'   plotEdges(rawgraph.10k)
#'
#'   ## compute costs
#'   x <- rawgraph.10k[isInArea(rawgraph.10k)]
#'   x <- setDistCosts(x)
#'
#'   ## replot edges
#'   plotEdges(x) # no big differences can be seen
#'   head(getCosts(x))
#' }
#'
############
## generic
############
#' @export
setGeneric("setDistCosts", function(x, ...) {
  standardGeneric("setDistCosts")
})





#################
## gGraph method
#################
#' @export
#' @describeIn setDistCosts Method for gGraph object
setMethod("setDistCosts", "gGraph", function(x, ...) {
  ## some checks ##
  if (!is.gGraph(x)) stop("x is not a valid gGraph object")


  ## get edges and coords ##
  E <- getEdges(x, res.type = "matNames")

  xy <- getCoords(x)
  xy1 <- xy[E[, 1], ]
  xy2 <- xy[E[, 2], ]


  ## get costs ##
  w <- sapply(1:nrow(E), function(i) fields::rdist.earth(xy1[i, , drop = FALSE], xy2[i, , drop = FALSE])) # list of costs

  ## assign costs to the graphNEL ##
  edgeData(x@graph, from = E[, 1], to = E[, 2], attr = "weight") <- w

  return(x)
}) # end setDistCosts gGraph
