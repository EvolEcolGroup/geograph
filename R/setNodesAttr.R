#' Set node attributes in a gGraph object
#'
#' The function `setNodesAttr` adds or replaces a node attribute
#' in a [`gGraph`] object.
#'
#' @param x a valid [`gGraph`] object.
#' @param attr.name a character string giving the name of the attribute
#'   to set.
#' @param values a vector of values, one per node.
#' @param \dots additional arguments passed to other methods (currently unused).
#' @return A [`gGraph`] object with the new node attribute added or replaced.
#' @seealso [`getNodesAttr`] to retrieve node attributes, [`setCosts`] to
#'   set edge costs.
#' @examples
#' ### for gGraphs
#' node.attr <- getNodesAttr(rawgraph.40k, attr.name = "habitat")
#' neigh.list <- rawgraph.40k@graph@edgeL
#' 
#' # reclassify sea nodes as "coast" if they have any land neighbors
#' levels(node.attr$habitat) <- c(levels(node.attr$habitat), "coast")
#' for (i.node in seq_len(nrow(node.attr))) {
#'   if (node.attr$habitat[i.node] == "sea") {
#'     neighbour.names <- neigh.list[[i.node]]$edges
#'     neighbour.land.values <- node.attr[neighbour.names, "habitat"]
#'     if (any(neighbour.land.values %in% c("land"))) {
#'       node.attr$habitat[i.node] <- "coast"
#'     }
#'   }
#' }
#' 
#' # create coast graph
#' coastGraph <- setNodesAttr(rawgraph.40k, attr.name = "habitat", values = node.attr$habitat)
#' 
#' colors <- data.frame(
#'   habitat = c("sea", "land", "coast"),
#'   color = c("blue", "green", "lightblue")
#' )
#' 
#' coastGraph@meta$colors <- colors
#' 
#' plot(coastGraph, reset = TRUE)
#' 
#' @export
setGeneric("setNodesAttr", function(x, ...) {
  standardGeneric("setNodesAttr")
})


#' @describeIn setNodesAttr Method for gGraph objects
#' @export
setMethod("setNodesAttr", "gGraph", function(x, attr.name, values, ...) {
  if (!is.gGraph(x)) stop("x is not a valid gGraph object.")
  if (!is.character(attr.name) || length(attr.name) != 1) {
    stop("`attr.name` must be a single character string.")
  }
  if (length(values) != length(getNodes(x))) {
    stop(sprintf(
      "`values` has length %d but graph has %d nodes; lengths must match exactly.",
      length(values), length(getNodes(x))
    ))
  }
  
  x@nodes.attr[[attr.name]] <- values
  return(x)
})
