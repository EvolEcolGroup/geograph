#' Set node attributes in a gGraph object
#'
#' The function `setNodesAttr` adds or replaces a node attribute
#' in a [`gGraph`] object.
#'
#' @param x a valid [`gGraph`] or [`gData`] object.
#' @param attr.name a character string giving the name of the attribute
#'   to set.
#' @param values a vector of values, one per node.
#' @param \dots additional arguments passed to other methods (currently unused).
#' @return A [`gGraph`] or [`gData`] object with the new node attribute added or replaced.
#' @seealso [`getNodesAttr`] to retrieve node attributes, [`setCosts`] to
#'   set edge costs.
#' @examples
#' ### for gGraphs
#' node.attr <- getNodesAttr(rawgraph.40k, attr.name = "habitat")
#' neigh.list <- rawgraph.40k@graph@edgeL
#' 
#' # reclassify sea nodes as "coast" if they have any land neighbors
#' levels(node.attr$habitat) <- c(levels(node.attr$habitat), "coast")
#' for (i_node in seq_len(nrow(node.attr))) {
#'   if (node.attr$habitat[i_node] == "sea") {
#'     neighbour.names <- neigh.list[[i_node]]$edges
#'     neighbour.land.values <- node.attr[neighbour.names, "habitat"]
#'     if (any(neighbour.land.values %in% c("land"))) {
#'       node.attr$habitat[i_node] <- "coast"
#'     }
#'   }
#' }
#' 
#' new.attribute <- node.attr
#' 
#' # create coast graph
#' coastGraph <- setNodesAttr(rawgraph.40k, attr.name = "habitat", values = new.attribute$habitat)
#' 
#' colors <- data.frame(
#'   habitat = c("sea", "land", "coast"),
#'   cost = c("blue", "green", "lightblue")
#' )
#' 
#' coastGraph@meta$colors <- colors
#' 
#' plot(coastGraph, reset = TRUE)
#' 
#' #### and now for a gData
#' 
#' plot(hgdp)
#' hgdp_coast <- setNodesAttr(hgdp, attr.name = "habitat", values = new.attribute$habitat)
#' plot(hgdp_coast)
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

#' @describeIn setNodesAttr Method for gData objects
#' @export
setMethod("setNodesAttr", "gData", function(x, attr.name, values, ...) {
  if (!is.gData(x)) stop("x is not a valid gData object.")
  if (!is.character(attr.name) || length(attr.name) != 1) {
    stop("`attr.name` must be a single character string.")
  }
  
  ## get the underlying gGraph from the global environment
  if (!exists(x@gGraph.name, envir = .GlobalEnv)) {
    stop(paste("gGraph object", x@gGraph.name, "not found."))
  }
  my.graph <- get(x@gGraph.name, envir = .GlobalEnv)
  
  ## values must match the number of nodes in the gGraph (not gData)
  if (length(values) != length(getNodes(my.graph))) {
    stop(sprintf(
      "`values` has length %d but the underlying gGraph has %d nodes; lengths must match exactly.",
      length(values), length(getNodes(my.graph))
    ))
  }
  
  ## modify the gGraph and save back to global environment
  my.graph <- setNodesAttr(my.graph, attr.name = attr.name, values = values)
  assign(x@gGraph.name, my.graph, envir = .GlobalEnv)
  
  return(invisible(x))
})