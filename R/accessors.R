#' @include classes.R
NULL


##############
## getGraph
##############
#' Get the graph component of a gGraph or gData object
#' 
#' The function `getGraph` returns the [`graph::graphNEL`] object stored in a [`gGraph`] 
#' or [`gData`] object.
#' 
#' @param x a valid [`gGraph`] or [`gData`] object.
#' @param ... additional arguments passed to other methods (currently unused).
#' @return A [`graph::graphNEL`] object.
#' @seealso [`getNodes`], [`getEdges`], [`getCoords`]
#' @examples
#' getGraph(worldgraph.10k)
#' @family accessor_methods
#' @export
setGeneric("getGraph", function(x, ...) {
  standardGeneric("getGraph")
})

#' @describeIn getGraph Method for gGraph objects
setMethod("getGraph", "gGraph", function(x, ...) {
  res <- x@graph
  return(res)
})

#' @describeIn getGraph Method for gData objects
setMethod("getGraph", "gData", function(x, ...) {
  if (!exists(x@gGraph.name, envir = .GlobalEnv)) stop(paste("gGraph object", x@gGraph.name, "not found."))
  res <- getGraph(get(x@gGraph.name, envir = .GlobalEnv))
  return(res)
})


##############
## setGraph
##############
#' Set the linked gGraph for a gData object
#'
#' The function `setGraph` sets the name of the [`gGraph`] object linked to a
#' [`gData`] object. It validates that the named object exists in
#' the global environment and is a valid [`gGraph`].
#'
#' @param x a valid [`gData`] object.
#' @param graph either a character string giving the name of a [`gGraph`]
#'   object in the global environment, or a [`gGraph`] object itself.
#' @return A [`gData`] object with the updated linked [`gGraph`].
#' @seealso [`getGraph`] to retrieve the linked graph. [`gData-class`] for
#'   the class definition.
#' @family accessor_methods
#' @examples
#' myGraph <- dropCosts(rawgraph.40k)
#' hgdp2 <- setGraph(hgdp, "myGraph")
#' getGraph(hgdp2)
#' @export
setGeneric("setGraph", function(x, graph) {
  standardGeneric("setGraph")
})

#' @describeIn setGraph Method for gData objects
#' @export
setMethod("setGraph", "gData", function(x, graph) {
  if (is.character(graph)) {
    if (!exists(graph, envir = .GlobalEnv)) {
      stop(paste("gGraph object", graph, "not found in global environment."))
    }
    if (!is.gGraph(get(graph, envir = .GlobalEnv))) {
      stop(paste(graph, "is not a valid gGraph object."))
    }
    x@gGraph.name <- graph
  } else if (is(graph, "gGraph")) {
    # Check if graph was passed as a symbol or by value
    if (typeof(substitute(graph)) == "symbol") {
      # Symbol case: store the name after checking global environment
      graph.name <- deparse(substitute(graph))
      if (!exists(graph.name, envir = .GlobalEnv)) {
        stop("gGraph object must exist in the global environment.")
      }
      x@gGraph.name <- graph.name
    } else {
      # Passed by value: accept directly without requiring global name
      # Note: the graph object itself is not stored in gData slots;
      # this case allows temporary graphs but getGraph may fail later
      # if the object is not findable via gGraph.name
      warning("gGraph object passed by value; it should be assigned to .GlobalEnv for getGraph to work.")
      x@gGraph.name <- ""
    }
  } else {
    stop("graph must be either a character string or a gGraph object.")
  }
  return(x)
})

################
## getNodesAttr
################
#' Get nodes attributes from gGraph/gData object
#'
#' The function `getNodesAttr` returns the values of a set of variables
#' associated to the nodes (i.e. node attributes) of a [`gGraph`] or
#' \linkS4class{gData} object.
#'
#'
#' @param x a valid \linkS4class{gGraph} or \linkS4class{gData} object.
#' @param nodes an optional integer, logical, or character string indicating
#' the subset of nodes to be used. If NULL, all nodes are used.
#' @param attr.name an optional character string indicating which node
#' attributes should be returned. If provided, it must match at least one of
#' the columns of \code{x@nodes.attr}.
#' @param \dots other arguments passed to other methods (currently unused).
#' @return A data.frame with the requested nodes attributes. Nodes are
#' displayed in rows, variables in columns.

#' @seealso [`getNodes`], [`getEdges`], [`getCoords`]
#' @keywords utilities methods
#' @examples
#'
#' ## gGraph method
#' head(getNodesAttr(worldgraph.40k))
#'
#'
#' ## gData method
#' getNodesAttr(hgdp)
#'
#' @family accessor_methods
#' @export
setGeneric("getNodesAttr", function(x, ...) {
  standardGeneric("getNodesAttr")
})

#' @describeIn getNodesAttr Method for gGraph objects
#' @export
setMethod("getNodesAttr", "gGraph", function(x, nodes = NULL, attr.name = NULL, ...) {
  if (is.null(nodes)) { # no node specified -> all nodes kept
    nodes <- TRUE
  }
  if (is.null(attr.name)) { # no attr specified -> all attr kept
    attr.name <- TRUE
  }

  res <- x@nodes.attr[nodes, attr.name, drop = FALSE]

  return(res)
})


#' @describeIn getNodesAttr Method for gData objects
#' @export
setMethod("getNodesAttr", "gData", function(x, attr.name = NULL, ...) {
  if (is.null(attr.name)) { # no attr specified -> all attr kept
    attr.name <- TRUE
  }

  myNodes <- getNodes(x)
  if (!exists(x@gGraph.name, .GlobalEnv)) stop("gGraph object not found in global environment.")
  mygGraph <- get(x@gGraph.name, envir = .GlobalEnv)

  res <- getNodesAttr(mygGraph, nodes = myNodes, attr.name = attr.name)

  return(res)
})


#############
## getCoords
#############
#' Get coordinates of nodes in a gGraph or gData object
#' 
#' The function `getCoords` returns the coordinates (longitude and latitude) of 
#' nodes in a [`gGraph`] or [`gData`] object. 
#' 
#' @param x a valid [`gGraph`] or [`gData`] object.
#' @param ... additional arguments passed to other methods (currently unused).
#' @return A matrix with two columns, `lon` and `lat`, giving the longitude
#'   and latitude of each node or location.
#' @seealso [`getNodes`], [`getGraph`]
#' @examples
#' head(getCoords(worldgraph.10k))
#'
#' ## for a gData object
#' getCoords(hgdp)
#'
#' ## coordinates of matched grid nodes instead of original locations
#' getCoords(hgdp, original = FALSE)
#' 
#' @family accessor_methods
#' @export
setGeneric("getCoords", function(x, ...) {
  standardGeneric("getCoords")
})

#' @describeIn getCoords Method for gGraph objects
#' @export
setMethod("getCoords", "gGraph", function(x, ...) {
  res <- x@coords
  return(res)
})


#' @describeIn getCoords Method for gData objects
#' @param original logical. If `TRUE` (default), returns the original location
#'   coordinates; if `FALSE`, returns the coordinates of the matched nodes on
#'   the [`gGraph`] grid.
#' @export
setMethod("getCoords", "gData", function(x, original = TRUE, ...) {
  if (original) { # original coords
    res <- x@coords
  } else {
    res <- getCoords(get(x@gGraph.name, envir = .GlobalEnv))[getNodes(x), , drop = FALSE] #
  }
  rownames(res) <- x@nodes.id
  return(res)
})


#############
## getNodes
#############
#' Get nodes of a gGraph or gData object
#' 
#' The function `getNodes` returns the names of nodes in a [`gGraph`] or [`gData`] object.
#' 
#' @param x a valid [`gGraph`] or [`gData`] object.
#' @param ... additional arguments passed to other methods (currently unused).
#' @return A character vector of node names.
#' @seealso [`getGraph`], [`getCoords`], [`getNodesAttr`]
#' @examples
#' head(getNodes(worldgraph.10k))
#' getNodes(hgdp)
#' 
#' @family accessor_methods
#' @export
setGeneric("getNodes", function(x, ...) {
  standardGeneric("getNodes")
})

#' @describeIn getNodes Method for gGraph objects
#' @export
setMethod("getNodes", "gGraph", function(x, ...) {
  res <- rownames(x@coords)
  return(res)
})

#' @describeIn getNodes Method for gData objects
#' @export
setMethod("getNodes", "gData", function(x, ...) {
  res <- x@nodes.id
  return(res)
})


#############
## getEdges
#############
#' Get edges from a gGraph object
#'
#' The function \code{getEdges} returns the edges of a \linkS4class{gGraph}
#' object using different possible outputs.
#'
#' @param x a valid \linkS4class{gGraph}.
#' @param res.type a character string indicating which kind of output should be
#' used. See value.
#' @param unique a logical indicating whether all returned edges should be
#' unique (TRUE) or if duplicated edges should be allowed (TRUE, default).
#' @param \dots other arguments passed to other methods (currently unused).
#' @return The output depends on the value of the argument \code{res.type}:\cr
#' - \code{asIs}: output is a named list of nodes, each slot containing nodes
#' forming an edge with one given node. This format is that of the \code{edges}
#' accessor for [`graph::graphNEL`] objects.\cr
#'
#' - \code{matNames}: a matrix with two columns giving couples of node names
#' forming edges.\cr
#'
#' - \code{matId}: a matrix with two columns giving couples of node indices
#' forming edges.\cr
#' @seealso [`setEdges`] to add or remove edges.
#'   [`geo.add.edges`] and [`geo.remove.edges`] for interactive versions.
#' @keywords utilities methods
#' @examples
#' head(getEdges(worldgraph.10k, res.type = "matNames", unique = TRUE))
#' head(getEdges(worldgraph.10k, res.type = "matId",   unique = TRUE))
#' 
#' @family accessor_methods
#' @export
setGeneric("getEdges", function(x, ...) {
  standardGeneric("getEdges")
})


#' @export
#' @describeIn getEdges Method for gGraph objects
setMethod("getEdges", "gGraph", function(x, res.type = c("asIs", "matNames", "matId"), unique = FALSE, ...) {
  res.type <- match.arg(res.type)
  ##    if(res.type=="asIs") return(x@graph@edgeL)
  if (res.type == "asIs") {
    return(edges(x@graph))
  }

  if (res.type == "matNames") { # return matrix of node names
    res <- edges(x@graph)
    temp <- sapply(res, length)
    col1 <- rep(names(res), temp)
    ## col1 <- rep(1:length(res), temp)
    col2 <- unlist(res)
    res <- cbind(Vi = col1, Vj = col2)
  }

  if (res.type == "matId") { # return matrix of node numbers
    res <- edgeL(x@graph)
    temp <- sapply(res, function(e) length(e$edges))
    col1 <- rep(seq_along(res), temp)
    col2 <- unlist(res)
    res <- cbind(Vi = col1, Vj = col2)
  }

  if (unique) {
    toKeep <- res[, 1] < res[, 2]
    res <- res[toKeep, , drop = FALSE]
  }

  rownames(res) <- NULL
  return(res)
})


##############
## getCosts
##############
#' Get costs associated to edges of a gGraph object
#'
#' The function `getCosts` returns the costs associated to the edges of a
#' [`gGraph`] object in different formats. `getNodeCosts` returns the costs
#' associated to nodes based on a node attribute and the cost rules stored
#' in `@meta$costs`.
#'
#' In `geoGraph`, costs are equivalent to weights in the `graph` package:
#' the larger the cost of an edge, the lower the connectivity between its
#' two nodes.
#'
#' @param x a valid [`gGraph`] object.
#' @param res.type a character string indicating the output format:
#'   - `"asIs"`: a named list of weights for each node's edges (default).
#'   - `"vector"`: a named numeric vector of all edge weights.
#'   - `"rules"`: the cost rules `data.frame` stored in `x@meta$costs`,
#'     with one row per node attribute value and a column named `"cost"`.
#' @param unique logical. If `TRUE`, only unique edge weights are returned.
#'   Defaults to `FALSE`. Only used when `res.type` is `"asIs"` or
#'   `"vector"`.
#' @param attr.name character string. The name of the node attribute used to
#'   look up costs from `x@meta$costs` (for `getNodeCosts` only).
#' @param ... other arguments passed to other methods (currently unused).
#' @return For `getCosts`:
#'   - `"asIs"`: a named list of edge weights, one element per node.
#'   - `"vector"`: a named numeric vector of edge weights.
#'   - `"rules"`: a `data.frame` of cost rules from `x@meta$costs`.
#'
#'   For `getNodeCosts`: a numeric vector of costs, one per node.
#' @seealso [`setCosts`] to set edge costs. [`dropCosts`] to remove all
#'   costs. [`hasCosts`] to check if a graph has costs defined.
#' @examples
#' ## get edge costs as a vector
#' head(getCosts(worldgraph.10k, res.type = "vector", unique = TRUE))
#'
#' ## get cost rules
#' getCosts(worldgraph.10k, res.type = "rules")
#'
#' ## get node costs based on habitat attribute
#' head(getNodeCosts(worldgraph.10k, attr.name = "habitat"))
#' @family accessor_methods
#' @family cost_functions
#' @export

setGeneric("getCosts", function(x, ...) {
  standardGeneric("getCosts")
})

#' @describeIn getCosts Method for gGraph object
#' @export
setMethod("getCosts", "gGraph", function(x, res.type = c("asIs", "vector", "rules"), 
                                         unique = FALSE, ...) {
  res.type <- match.arg(res.type)
  
  ## return cost rules directly if requested
  if (res.type == "rules") {
    if (is.null(x@meta$costs)) {
      stop("No cost rules defined in x (x@meta$costs is NULL).")
    }
    return(x@meta$costs)
  }

  ## retrieve edge weights
  res <- edgeWeights(x@graph)

  ## convert to vector if requested
  if (res.type == "vector") {
    res <- unlist(res)
  }

  ## apply unique filtering for both asIs and vector
  if (unique) {
    if (res.type == "asIs") {
      # For asIs (list format), we need to filter each element
      res <- lapply(names(res), function(node) {
        edges <- res[[node]]
        if (length(edges) > 0) {
          # Keep only edges where node name < neighbor name
          keep <- node < names(edges)
          edges[keep]
        } else {
          edges
        }
      })
      names(res) <- names(edgeWeights(x@graph))
    } else {
      # For vector format
      nodeNames <- names(res)
      temp      <- strsplit(nodeNames, "[.]")
      toKeep    <- sapply(temp, function(v) v[1] < v[2])
      res       <- res[toKeep]
    }
  }

  return(res)
})

#################
## getNodeCosts
#################
#' @export
#' @describeIn getCosts Function to get the costs values for nodes
setGeneric("getNodeCosts", function(x, ...) {
  standardGeneric("getNodeCosts")
})


#' @describeIn getCosts Method to get node costs for gGraph object
#' @export
setMethod("getNodeCosts", "gGraph", function(x, attr.name, ...) {
  if (!is.gGraph(x)) stop("x is not a valid gGraph object")
  
  ## assign costs to vertices
  nodeAttr <- unlist(getNodesAttr(x, attr.name = attr.name))
  if (!is.null(x@meta$costs)) {
    if (!any(attr.name %in% colnames(x@meta$costs))) {
      stop("attr.name is not documented in x@meta$costs.")
    }
    nodeCosts <- as.character(nodeAttr)
    rules <- x@meta$costs
    for (i in seq_len(nrow(x@meta$costs))) {
      nodeCosts[nodeCosts == rules[i, attr.name]] <- rules[i, ncol(rules)]
    }
    nodeCosts <- as.numeric(nodeCosts)
  } else {
    stop("x@meta does not contain a 'costs' component.")
  }
  
  
  return(nodeCosts)
}) # end getNodeCosts


###############
## dropCosts
###############
#' Remove all costs from a gGraph object
#' 
#' The function `dropCosts` removes all edge weights (costs) from a [`gGraph`]
#' object, returning an unweighted graph.
#' 
#' @param x a valid [`gGraph`] object.
#' @param ... additional arguments passed to other methods (currently unused).
#' @return A [`gGraph`] object with all edge costs removed.
#' @seealso [`getCosts`] to retrieve edge costs, [`setCosts`] to set edge costs.
#' [`hasCosts`] to check if a graph has costs defined.
#' @family cost_functions
#' @examples
#' hasCosts(rawgraph.10k)  
#' x <- dropCosts(worldgraph.10k)
#' hasCosts(x)               
#' @export
setGeneric("dropCosts", function(x, ...) {
  standardGeneric("dropCosts")
})

#' @describeIn dropCosts Method for gGraph objects
#' @export
setMethod("dropCosts", "gGraph", function(x) {
  myGraph <- getGraph(x)
  myGraph@edgeData@data <- list()
  x@graph <- myGraph

  return(x)
})


#############
## getData
#############
#' Get the data component of a gData object
#' 
#' The function `getData` returns the data stored in the `@data` slot of a
#' [`gData`] object.
#' 
#' @param x a valid [`gData`] object.
#' @param ... additional arguments passed to other methods (currently unused).
#' @return The data stored in the `@data` slot of the input object, typically a
#' data.frame where each row corresponds to a sampled location.
#' @seealso [`getCoords`] to retrieve coordinates. [`getNodes`] to retrieve
#'   matched node identifiers. [`getNodesAttr`] to retrieve node attributes
#'   from the underlying [`gGraph`].
#' @examples
#' ## get the data stored in the hgdp dataset
#' head(getData(hgdp))
#' 
#' @family accessor_methods
#' @export
setGeneric("getData", function(x, ...) {
  standardGeneric("getData")
})

#' @describeIn getData Method for gData objects
#' @export
setMethod("getData", "gData", function(x, ...) {
  res <- x@data
  return(res)
})


#############
## getColors
#############
#' Get colors associated to nodes of a gGraph object
#'
#' The function `getColors` returns either the color rules stored in a
#' [`gGraph`] object (`res.type = "rules"`) or a vector of colors for each
#' node based on a specified node attribute (`res.type = "colors"`).
#'
#' Color rules are stored as a two-column `data.frame` in `x@meta$colors`.
#' The first column is named after the node attribute and contains its possible
#' values; the second column is named `"color"` and contains valid R color
#' strings.
#'
#' @param x a valid [`gGraph`] object.
#' @param nodes a vector of node names or indices, or `"all"` for all nodes
#'   (default). Only used when `res.type = "colors"`.
#' @param attr.name a character string giving the name of the node attribute
#'   to use for color assignment. Required when `res.type = "colors"`.
#' @param col.rules a two-column `data.frame` mapping attribute values to
#'   colors. If `NULL`, uses `x@meta$colors`. Only used when
#'   `res.type = "colors"`.
#' @param res.type a character string indicating the output type:
#'   - `"colors"`: a named character vector of colors, one per node.
#'   - `"rules"`: the color rules `data.frame` stored in `x@meta$colors`.
#' @param ... other arguments passed to other methods (currently unused).
#' @return A named character vector of colors when `res.type = "colors"`, or
#'   a `data.frame` of color rules when `res.type = "rules"`.
#' @seealso [`setColors`] to set color rules. [`getNodesAttr`] to retrieve
#'   node attributes.
#' @family accessor_methods
#' @examples
#' ## get color rules
#' getColors(worldgraph.10k, res.type = "rules")
#'
#' ## get node colors based on habitat attribute
#' head(getColors(worldgraph.10k, attr.name = "habitat"))
#' @export
setGeneric("getColors", function(x, ...) {
  standardGeneric("getColors")
})

#' @describeIn getColors Method for gGraph objects
#' @export
setMethod("getColors", "gGraph", function(x, nodes = "all", attr.name = NULL,
                                          col.rules = NULL,
                                          res.type = c("colors", "rules"), ...) {
  res.type <- match.arg(res.type)
  
  ## return rules directly if requested
  if (res.type == "rules") {
    if (is.null(x@meta$colors)) {
      stop("No color rules defined in x (x@meta$colors is NULL).")
    }
    return(x@meta$colors)
  }
  
  ## res.type == "colors" from here
  if (is.null(attr.name)) {
    stop("attr.name must be provided when res.type = 'colors'.")
  }
  
  if (!attr.name %in% colnames(getNodesAttr(x))) {
    stop("Requested attribute not found in x@nodes.attr.")
  }
  
  if (is.null(col.rules)) {
    if (is.null(x@meta$colors)) {
      stop("No color rules provided and none defined in x (x@meta$colors is NULL).")
    }
    col.rules <- x@meta$colors
  }
  
  if (is.null(ncol(col.rules)) || ncol(col.rules) != 2) {
    stop("col.rules must have exactly two columns.")
  }
  
  if (!attr.name %in% colnames(col.rules)) {
    stop(paste("Nothing known about", attr.name, "in color rules."))
  }
  
  ## handle nodes
  if (length(nodes) == 1 && nodes == "all") {
    toKeep <- TRUE
  } else if (is.numeric(nodes)) {
    toKeep <- nodes
  } else if (is.character(nodes)) {
    toKeep <- match(nodes, getNodes(x))
  } else {
    stop("Don't know what to do with 'nodes': wrong specification.")
  }
  
  ## define colors
  criterion <- getNodesAttr(x, nodes = toKeep, attr.name = attr.name)
  col       <- as.character(unlist(criterion))
  
  for (i in seq_len(nrow(col.rules))) {
    col[col == col.rules[i, 1]] <- col.rules[i, 2]
  }
  
  names(col) <- getNodes(x)[toKeep]
  return(col)
})


#############
## setColors
#############
#' Set color rules for a gGraph object
#'
#' The function `setColors` sets the color rules stored in the `@meta$colors`
#' slot of a [`gGraph`] object. Color rules control how node attribute values
#' are mapped to colors when plotting.
#'
#' Color rules must be provided as a two-column `data.frame`. The first column
#' must be named after the node attribute and contain its possible values; the
#' second column must be named `"color"` and contain valid R color strings.
#'
#' @param x a valid [`gGraph`] object.
#' @param col.rules a two-column `data.frame` mapping attribute values to
#'   colors.
#' @return A [`gGraph`] object with the updated color rules.
#' @seealso [`getColors`] to retrieve colors or color rules.
#' @family accessor_methods
#' @examples
#' ## get current rules
#' col.rules <- getColors(worldgraph.10k, res.type = "rules")
#' col.rules
#'
#' ## modify a color
#' col.rules$color[col.rules$habitat == "sea"] <- "lightblue"
#'
#' ## set back
#' x <- setColors(worldgraph.10k, col.rules)
#' getColors(x, res.type = "rules")
#' @export
setColors <- function(x, col.rules) {
  if (!is.gGraph(x)) stop("x is not a valid gGraph object.")
  if (!is.data.frame(col.rules)) stop("col.rules must be a data.frame.")
  if (ncol(col.rules) != 2) stop("col.rules must have exactly two columns.")
  if (!("color" %in% colnames(col.rules))) stop("col.rules must have a column named 'color'.")
  
  ## get the attribute name from the first column
  attr.name <- colnames(col.rules)[1]
  
  ## check that the attribute exists in nodes.attr
  if (nrow(x@nodes.attr) > 0) {
    if (!attr.name %in% colnames(x@nodes.attr)) {
      stop(paste0("Column '", attr.name, "' not found in x@nodes.attr. ",
                  "The first column of col.rules must match a node attribute name."))
    }
    
    ## check that all node attribute values have a color rule defined
    node.values   <- unique(as.character(x@nodes.attr[, attr.name]))
    rule.values   <- as.character(col.rules[, attr.name])
    unmapped      <- node.values[!node.values %in% rule.values]
    if (length(unmapped) > 0) {
      stop(sprintf(
        "The following node attribute values have no color rule defined: %s.",
        paste(unmapped, collapse = ", ")
      ))
    }
    
    ## check that colors are valid R colors
    valid.colors <- tryCatch(
      { grDevices::col2rgb(col.rules$color); TRUE },
      error = function(e) FALSE
    )
    if (!valid.colors) {
      stop("col.rules contains invalid R color values.")
    }
  }
  
  x@meta$colors <- col.rules
  return(x)
}