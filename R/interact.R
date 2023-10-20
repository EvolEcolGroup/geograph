#' @importFrom graphics  identify locator segments
NULL

#################
## geo.add.edges
#################


#' Add and remove edges from a gGraph object
#'
#' The functions \code{geo.add.edges} and \code{geo.remove.edges} allow one to
#' add or remove edges interactively with a \linkS4class{gGraph} object. When
#' adding edges, two approaches are possible:\cr - click vertices defining new
#' edges (mode="points")\cr - select an area in which all edges from a
#' reference graph are added (mode="area").\cr
#'
#'
#' @aliases geo.add.edges geo.remove.edges
#' @param x a valid \linkS4class{gGraph} object.
#' @param mode a character string indicating the mode for addition or removal
#' of edges. 'points': user is expected to click vertices to indicate edges.
#' 'area': user is expected to click two points defining a rectangular area
#' within which all edges are selected. 'all': all edges from the reference
#' graph are added to the current object.
#' @param refObj a valid \linkS4class{gGraph} object, used as a reference when
#' adding edges. When selecting an area inside which edges are added, all edges
#' existing in this area in \code{refObj} are added to \code{x}. Alternatively,
#' a character string can be provided, corresponding to one of the following
#' datasets: 'rawgraph.10k', rawgraph.40k'.
#' @return A \linkS4class{gGraph} object with newly added or removed edges.
#' @author Thibaut Jombart (\email{t.jombart@@imperial.ac.uk})
#' @seealso \code{\link{setEdges}} to non-interactively add or remove edges in
#' a \linkS4class{gGraph} object.
#' @keywords utilities
#' @examples
#' \dontrun{
#' plot(worldgraph.10k, reset = TRUE)
#'
#' ## zooming in
#' geo.zoomin(list(x = c(-6, 38), y = c(35, 73)))
#' title("Europe")
#'
#' ## remove edges
#' geo.remove.edges(worldgraph.10k) # points mode
#' geo.remove.edges(worldgraph.10k, mode = "area") # area mode
#'
#' ## add edges
#' geo.add.edges(worldgraph.10k) # points mode
#' geo.add.edges(worldgraph.10k, mode = "area") # area mode
#' }
#' @export
geo.add.edges <- function(x, mode = c("points", "area", "all"), refObj = "rawgraph.40k") {
  ## preliminary stuff
  if (!is.gGraph(x)) stop("x is not a valid gGraph object")
  mode <- match.arg(mode)
  ## temp <- isInArea(x) # not needed
  ## coords <- getCoords(x)[temp,]
  ## nodes <- getNodes(x)[temp]
  coords <- getCoords(x)
  nodes <- getNodes(x)
  lon <- coords[, 1]
  lat <- coords[, 2]
  # env <- get(".geoGraphEnv", envir=.GlobalEnv) # env is our target environnement

  ## handle refObj
  if (is.character(refObj) && refObj == "rawgraph.10k") {
    refObj <- rawgraph.10k
  } else if (is.character(refObj) && refObj == "rawgraph.40k") {
    refObj <- rawgraph.40k
  } else if (!is.gGraph(refObj)) {
    stop("refObj is not a valid gGraph object.")
  }

  ## handle plot param
  last.plot.param <- get("last.plot.param", envir = .geoGraphEnv)
  psize <- last.plot.param$psize
  pch <- last.plot.param$pch

  ## initialize toAdd
  toAdd <- list(from = NULL, to = NULL)

  ## "points" mode ##
  if (mode == "points") {
    spoint <- 1:2
    ## getting input from the user
    while (length(spoint) > 1) {
      spoint <- NULL
      spoint <- identify(lon, lat, plot = FALSE, n = 2)
      if (length(spoint) > 1) {
        segments(lon[spoint[1]], lat[spoint[1]], lon[spoint[2]], lat[spoint[2]], col = "green")
        points(lon[spoint[1]], lat[spoint[1]], cex = psize, col = "green", pch = pch)
        points(lon[spoint[2]], lat[spoint[2]], cex = psize, col = "green", pch = pch)

        toAdd$from <- c(toAdd$from, nodes[spoint[1]])
        toAdd$to <- c(toAdd$to, nodes[spoint[2]])
      }
    }
  } # end mode "points"

  ## "area" mode ##
  if (mode == "area") {
    selArea <- data.frame(x = 1:2, y = 1:2)

    ## getting input from the user
    while (nrow(selArea) > 1) {
      ##  selArea <- selArea[integer(0),]  not needed
      selArea <- data.frame(locator(2))

      if (nrow(selArea) > 1) {
        selNodes <- isInArea(refObj, reg = selArea, res.type = "integer") # indices of selected points
        selEdges <- getEdges(refObj, res.type = "matId", unique = TRUE) # edges, nodes=numerical indices
        temp <- (selEdges[, 1] %in% selNodes) & (selEdges[, 2] %in% selNodes)
        selEdges <- selEdges[temp, ] # edges of refobj wholly inside the selected area

        segments(lon[selEdges[, 1]], lat[selEdges[, 1]], lon[selEdges[, 2]], lat[selEdges[, 2]], col = "red")
        points(lon[selNodes], lat[selNodes], cex = psize, col = "green", pch = pch)

        toAdd$from <- c(toAdd$from, getNodes(refObj)[selEdges[, 1]])
        toAdd$to <- c(toAdd$to, getNodes(refObj)[selEdges[, 2]])
      }
    } # end while
  } # end mode "area"

  ## "all" mode ##
  if (mode == "all") {
    x@graph <- getGraph(refObj)
    return(x)
  }

  ## make sure added edges are unique
  toAdd <- as.matrix(as.data.frame(toAdd))
  toAdd <- t(apply(toAdd, 1, sort)) # sorting
  toAdd <- paste(toAdd[, 1], toAdd[, 2], sep = "-") # making strings
  toAdd <- unique(toAdd) # keep unique strings
  toAdd <- strsplit(toAdd, "-")
  from <- sapply(toAdd, function(e) e[1])
  to <- sapply(toAdd, function(e) e[2])

  ## call to setEdges
  res <- setEdges(x = x, add = cbind(from, to))

  return(res)
} # end geo.add.edges






####################
## geo.remove.edges
####################
#' @export
geo.remove.edges <- function(x, mode = c("points", "area")) {
  ## preliminary stuff
  if (!is.gGraph(x)) stop("x is not a valid gGraph object")
  temp <- isInArea(x)
  # coords <- getCoords(x)[temp,] # not needed: can work with whole object
  coords <- getCoords(x)
  nodeNames <- getNodes(x)
  lon <- coords[, 1]
  lat <- coords[, 2]
  # env <- get(".geoGraphEnv", envir=.GlobalEnv) # env is our target environnement
  psize <- get("psize", envir = .geoGraphEnv)
  mode <- match.arg(mode)

  ## handle plot param
  last.plot.param <- get("last.plot.param", envir = .geoGraphEnv)
  psize <- last.plot.param$psize
  pch <- last.plot.param$pch

  ## initialize toRemove
  toRemove <- list(from = NULL, to = NULL)


  ## mode: points ##

  if (mode == "points") {
    spoint <- 1:2
    ## getting input from the user
    while (length(spoint) > 1) {
      spoint <- NULL
      spoint <- identify(lon, lat, plot = FALSE, n = 2)
      if (length(spoint) > 1) {
        segments(lon[spoint[1]], lat[spoint[1]], lon[spoint[2]], lat[spoint[2]], col = "red")
        points(lon[spoint[1]], lat[spoint[1]], cex = psize, col = "red", pch = pch)
        points(lon[spoint[2]], lat[spoint[2]], cex = psize, col = "red", pch = pch)

        toRemove$from <- c(toRemove$from, nodeNames[spoint[1]])
        toRemove$to <- c(toRemove$to, nodeNames[spoint[2]])
      }
    }
  } # end mode: points


  ## mode: area ##

  if (mode == "area") {
    selArea <- data.frame(x = 1:2, y = 1:2)

    ## getting input from the user
    while (nrow(selArea) > 1) {
      ##  selArea <- selArea[integer(0),]  not needed
      selArea <- data.frame(locator(2))

      if (nrow(selArea) > 1) {
        selIdx <- which(isInArea(x, reg = selArea)) # indices of selected points
        selEdges <- getEdges(x, res.type = "matId", unique = TRUE) # edges, nodes=numerical indices
        temp <- (selEdges[, 1] %in% selIdx) & (selEdges[, 2] %in% selIdx)
        selEdges <- selEdges[temp, ] # edges wholly inside the selected area

        segments(lon[selEdges[, 1]], lat[selEdges[, 1]], lon[selEdges[, 2]], lat[selEdges[, 2]], col = "red")
        points(lon[selIdx], lat[selIdx], cex = psize * 1.5, col = "red")

        toRemove$from <- c(toRemove$from, nodeNames[selEdges[, 1]])
        toRemove$to <- c(toRemove$to, nodeNames[selEdges[, 2]])
      }
    }
  } # end mode: area


  ## handle toRemove ##
  ## make sure removed edges are unique
  toRemove <- as.matrix(as.data.frame(toRemove))
  toRemove <- t(apply(toRemove, 1, sort)) # sorting
  toRemove <- paste(toRemove[, 1], toRemove[, 2], sep = "-") # making strings
  toRemove <- unique(toRemove) # keep unique strings
  toRemove <- strsplit(toRemove, "-")
  from <- sapply(toRemove, function(e) e[1])
  to <- sapply(toRemove, function(e) e[2])

  ## call to setEdges
  res <- setEdges(x = x, remove = cbind(from, to))

  return(res)
} # end geo.remove.edges






###################
## geo.change.attr
###################


#' Change values of a node attribute
#'
#' The functions \code{geo.change.attr} changes values of a given node
#' attribute for a set of selected nodes of a \linkS4class{gGraph} object.
#'
#' The argument \code{only.name} allows one to perform a more accurate
#' selection of nodes whose attribute is changed, by specifying values
#' (\code{only.value}) of an attribute (\code{only.name}) that can be selected.
#' For instance, one may want to define new attributes for nodes of
#' worldgraph.10k that are exclusively on land: this would be done by
#' specifying \code{only.name="habitat"} and \code{only.value="land"}.
#'
#' @param x a valid \linkS4class{gGraph} object.
#' @param mode a character string indicating whether selected nodes are clicked
#' one by one ('points') or by defining a rectangular area ('area').
#' @param attr.name the name of the node attribute to be modified.
#' @param attr.value the new value of attribute assigned to selected nodes.
#' @param only.name (optional) in area mode, the name of a node attribute to
#' add an extra selection criterion. See details.
#' @param only.value (optional) in area mode, and if \code{only.name} is
#' specified, the values of \code{only.name} that can be selected. See details.
#' @param newCol a character string giving the new color for the attribute
#' value.
#' @param restore.edges a logical indicating whether edges stemming from the
#' modified nodes should be re-added to the graph, using \code{refObj} as a
#' reference. This is useful when connectivity is to be redefined using
#' \code{\link{setCosts}} for nodes that were previously disconnected.
#' @param refObj a character string or a \linkS4class{gGraph} object, used as
#' reference when re-adding edges. If a character string is provided, it must
#' match one of the following dataset: 'rawgraph.10k', 'rawgraph.40k'.
#' @return A \linkS4class{gGraph} object with modified node attributes.
#' @author Thibaut Jombart (\email{t.jombart@@imperial.ac.uk})
#' @keywords utilities
#' @examples
#' \dontrun{
#' plot(worldgraph.10k, reset = TRUE)
#'
#' ## have to click here for an area
#' ## all nodes are modified in the area
#' x <- geo.change.attr(worldgraph.10k, mode = "area", attr.name = "habitat", attr.value = "fancy
#' habitat", newCol = "pink") # modify selected area
#'
#' plot(x, reset = TRUE) # modification in the whole selected area
#'
#' ## have to click here for an area
#' ## only nodes on land are modified
#' x <- geo.change.attr(x, mode = "area", attr.name = "habitat", attr.value = "fancy2
#' habitat", newCol = "purple", only.name = "habitat", only.value = "land")
#'
#' plot(x, reset = TRUE) # modification in the whole selected area
#' }
#'
#' @export
geo.change.attr <- function(x, mode = c("points", "area"), attr.name, attr.value,
                            only.name = NULL, only.value = NULL, newCol = "black",
                            restore.edges = FALSE, refObj = "rawgraph.40k") {
  ## preliminary stuff ##
  if (!is.gGraph(x)) stop("x is not a valid gGraph object")

  ## handle "only" ##
  if (!is.null(only.name)) {
    temp <- unlist(getNodesAttr(x, attr.name = only.name))
    temp <- as.character(unlist(temp))
    hasRightAttr <- which(temp == only.value)
    if (length(hasRightAttr) == 0) stop(paste("specified values of", only.name, "never found."))
  } else {
    hasRightAttr <- 1:nrow(getCoords(x))
  }

  ## handle refObj ##
  if (restore.edges) {
    if (is.character(refObj) && refObj == "rawgraph.10k") {
      refObj <- rawgraph.10k
    } else if (is.character(refObj) && refObj == "rawgraph.40k") {
      refObj <- rawgraph.40k
    } else if (!is.gGraph(refObj)) {
      stop("refObj is not a valid gGraph object.")
    }
  } # end handle refObj


  coords <- getCoords(x)
  lon <- coords[, 1]
  lat <- coords[, 2]
  # env <- get(".geoGraphEnv", envir=.GlobalEnv) # env is our target environnement
  mode <- match.arg(mode)
  if (!attr.name %in% colnames(x@nodes.attr)) stop("specified node attribute name not found")

  ## set replacement colors
  if ((!is.null(x@meta$colors)) && (attr.name %in% colnames(x@meta$colors))) {
    temp <- which(attr.value == x@meta$colors[, attr.name])[1]
    if (!is.na(temp)) { # attr.value is documented in @meta$colors
      newCol <- x@meta$colors[temp, 2]
    } else { # if attr.value is not documented, we document it in @meta$colors
      if (is.factor(x@meta$colors[, attr.name])) { # if attr is a factor
        x@meta$colors[, attr.name] <- as.character(x@meta$colors[, attr.name]) # convert as character
        x@meta$colors <- rbind.data.frame(x@meta$colors, c(attr.value, newCol))
        x@meta$colors[, attr.name] <- factor(x@meta$colors[, attr.name]) # restore factor type
      } else { # attr is not a factor
        x@meta$colors <- rbind.data.frame(x@meta$colors, c(attr.value, newCol))
      }
    }
  } # end setting replacement colors


  ## handle plot param
  last.plot.param <- get("last.plot.param", envir = .geoGraphEnv)
  psize <- last.plot.param$psize
  pch <- last.plot.param$pch

  ## initialize toChange
  toChange <- integer(0)


  ## mode: points ##

  if (mode == "points") {
    spoint <- 0
    ## getting input from the user
    while (length(spoint) > 0) {
      spoint <- NULL
      spoint <- identify(lon, lat, plot = FALSE, n = 1)
      if (length(spoint) > 0) {
        spoint <- spoint[spoint %in% hasRightAttr] # only nodes with a given attributes will be modified
        points(lon[spoint], lat[spoint], cex = psize, pch = pch, col = newCol)

        toChange <- c(toChange, spoint)
      }
    }
  } # end mode: points

  if (mode == "area") {
    selArea <- data.frame(x = 1:2, y = 1:2)

    ## getting input from the user
    while (nrow(selArea) > 1) {
      selArea <- selArea[integer(0), ]
      selArea <- data.frame(locator(2))

      if (nrow(selArea) > 1) {
        selIdx <- which(isInArea(x, reg = selArea)) # indices of selected points
        selIdx <- selIdx[selIdx %in% hasRightAttr] # only nodes with replaced attribute
        points(lon[selIdx], lat[selIdx], cex = psize, pch = pch, col = newCol)

        toChange <- c(toChange, selIdx)
      }
    }
  } # end mode: area


  ## make changes ##
  toChange <- unique(toChange) # unique id
  res <- x

  if (is.factor(res@nodes.attr[, attr.name])) { # special handling if attr is a factor
    temp <- as.character(res@nodes.attr[, attr.name])
    temp[toChange] <- attr.value
    res@nodes.attr[, attr.name] <- factor(temp)
  } else { # in other cases...
    res@nodes.attr[toChange, attr.name] <- attr.value
  }

  ## re-add some edges if restore.edges is TRUE ##
  if (restore.edges) {
    nodeLab <- getNodes(res)[toChange] # label of changed nodes
    temp <- adj(getGraph(refObj), nodeLab)
    toAdd1 <- rep(names(temp), sapply(temp, length))
    toAdd2 <- unlist(temp)
    toAdd <- list(toAdd1, toAdd2)
    res <- setEdges(res, add = toAdd)
  }
  ## need to save the call here ! ##

  return(res)
} # end geo.change.attr
