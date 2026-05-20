#' @include classes.R
NULL


#################
## BASIC METHODS
#################

############
## [ gGraph
############
setMethod("[", "gGraph", function(x, i, j, ..., drop = TRUE) {
  if (missing(i)) {
    i <- TRUE
  }
  if (is.logical(i)) {
    i <- rep(i, length = nrow(getCoords(x)))
  }
  if (is.character(i)) {
    i <- match(i, getNodes(x))
    if (any(is.na(i))) stop("Some specified node labels were not found.")
  }
  if (missing(j)) {
    j <- TRUE
  }

  if (is.logical(i) && is.logical(j) && all(c(i, j))) {
    return(x)
  } # don't loose time for silly trials

  argList <- list(...)
  if (is.null(argList$useSubGraph)) {
    useSubGraph <- TRUE
  } else {
    useSubGraph <- argList$useSubGraph
  }

  ## do the subsetting ##
  res <- x
  res@coords <- res@coords[i, , drop = FALSE]
  if (nrow(res@nodes.attr) > 0) {
    res@nodes.attr <- res@nodes.attr[i, j, drop = FALSE]
  }

  myGraph <- subGraph(nodes(res@graph)[i], res@graph)

  res@graph <- myGraph
  
  return(res)
})


###########
## [ gData
###########
setMethod("[", "gData", function(x, i, j, ..., drop = FALSE) {
  if (missing(i)) {
    i <- TRUE
  }
  if (is.logical(i)) {
    i <- rep(i, length = nrow(getCoords(x)))
  }
  if (is.character(i)) {
    i <- match(i, getNodes(x))
    if (any(is.na(i))) stop("Some specified node labels were not found.")
  }
  if (missing(j)) {
    j <- TRUE
  }

  if (is.logical(i) && is.logical(j) && all(c(i, j))) {
    return(x)
  } # don't loose time for silly trials


  ## do the subsetting ##

  ## coords
  res <- x
  N <- nrow(res@coords)
  res@coords <- res@coords[i, , drop = FALSE]

  ## nodes id
  res@nodes.id <- res@nodes.id[i]

  ## data
  if (!is.null(getData(x))) {
    if (nrow(getData(x)) == N) {
      res@data <- res@data[i, j, drop = FALSE]
    } else if (length(getData) == N) {
      res@data <- res@data[i]
    } else if (existsMethod("[", class(res@data)[1])) {
      res@data <- res@data[i, j, ..., drop = drop]
    } else {
      warning("Don't know what to do with @data.")
    }
  }

  return(res)
})


################
## SHOW METHODS
################

###############
## show gGraph
###############
setMethod("show", "gGraph", function(object) {
  x <- object
  N <- nrow(x@coords)
  nDisp <- 3

  ## printing
  cat("\n=== gGraph object ===\n")
  cat("\n@coords: spatial coordinates of", nrow(x@coords), "nodes\n")
  print(utils::head(x@coords, nDisp))
  if (N > nDisp) cat("...\n")

  cat("\n@nodes.attr:", ncol(x@nodes.attr), "nodes attributes\n")
  print(utils::head(x@nodes.attr, nDisp))
  if (nrow(x@nodes.attr) > nDisp) cat("...\n")

  cat("\n@meta: list of meta information with", length(x@meta), "items\n")
  if (length(x@meta) > 0) print(paste("$", names(x@meta), sep = ""))

  cat("\n@graph:\n")
  print(x@graph)

}) # end show gGraph


###############
## show gData
###############
setMethod("show", "gData", function(object) {
  x <- object
  N <- nrow(x@coords)
  nDisp <- 3

  ## printing
  cat("\n=== gData object ===\n")
  cat("\n@coords: spatial coordinates of", nrow(x@coords), "nodes\n")
  print(utils::head(x@coords, nDisp))
  if (N > nDisp) cat("...\n")

  cat("\n@nodes.id:", nrow(x@nodes.id), "nodes identifiers\n")
  print(utils::head(x@nodes.id, nDisp))
  if (length(x@nodes.id) > nDisp) cat("...\n")

  cat("\n@data:", nrow(x@data), "data\n")
  print(utils::head(x@data, nDisp))
  if (N > nDisp) cat("...\n")

  cat("\nAssociated gGraph:", x@gGraph.name, "\n")
}) # end show gData
