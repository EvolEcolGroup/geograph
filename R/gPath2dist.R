#' Extract distances from a gPath object
#'
#' This function extracts distances from a \code{gPath} object returned by
#' [dijkstraBetween()] or [dijkstraFrom()]. Depending on \code{res.type}, it
#' returns either a [`dist`] object or a numeric vector of distances.
#'
#' @param m a \code{gPath} object obtained by [dijkstraBetween()] or
#' [dijkstraFrom()].
#' @param upper unused parameter added for consistency with [as.dist()].
#' @param diag unused parameter added for consistency with [as.dist()].
#' @param res.type deprecated parameter that is now ignored; the function
#' automatically detects whether the input is from [dijkstraBetween()] or
#' [dijkstraFrom()] and returns the appropriate output type.
#' @return Either a [`dist`] object containing pairwise distances between
#' nodes the gPath object was constructed from dijkstraBetween(), 
#' or a numeric vector of distances if the gPath object was constructed from dijkstraFrom().
#' @examples
#' ## for pairwise distances between multiple a "dist" object is returned
#' # select a few populations from the HGDP dataset
#' hgdp.sub <- hgdp[getData(hgdp)$Population %in%
#'   c("Balochi", "BantuKenya", "Papuan", "Pima")]
#' hgdp.path <- dijkstraBetween(hgdp.sub) # compute shortest path
#' gPath2dist(hgdp.path) # extract as dist object
#' ## for distances from a single origin node to multiple the output is a vector of distances
#' #' # choose an origin node
#' start <- "24988"
#' hgdp.path <- dijkstraFrom(hgdp.sub, start) # compute shortest path from origin
#' gPath2dist(hgdp.path) # extract as vector of distances
#' @family dijkstra_methods

# TODO think whether this should be a as.dist method for gPath

#' @export
gPath2dist <- function(m, diag = FALSE, upper = FALSE,
                       res.type = NULL) {
  # check that m is a gPath object
  if (!inherits(m, "gPath")) {
    stop("m is not a gPath object.")
  }
  ## find the size of the dist object ##
  x <- m
  L <- length(x)
  if (L < 1L) {
    stop("m must contain at least one element.")
  }
  if (is.null(names(x))) {
    stop("m must have non-NULL names.")
  }
  
  ## deprecation: res.type is now ignored, type is auto-detected
  if (!is.null(res.type)) {
    message("res.type is deprecated; the function now returns a vector for ",
            "dijkstraFrom outputs and a 'dist' object for dijkstraBetween ",
            "outputs.")
  }
  
  ## check type
  # check that the origin nodes are not all the same > djikstraFrom output
  origins <- sub(":.*", "", names(x))
  if (length(unique(origins)) == 1) {
    type <- "from"
  } else {
    n <- (1 + sqrt(1 + 8 * L)) / 2
    if (abs(n - round(n)) > 1e-9) {
      stop("Length of m does not match a pairwise count and origins differ; ",
           "cannot interpret as dijkstraBetween or dijkstraFrom output.")
    }
    n    <- round(n)
    type <- "between"
  }
  
  
  ## GET DISTANCES ##
  resDist <- sapply(x, function(e) sum(e$length_detail[[1]], na.rm = TRUE))
  ## BUILD RESULT ##
  ## type == between
  if (type == "between") {
    res <- stats::dist(1:n)
    res[] <- resDist
  } else {
    ## type == between (no change)
    res <- resDist
  }
  return(res)
} # end gPath2dist # end gPath2dist
