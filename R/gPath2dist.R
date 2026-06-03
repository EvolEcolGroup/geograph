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
#' @param res.type a character string indicating what type of result should be
#' returned: a \code{dist} object ('dist'), or a vector of distances
#' ('vector'). Note that 'dist' should only be required for pairwise data, as
#' output by dijkstraBetween (as opposed to dijkstraFrom).
#' @return Either a [`dist`] object containing pairwise distances between
#' nodes when \code{res.type = "dist"}, or a numeric vector of distances when
#' \code{res.type = "vector"}.
#' @examples
#' ## for pairwise distances between multiple nodes you can use res.type = "dist"
#' # select a few populations from the HGDP dataset
#' hgdp.sub <- hgdp[getData(hgdp)$Population %in%
#'   c("Balochi", "BantuKenya", "Papuan", "Pima")]
#' hgdp.path <- dijkstraBetween(hgdp.sub) # compute shortest path
#' gPath2dist(hgdp.path, res = "dist") # extract as dist object
#' ## for distances from a single origin node to multiple we set res.type = "vector"
#' #' # choose an origin node
#' start <- "24988"
#' hgdp.path <- dijkstraFrom(hgdp.sub, start) # compute shortest path from origin
#' gPath2dist(hgdp.path, res = "vector") # extract as vector of distances
#' @family dijkstra_methods

# TODO think whether this should be a as.dist method for gPath

#' @export
gPath2dist <- function(m, diag = FALSE, upper = FALSE,
                       res.type = c("dist", "vector")) {
  # check that m is a gPath object
  if (!inherits(m, "gPath")) {
    stop("m is not a gPath object.")
  }
  ## find the size of the dist object ##
  x <- m
  res.type <- match.arg(res.type)
  L <- length(x)
  if (L < 1L) {
    stop("m must contain at least one element.")
  }
  if (is.null(names(x))) {
    stop("m must have non-NULL names.")
  }
  ## check size consistency
  if (res.type == "dist") {
    origins <- sub(":.*", "", names(x))
    
    n <- (1 + sqrt(1 + 8 * L)) / 2
    n <- round(n)
    if (n * (n - 1) / 2 != L) {
      stop(
        "Length of x does not match a number of pairwise comparisons; ",
        "cannot construct a 'dist' object. Use res.type = 'vector' instead."
      )
    }
    resSize <- n
    
    # check that the origin nodes are not all the same
    if (length(unique(origins)) == 1) {
      stop("All paths in m have the same origin node; cannot construct a 'dist'. ",
           "Use res.type = 'vector'.")
    }
  }
  ## GET DISTANCES ##
  resDist <- sapply(x, function(e) sum(e$length_detail[[1]], na.rm = TRUE))
  ## BUILD RESULT ##
  ## type == dist
  if (res.type == "dist") {
    res <- stats::dist(1:resSize)
    res[] <- resDist
  } else {
    ## type == vector (no change)
    res <- resDist
  }
  return(res)
} # end gPath2dist # end gPath2dist
