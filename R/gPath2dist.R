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
#' ## select a few populations from the HGDP dataset
#' hgdp.sub <- hgdp[getData(hgdp)$Population %in%
#'   c("French", "Balochi", "BantuKenya", "Papuan", "Pima")]
#' hgdp.path <- dijkstraBetween(hgdp.sub) # compute shortest path
#' gPath2dist(hgdp.path) # extract pairwise distances
#' @family dijkstra_methods
# Old text which does not make much sense?!?
# Note
# that if the \code{gPath} does not contain pairwise information, a warning
# will be issued, but the resulting output will likely be meaningless.\cr



#TODO think whether this should be a as.dist method for gPath

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
  x.names <- sub(":.*", "", names(x))
  i <- 1
  while (i < L && x.names[i] == x.names[i + 1]) {
    i <- i + 1
  }

  resSize <- i + 1

  ## check size consistency
  if (res.type == "dist" && L != (resSize * (resSize - 1L)) %/% 2L) {
    stop("Length of x does not match a number of pairwise comparisons; ",
          "cannot construct a 'dist' object. Use res.type = 'vector' instead.")
  }


  ## GET DISTANCES ##
  resDist <- sapply(x, function(e) sum(e$length_detail[[1]]))


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
} # end gPath2dist
