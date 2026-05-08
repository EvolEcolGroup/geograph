#' Extract pairwise distances from a gPath object
#'
#' This function extracts the pairwise distances from the \code{gPath}
#' returned by [dijkstraBetween()] and returns a [`dist`] object.
#'
#' @param m a \code{gPath} object obtained by [dijkstraBetween()].
#' @param upper unused parameter added for consistency with [as.dist()].
#' @param diag unused parameter added for consistency with [as.dist()].
#' @param res.type a character string indicating what type of result should be
#' returned: a \code{dist} object ('dist'), or a vector of distances
#' ('vector'). Note that 'dist' should only be required for pairwise data, as
#' output by dijkstraBetween (as opposed to dijkstraFrom).
#' @return [`dist`] object containing the pairwise distances between nodes as
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
  x.names <- sub(":.*", "", names(x))
  i <- 1
  while (x.names[i] == x.names[i + 1] && i < L) {
    i <- i + 1
  }

  resSize <- i + 1

  ## check size consistency
  if (L != (resSize * (resSize - 1) * 0.5)) {
    if (res.type == "dist") {
      warning("Length of x does not match a number of pairwise comparisons.")
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
} # end gPath2dist
