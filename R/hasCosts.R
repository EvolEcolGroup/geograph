#' Check if a gGraph has costs
#'
#' This function tests whether a \linkS4class{gGraph} has costs associated
#' to its edges.
#' 
#' This low-level function is designed to be called  by other procedures of
#' [geoGraph]. However, it can sometimes be useful by itself. Note that
#' unlike other functions in \code{geoGraph}, this functions does not
#' test for the validity of the provided arguments (for speed purposes).
#' 
#' @param x a valid \linkS4class{gGraph}.
#' @return a logical value is returned.
#' @keywords utilities methods
#' @name auxiliary
#' @examples
#'
#' hasCosts(worldgraph.10k)
#'
#' @export

hasCosts <- function(x) {
  if (length(getGraph(x)@edgeData@data) == 0) {
    return(FALSE)
  }
  w <- getCosts(x, res.type = "vector")
  if (length(unique(w)) < 2) {
    return(FALSE)
  }
  return(TRUE)
}





