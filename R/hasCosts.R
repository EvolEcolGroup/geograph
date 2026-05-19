#' Check if a gGraph has costs
#'
#' This function tests whether a [`gGraph`] has heterogeneous 
#' costs associated to its edges.
#'
#' @details This low-level function is designed to be called  by other procedures of
#' [geoGraph]. However, it can sometimes be useful by itself. Note that a [`gGraph`]
#' with all costs set uniform will also result in FALSE.
#' Unlike other functions in [geoGraph], this function does not
#' test for the validity of the provided arguments (for speed purposes).
#'
#' @param x a valid [`gGraph`] object.
#' @return `TRUE` if the graph has heterogeneous edge costs, `FALSE` if costs
#'   are absent or uniform.
#' @keywords utilities methods
#' @family cost_functions
#' @examples
#'
#' hasCosts(rawgraph.10k)   # TRUE
#' hasCosts(worldgraph.10k) # FALSE as not all edges have costs
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
