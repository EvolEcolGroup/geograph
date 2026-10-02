#' Read a MAPS deme grid as a gGraph
#'
#' The function [`readMapsGrid`] builds a [`gGraph`] from the grid of demes
#' used by MAPS (Migration And Population-size Surfaces, Al-Asadi et al. 2019).
#' Each MAPS deme becomes a node and each MAPS edge becomes an edge of the
#' graph, so that MAPS estimates, which are reported per deme, can be analysed
#' on the same graph as other geoGraph data.
#'
#' @details
#' The gGraph is created by the following files in the output directory (`mcmcpath`): 
#' `demes.txt` with one row per deme, giving its longitude and latitude and
#' `edges.txt` with one row per edge, giving the two demes it connects.
#'   
#' MAPS reports longitudes from 0 to 360 when the habitat crosses the
#' antimeridian. As for any [`gGraph`], longitudes above 180 are converted to
#' the range -180 to 180.
#' 
#' All edges get a cost of 1; see [`setCosts`] to change this.
#'
#' @param mcmcpath a character string giving the path to a MAPS output
#'   directory containing `demes.txt` and `edges.txt`.
#' @return A \linkS4class{gGraph} object with one node per MAPS deme and one
#'   edge per MAPS edge.
#' @references Al-Asadi H, Petkova D, Stephens M, Novembre J (2019). Estimating
#'   recent migration and population-size surfaces. *PLoS Genetics* 15(1):
#'   e1007908. \doi{10.1371/journal.pgen.1007908}
#' @seealso [`makeHexGrid`] and [`makeSquareGrid`] to build other grids.
#'   [`setCosts`] to set edge costs.
#' @family maps
#' @export
#' @examples
#' mcmcpath <- system.file("extdata", "maps", package = "geoGraph")
#' mapsGraph <- readMapsGrid(mcmcpath)
#' mapsGraph
#'
#' # node names are the MAPS deme indices
#' head(getEdges(mapsGraph, res.type = "matNames", unique = TRUE))
readMapsGrid <- function(mcmcpath) {
  ## check input
  if (!is.character(mcmcpath) || length(mcmcpath) != 1 || is.na(mcmcpath)) {
    stop("`mcmcpath` must be a single character string")
  }
  if (!dir.exists(mcmcpath)) {
    stop("directory not found: ", mcmcpath)
  }
  files <- file.path(mcmcpath, c("demes.txt", "edges.txt"))
  missing.files <- !file.exists(files)
  if (any(missing.files)) {
    stop(
      "`mcmcpath` does not contain ",
      paste(basename(files[missing.files]), collapse = " and ")
    )
  }
  
  ## read demes
  demes <- as.matrix(utils::read.table(files[1]))
  if (ncol(demes) != 2 || !is.numeric(demes) || anyNA(demes)) {
    stop("demes.txt must have two numeric columns (longitude, latitude) without missing values")
  }
  colnames(demes) <- c("lon", "lat")
  n <- nrow(demes)
  
  ## read edges
  edges <- as.matrix(utils::read.table(files[2]))
  if (ncol(edges) != 2 || !is.numeric(edges) || anyNA(edges)) {
    stop("edges.txt must have two numeric columns without missing values")
  }
  if (any(edges != round(edges))) {
    stop("edges.txt contains non-integer deme indices")
  }
  if (min(edges) < 1 || max(edges) > n) {
    stop(
      "edges.txt refers to demes outside 1..", n,
      " (the indices must be 1-based row numbers of demes.txt)"
    )
  }
  if (any(edges[, 1] == edges[, 2])) {
    stop("edges.txt contains edges from a deme to itself")
  }
  
  ## keep each undirected edge once
  ft <- unique(cbind(pmin(edges[, 1], edges[, 2]), pmax(edges[, 1], edges[, 2])))
  if (nrow(ft) < nrow(edges)) {
    message(nrow(edges) - nrow(ft), " duplicated edge(s) in edges.txt ignored")
  }
  
  ## build the gGraph
  myGraph <- graph::ftM2graphNEL(ft, V = as.character(seq_len(n)), edgemode = "undirected")
  res <- new("gGraph", coords = demes, graph = myGraph)
  
  return(res)
} 

