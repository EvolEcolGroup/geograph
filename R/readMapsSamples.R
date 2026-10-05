#' Read the sampled demes of a MAPS run as a gData object
#'
#' The function [`readMapsSamples`] builds a [`gData`] object locating the
#' samples of a MAPS (Migration And Population-size Surfaces, Al-Asadi et al.
#' 2019) run on a [`gGraph`] created with [`readMapsGrid`]. There is one
#' location per sampled deme, placed on the node of that deme.
#'
#' @details
#' As MAPS runs, it assigns each haploid sample to a deme (grid cell) and writes
#' the assignments to `ipmap.txt` in the output directory (`mcmcpath`). In order
#' to read the samples, [`readMapsSamples`] needs the [`gGraph`] of the MAPS run
#' (created with [`readMapsGrid`]) and the path to the output directory.
#'
#' The `@data` slot of the result is a `data.frame` with one row per sampled
#' deme and the columns `deme` (MAPS deme index) and `n_haploids`.
#'
#' @param x a [`gGraph`] object created with [`readMapsGrid`] from the same
#'   MAPS run.
#' @param mcmcpath a character string giving the path to a MAPS output
#'   directory containing `ipmap.txt`.
#' @return A [`gData`] object with one location per sampled deme.
#' @references Al-Asadi H, Petkova D, Stephens M, Novembre J (2019). Estimating
#'   recent migration and population-size surfaces. *PLoS Genetics* 15(1):
#'   e1007908. \doi{10.1371/journal.pgen.1007908}
#' @seealso [`readMapsGrid`] to create the [`gGraph`]. [`readMapsRates`] to add
#'   the MAPS estimates to it.
#' @family maps
#' @export
#' @examples
#' mcmcpath <- system.file("extdata", "maps", package = "geoGraph")
#' mapsGraph <- readMapsGrid(mcmcpath)
#' mapsSamples <- readMapsSamples(mapsGraph, mcmcpath)
#' mapsSamples
#' getData(mapsSamples)
readMapsSamples <- function(x, mcmcpath) {
  ## check input
  if (!is(x, "gGraph")) {
    stop("`x` must be a gGraph object")
  }
  gGraph.name <- deparse(substitute(x))
  if (!exists(gGraph.name, envir = .GlobalEnv, inherits = FALSE)) {
    stop(
      "`x` must be a gGraph object stored in a variable in the global environment ",
      "(e.g. mapsGraph <- readMapsGrid(mcmcpath)), as gData objects refer to their gGraph by name"
    )
  }
  if (!is.character(mcmcpath) || length(mcmcpath) != 1 || is.na(mcmcpath)) {
    stop("`mcmcpath` must be a single character string")
  }
  if (!nzchar(mcmcpath)) {
    stop("`mcmcpath` is an empty string (system.file() returns \"\" when a file is not installed)")
  }
  if (!identical(getNodes(get(gGraph.name, envir = .GlobalEnv)), getNodes(x))) {
    stop("the object '", gGraph.name, "' in the global environment has different nodes than `x`")
  }
  f.ipmap <- file.path(mcmcpath, "ipmap.txt")
  if (!file.exists(f.ipmap)) {
    stop("`mcmcpath` does not contain ipmap.txt")
  }

  ## read sample assignments
  ipmap <- scan(f.ipmap, quiet = TRUE)
  n <- length(getNodes(x))
  if (length(ipmap) == 0) {
    stop("ipmap.txt is empty")
  }
  if (anyNA(ipmap) || any(ipmap != round(ipmap))) {
    stop("ipmap.txt must contain integer deme indices")
  }
  if (min(ipmap) < 1 || max(ipmap) > n) {
    stop("ipmap.txt refers to demes outside 1..", n, " -- is `x` from the same MAPS run?")
  }

  ## one row per sampled deme
  demes <- sort(unique(as.integer(ipmap)))
  node.id <- as.character(demes)
  dat <- data.frame(deme = demes, n_haploids = tabulate(ipmap, nbins = n)[demes])

  ## build the gData; nodes.id and gGraph.name are set after construction so
  ## that MAPS' assignment is kept (the constructor would otherwise recompute
  ## closest nodes)
  coords <- getCoords(x)[match(node.id, getNodes(x)), , drop = FALSE]
  res <- new("gData", coords = coords, data = dat)
  res@nodes.id <- node.id
  res@gGraph.name <- gGraph.name
  validObject(res)

  return(res)
} # end readMapsSamples
