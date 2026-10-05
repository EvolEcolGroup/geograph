#' Add MAPS migration and population-size estimates to a gGraph
#'
#' The function [`readMapsRates`] reads the migration and population-size
#' surfaces estimated by MAPS (Migration And Population-size Surfaces,
#' Al-Asadi et al. 2019) and adds posterior summaries as node attributes to a
#' [`gGraph`] created with [`readMapsGrid`]. Optionally, it also sets edge
#' costs from the migration rates and node colors showing where migration is
#' above or below average.
#'
#' @details
#' The function reads `mRates.txt` and `qRates.txt` and adds the following
#' node attributes (with `_<suffix>` appended to each name if `suffix` is
#' given):
#' * `m_mean`: posterior mean of the migration rate \eqn{m} (linear scale).
#' * `log10m_mean`, `log10m_sd`: posterior mean and standard deviation of
#'   \eqn{\log_{10} m}.
#' * `m_p_above`: proportion of draws in which the deme's \eqn{\log_{10} m} is
#'   above the average over all demes of that draw (as in the MAPS and EEMS
#'   "sign" plots).
#' * `m_sign`: `"above"` if `m_p_above` > 0.95, `"below"` if `m_p_above` <
#'   0.05, and `"uncertain"` otherwise.
#' * `log10N_mean`, `log10N_sd`, `N_p_above`: the same summaries for
#'   \eqn{\log_{10} N = -\log_{10}(2) - \log_{10} q}, so that larger values
#'   mean larger populations.
#' * `m_colour` (only if `colors = TRUE`): the color of each node, see below.
#'
#' Existing attributes with these names are replaced. Attributes describe the
#' posterior at each deme; values at demes far from any sample mostly reflect
#' the prior, which `m_sign` and `m_p_above` help to identify.
#'
#' If `costs = TRUE`, edge costs are set to the inverse of the MAPS edge
#' migration rate. MAPS sets the rate of the edge between demes \eqn{a} and
#' \eqn{b} to \eqn{(m_a + m_b)/2}; since this is linear in the node rates,
#' its posterior mean is the mean of the two `m_mean` values, and the cost is
#' \eqn{2/(m_a + m_b)} using those posterior means. High migration thus means
#' low cost.
#'
#' If `colors = TRUE`, nodes are colored on a continuous diverging scale by
#' `log10m_mean` relative to its average over all demes: red below, white at
#' and blue above the average, with limits symmetric around the average.
#' Demes where `m_sign` is `"uncertain"` are grey. The colors are stored in
#' the attribute `m_colour` and color rules are set on it. 
#' A [`gGraph`] holds one set of costs and one set of color rules,
#' so these replace any existing ones.
#'
#' @param x a [`gGraph`] object created with [`readMapsGrid`] from the same
#'   MAPS run.
#' @param mcmcpath a character string giving the path to a MAPS output
#'   directory containing `mRates.txt` and `qRates.txt`.
#' @param suffix an optional character string appended to the attribute
#'   names, e.g. to keep results for several segment length ranges on one
#'   [`gGraph`].
#' @param costs a logical; if `TRUE`, edge costs are set from the migration
#'   rates (see Details).
#' @param colors a logical; if `TRUE`, nodes are colored by their migration
#'   rate relative to the average (see Details).
#' @return A [`gGraph`] object with the new node attributes and, if requested,
#'   edge costs and color rules.
#' @references Al-Asadi H, Petkova D, Stephens M, Novembre J (2019). Estimating
#'   recent migration and population-size surfaces. *PLoS Genetics* 15(1):
#'   e1007908. \doi{10.1371/journal.pgen.1007908}
#' @seealso [`readMapsGrid`] to create the [`gGraph`]. [`readMapsSamples`] for
#'   the sampled demes. [`setCosts`] and [`setColors`], used to set costs and
#'   colors.
#' @family maps
#' @export
#' @examples
#' mcmcpath <- system.file("extdata", "maps", package = "geoGraph")
#' mapsGraph <- readMapsGrid(mcmcpath)
#' mapsGraph <- readMapsRates(mapsGraph, mcmcpath)
#' head(getNodesAttr(mapsGraph))
#'
#' # nodes colored by migration rate relative to the average
#' plot(mapsGraph, reset = TRUE)
#'
#' # edge costs are the inverse of the MAPS edge migration rates
#' head(getCosts(mapsGraph, res.type = "vector", unique = TRUE))
#'
#' # several length ranges on one graph: use suffixes
#' mapsGraph <- readMapsRates(mapsGraph, mcmcpath,
#'   suffix = "6_Inf",
#'   costs = FALSE, colors = FALSE
#' )
#' names(getNodesAttr(mapsGraph))
readMapsRates <- function(x, mcmcpath, suffix = NULL, costs = TRUE, colors = TRUE) {
  ## check input
  if (!is(x, "gGraph")) {
    stop("`x` must be a gGraph object")
  }
  if (!is.character(mcmcpath) || length(mcmcpath) != 1 || is.na(mcmcpath)) {
    stop("`mcmcpath` must be a single character string")
  }
  if (!nzchar(mcmcpath)) {
    stop("`mcmcpath` is an empty string (system.file() returns \"\" when a file is not installed)")
  }
  if (!is.null(suffix) && (!is.character(suffix) || length(suffix) != 1 ||
                           is.na(suffix) || !nzchar(suffix))) {
    stop("`suffix` must be NULL or a single non-empty character string")
  }
  for (arg in list(costs = costs, colors = colors)) {
    if (!is.logical(arg) || length(arg) != 1 || is.na(arg)) {
      stop("`costs` and `colors` must be TRUE or FALSE")
    }
  }
  files <- file.path(mcmcpath, c("mRates.txt", "qRates.txt"))
  missing.files <- !file.exists(files)
  if (any(missing.files)) {
    stop(
      "`mcmcpath` does not contain ",
      paste(basename(files[missing.files]), collapse = " and ")
    )
  }
  
  ## read the posterior draws (draws x demes)
  n <- length(getNodes(x))
  read_rates <- function(f) {
    r <- as.matrix(utils::read.table(f))
    if (!is.numeric(r) || anyNA(r)) {
      stop(basename(f), " must contain numeric values without missing values")
    }
    if (ncol(r) != n) {
      stop(
        basename(f), " has ", ncol(r), " columns but `x` has ", n,
        " nodes -- is `x` from the same MAPS run?"
      )
    }
    r
  }
  log10m <- read_rates(files[1])
  log10N <- -log10(2) - read_rates(files[2])
  
  ## posterior summaries per deme
  p_above <- function(r) colMeans((r - rowMeans(r)) > 0)
  col_sd <- function(r) apply(r, 2, stats::sd)
  m.p <- p_above(log10m)
  new.attr <- data.frame(
    m_mean = colMeans(10^log10m),
    log10m_mean = colMeans(log10m),
    log10m_sd = col_sd(log10m),
    m_p_above = m.p,
    m_sign = ifelse(m.p > 0.95, "above", ifelse(m.p < 0.05, "below", "uncertain")),
    log10N_mean = colMeans(log10N),
    log10N_sd = col_sd(log10N),
    N_p_above = p_above(log10N),
    row.names = getNodes(x)
  )
  
  ## colors: diverging scale around the average, grey where uncertain
  if (colors) {
    dev <- new.attr$log10m_mean - mean(new.attr$log10m_mean)
    lim <- max(abs(dev))
    ramp <- grDevices::colorRampPalette(c("#b2182b", "#f7f7f7", "#2166ac"))(101)
    idx <- if (lim > 0) round((dev + lim) / (2 * lim) * 100) + 1 else rep(51L, n)
    node.col <- ramp[idx]
    node.col[new.attr$m_sign == "uncertain"] <- "grey85"
    new.attr$m_colour <- node.col
  }
  
  if (!is.null(suffix)) {
    names(new.attr) <- paste0(names(new.attr), "_", suffix)
  }
  
  ## add to (or replace in) the node attributes
  attrs <- x@nodes.attr
  if (nrow(attrs) == 0) {
    attrs <- data.frame(row.names = getNodes(x))
  }
  attrs[names(new.attr)] <- new.attr
  x@nodes.attr <- attrs
  
  ## color rules: each stored color maps to itself
  if (colors) {
    col.name <- names(new.attr)[ncol(new.attr)]
    col.values <- unique(new.attr[[col.name]])
    col.rules <- data.frame(col.values, color = col.values)
    names(col.rules)[1] <- col.name
    x <- setColors(x, col.rules)
  }
  
  ## costs: inverse of the MAPS edge rate (m_a + m_b) / 2
  if (costs) {
    x <- setCosts(x,
                  node.values = new.attr[[1]], method = "function",
                  FUN = function(a, b) 2 / (a + b)
    )
  }
  
  validObject(x)
  return(x)
} # end readMapsRates