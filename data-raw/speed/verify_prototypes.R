## Equivalence checks of the prototypes in data-raw/speed/ against the current
## package code (see data-raw/speed_plan.md). Run from the package root:
##   Rscript data-raw/speed/verify_prototypes.R
## Every line must print TRUE. Takes about 1-2 minutes.

suppressMessages(pkgload::load_all(".", export_all = TRUE, quiet = TRUE))
source("data-raw/speed/proto_shortestPaths.R")
source("data-raw/speed/proto_edgeTable.R")

check <- function(label, ok) cat(sprintf("%-62s %s\n", label, isTRUE(ok)))
sameGraph <- function(a, b) {
  identical(graph::nodes(a), graph::nodes(b)) &&
    identical(graph::edges(a), graph::edges(b)) &&
    identical(graph::edgeWeights(a), graph::edgeWeights(b)) &&
    identical(graph::edgemode(a), graph::edgemode(b)) &&
    length(a@edgeData@data) == length(b@edgeData@data)
}
set.seed(1)
g40 <- worldgraph.40k

## ---- P1.1 .shortestPaths vs RBGL::sp.between ---------------------------------
g <- getGraph(g40)
cc <- RBGL::connectedComp(g)
big <- cc[[which.max(lengths(cc))]]
st <- big[1]
fin <- sample(setdiff(big, st), 300)
a <- RBGL::sp.between(g, st, fin)[paste(st, fin, sep = ":")]
check("P1.1 one start -> 300 targets", identical(a, .shortestPaths(g, rep(st, 300), fin)))
s20 <- sample(big, 20)
cmb <- utils::combn(20, 2)
a <- RBGL::sp.between(g, s20[cmb[1, ]], s20[cmb[2, ]])
a <- a[paste(s20[cmb[1, ]], s20[cmb[2, ]], sep = ":")]
check("P1.1 20 nodes pairwise", identical(a, .shortestPaths(g, s20[cmb[1, ]], s20[cmb[2, ]])))
## start == finish and duplicated pairs (unreachable targets also match, but
## sp.between needs ~15 s per unreachable target on this graph)
a <- RBGL::sp.between(g, c(st, st, st), c(st, fin[1], fin[1]))
b <- .shortestPaths(g, c(st, st, st), c(st, fin[1], fin[1]))
check("P1.1 start == finish, duplicated pair", identical(a[[1]], b[[1]]) && identical(a[[2]], b[[2]]) && identical(b[[2]], b[[3]]))

## ---- P0 / P1.2 / P1.3 / P1.6 edge-table helpers --------------------------------
check("P1.6 setCosts(mean) core", sameGraph(
  setCosts(g40, attr.name = "habitat", method = "mean")@graph,
  setCosts_core(g40@graph, getNodeCosts(g40, "habitat"), "mean")
))
check("P1.6 dropDeadEdges core", sameGraph(dropDeadEdges(g40, 10)@graph, dropDeadEdges_core(g40@graph, 10)))
w10 <- worldgraph.10k
idx <- seq(1, length(getNodes(w10)), by = 2)
check("P1.2 [ every other node, weighted", sameGraph(w10[idx]@graph, .subsetGraph(w10@graph, idx)))
u10 <- dropCosts(rawgraph.10k)
check("P1.2 [ every other node, no costs", sameGraph(u10[idx]@graph, .subsetGraph(u10@graph, idx)))
ri <- sample(length(getNodes(w10)), 500)
check("P1.2 [ re-ordered integer index", sameGraph(w10[ri]@graph, .subsetGraph(w10@graph, ri)))
a <- keepMaxConnectedSet(w10)
cc <- RBGL::connectedComp(w10@graph)
b <- .subsetGraph(w10@graph, sort(match(cc[[which.max(lengths(cc))]], graph::nodes(w10@graph))))
check("P1.3 keepMaxConnectedSet as a subset", sameGraph(a@graph, b))
E0 <- getEdges(w10, res.type = "matNames")
nn <- E0[sample(nrow(E0), 2000), ]
nn[1:50, 2] <- sample(getNodes(w10), 50)
nn[51, 1] <- "not_a_node"
check("P2.5 areNeighbours vectorised", identical(areNeighbours(nn[, 1], nn[, 2], w10), areNeighbours_core(nn[, 1], nn[, 2], w10@graph)))

## ---- P1.5 getCosts(asIs, unique) -----------------------------------------------
ew <- graph::edgeWeights(w10@graph)
b <- .mapply(function(node, e) if (length(e)) e[node < names(e)] else e, list(names(ew), ew), NULL)
names(b) <- names(ew)
check("P1.5 getCosts(asIs, unique) without name lookups", identical(getCosts(w10, res.type = "asIs", unique = TRUE), b))

## ---- P1.4 assignByRaster nearest node -------------------------------------------
r <- terra::rast(xmin = -10, xmax = 40, ymin = 35, ymax = 70, resolution = 0.25, crs = "EPSG:4326")
terra::values(r) <- runif(terra::ncell(r))
cur <- getNodesAttr(assignByRaster(g40, r))$raster_points
xy <- terra::xyFromCell(r, seq_len(terra::ncell(r)))
v <- terra::values(r, mat = FALSE)
nid <- FNN::get.knnx(unname(getCoords(g40)), xy, k = 1)$nn.index[, 1]
s <- split(v, factor(nid, levels = seq_len(nrow(getCoords(g40)))))
new <- vapply(s, function(x) if (length(x)) mean(x) else NA_real_, numeric(1), USE.NAMES = FALSE)
check("P1.4 assignByRaster via planar FNN (mean)", isTRUE(all.equal(cur, new)))

## ---- P3.3 the asymmetric setCosts(method = "function") quirk -------------------
FUN <- function(a, b) a + 2 * b
nv <- runif(length(getNodes(w10)))
x <- setCosts(w10, node.values = nv, method = "function", FUN = FUN)
et <- .edgeTable(x@graph)
check("P3.3 (info) edge i -> j currently stores FUN(c_j, c_i)", isTRUE(all.equal(et$w, FUN(nv[et$to], nv[et$from]))))
