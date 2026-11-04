## Benchmarks for the performance work described in data-raw/speed_plan.md.
##
## Run from the package root, before and after each change:
##   Rscript data-raw/speed/benchmark.R            # quick set (about 2-3 minutes)
##   Rscript data-raw/speed/benchmark.R --slow     # also the multi-minute cases
##   Rscript data-raw/speed/benchmark.R --only=dijkstra,raster   # filter by group
##
## Each line prints the group, the label and the elapsed time. Copy the
## before/after numbers of the items you touched into the commit message.
## All timings in the plan were taken on a 4-core container, R 4.3.3,
## graph 1.80, RBGL 1.78, sf 1.0-15, terra 1.7-65, dggridR 3.1.1.

args <- commandArgs(trailingOnly = TRUE)
slow <- "--slow" %in% args
only <- sub("^--only=", "", grep("^--only=", args, value = TRUE))
only <- if (length(only)) strsplit(only, ",")[[1]] else NULL

suppressMessages(pkgload::load_all(".", export_all = TRUE, quiet = TRUE))
suppressMessages(sf::sf_use_s2(TRUE))

bench <- function(group, label, expr, is.slow = FALSE) {
  if (!is.null(only) && !group %in% only) return(invisible(NULL))
  if (is.slow && !slow) return(invisible(NULL))
  gc(FALSE)
  t <- system.time(res <- suppressMessages(force(expr)))[["elapsed"]]
  cat(sprintf("%-10s %-58s %8.3f s\n", group, label, t))
  invisible(res)
}

g40 <- worldgraph.40k # 40,962 nodes, 29,458 edges, habitat costs
r40 <- rawgraph.40k # 40,962 nodes, 122,880 edges
set.seed(1)

## ---- graph rebuilding / costs ---------------------------------------------
bench("costs", "setCosts(worldgraph.40k, mean)", setCosts(g40, attr.name = "habitat", method = "mean"))
bench("costs", "setCosts(rawgraph.40k, node.values)", setCosts(r40, node.values = runif(40962)))
bench("costs", "setDistCosts(rawgraph.40k)", setDistCosts(r40))
bench("costs", "dropDeadEdges(worldgraph.40k, 10)", dropDeadEdges(g40, thres = 10))
bench("costs", "combineCosts(worldgraph.40k, worldgraph.40k)", combineCosts(g40, g40))
bench("costs", "hasCosts(worldgraph.40k)", hasCosts(g40))
bench("costs", "getCosts(vector, unique)", getCosts(g40, res.type = "vector", unique = TRUE))
bench("costs", "getCosts(asIs, unique)", getCosts(g40, res.type = "asIs", unique = TRUE))
bench("costs", "getEdges(matNames, unique)", getEdges(g40, res.type = "matNames", unique = TRUE))

## ---- subsetting / connectivity ---------------------------------------------
eu <- isInArea(g40, reg = list(x = c(-10, 40), y = c(35, 70)), quiet = TRUE)
bench("subset", "worldgraph.40k[Europe]", g40[eu])
bench("subset", "worldgraph.40k[every other node]", g40[seq(1, 40962, by = 2)])
bench("subset", "dropDeadNodes(worldgraph.40k)", dropDeadNodes(g40))
bench("subset", "keepMaxConnectedSet(worldgraph.40k)", keepMaxConnectedSet(g40))
E0 <- getEdges(g40, res.type = "matNames")
nn <- E0[sample(nrow(E0), 5000), ]
bench("subset", "areNeighbours(5000 pairs)", areNeighbours(nn[, 1], nn[, 2], g40))
und <- E0[E0[, 1] < E0[, 2], ]
bench("subset", "setEdges(remove = 5000 edges)", setEdges(g40, remove = und[sample(nrow(und), 5000), ]))
add <- cbind(sample(getNodes(g40), 5000), sample(getNodes(g40), 5000))
add <- add[add[, 1] != add[, 2], ]
bench("subset", "setEdges(add = ~5000 edges)", setEdges(g40, add = add))

## ---- shortest paths ---------------------------------------------------------
myGraph <- g40
assign("myGraph", myGraph, envir = .GlobalEnv)
hg <- setGraph(hgdp, "myGraph")
assign("hg", hg, envir = .GlobalEnv)
ori <- closestNode(myGraph, cbind(38, 9))
bench("dijkstra", "dijkstraBetween(hgdp) [51 locations, 1275 paths]", dijkstraBetween(hg))
bench("dijkstra", "dijkstraFrom(hgdp, Addis Ababa)", dijkstraFrom(hg, ori))
cc <- RBGL::connectedComp(getGraph(g40))
big <- g40[cc[[which.max(lengths(cc))]]] # Afro-Eurasia, ~10.5k nodes, connected
bench("dijkstra", "dijkstraFrom(gGraph, ~10.5k connected nodes)", dijkstraFrom(big, getNodes(big)[1]), is.slow = TRUE)
bench("dijkstra", "dijkstraBuffer(worldgraph.40k, d = 5000)", dijkstraBuffer(g40, origin = ori, d = 5000))
gc40 <- assignByPolygon(g40, attr = "name")
bench("dijkstra", "polygonBetween(France, Spain) [209 paths]", polygonBetween(gc40, "name", "France", "Spain"))

## ---- buffers / nodes ----------------------------------------------------------
bench("buffer", "buffer(worldgraph.40k, 1 node, d = 5000 km)", buffer(g40, ori, d = 5000))
bench("buffer", "buffer(rawgraph.40k, 51 nodes, d = 300 km)", buffer(r40, getNodes(hgdp), d = 300))
bench("buffer", "closestNode(1e5 locations)", closestNode(g40, cbind(runif(1e5, -180, 180), runif(1e5, -60, 70))))

## ---- raster / polygons ---------------------------------------------------------
r.eu <- terra::rast(xmin = -10, xmax = 40, ymin = 35, ymax = 70, resolution = 0.1, crs = "EPSG:4326")
terra::values(r.eu) <- runif(terra::ncell(r.eu))
bench("raster", "assignByRaster(175k cells)", assignByRaster(g40, r.eu))
r.w <- terra::rast(nrows = 1080, ncols = 2160, crs = "EPSG:4326")
terra::values(r.w) <- runif(terra::ncell(r.w))
bench("raster", "assignByRaster(2.3M cells, world)", assignByRaster(g40, r.w), is.slow = TRUE)
bench("raster", "findLand(worldgraph.40k)", findLand(g40))
bench("raster", "assignByPolygon(worldgraph.40k)", assignByPolygon(g40, attr = "continent"))

## ---- grids ------------------------------------------------------------------------
box <- c(xmin = -10, xmax = 40, ymin = 35, ymax = 70)
bench("grid", "makeHexGrid(Europe, spacing = 30) [15k nodes]", makeHexGrid(box, spacing = 30))
bench("grid", "makeHexGrid(Europe, spacing = 15) [136k nodes]", makeHexGrid(box, spacing = 15), is.slow = TRUE)
bench("grid", "makeSquareGrid(size = 90000)", makeSquareGrid(size = 90000))

## ---- plotting -----------------------------------------------------------------------
grDevices::pdf(NULL)
bench("plot", "plot(worldgraph.40k, reset = TRUE)", plot(g40, reset = TRUE))
bench("plot", "plot(worldgraph.40k, edges = TRUE, reset = TRUE)", plot(g40, edges = TRUE, reset = TRUE))
bench("plot", "plotEdges(rawgraph.40k)", { plot(r40, reset = TRUE, shape = NULL); plotEdges(r40) })
p.hg <- dijkstraBetween(hg)
bench("plot", "plot(gPath with 1275 paths)", plot(p.hg))
bench("plot", "connectivityPlot(worldgraph.40k)", connectivityPlot(g40))
grDevices::dev.off()
bench("ggplot", "ggplot_build(autoplot(worldgraph.40k))", ggplot2::ggplot_build(autoplot(g40)))
bench("ggplot", "ggplot_build(geom_ggraph(rawgraph.40k, edges))", ggplot2::ggplot_build(ggplot2::ggplot() + geom_ggraph(data = r40, edges = TRUE)))
bench("ggplot", ".gpathDf + .dfToSfPaths(1275 paths)", .dfToSfPaths(.gpathDf(p.hg)))
