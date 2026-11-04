## .shortestPaths() must return exactly what RBGL::sp.between() returns, one
## element per requested pair, in the requested order
spRef <- function(g, start, finish) {
  res <- suppressWarnings(RBGL::sp.between(g, start, finish))
  res[paste(start, finish, sep = ":")]
}

g10 <- getGraph(rawgraph.10k)
cc <- RBGL::connectedComp(g10)
big <- cc[[which.max(lengths(cc))]]

test_that(".shortestPaths matches sp.between from one start", {
  set.seed(2)
  st <- big[1]
  fin <- sample(setdiff(big, st), 200)
  expect_identical(.shortestPaths(g10, rep(st, 200), fin), spRef(g10, rep(st, 200), fin))
})

test_that(".shortestPaths matches sp.between pairwise and cross", {
  set.seed(3)
  s10 <- sample(big, 10)
  cmb <- utils::combn(10, 2)
  expect_identical(
    .shortestPaths(g10, s10[cmb[1, ]], s10[cmb[2, ]]),
    spRef(g10, s10[cmb[1, ]], s10[cmb[2, ]])
  )
  from <- s10[1:3]
  to <- s10[4:8]
  start <- rep(from, each = length(to))
  finish <- rep(to, times = length(from))
  expect_identical(.shortestPaths(g10, start, finish), spRef(g10, start, finish))
})

test_that(".shortestPaths handles start == finish and duplicated pairs", {
  st <- big[5]
  fin <- c(st, big[10], big[10], st)
  res <- .shortestPaths(g10, rep(st, 4), fin)
  expect_identical(res, spRef(g10, rep(st, 4), fin))
  expect_identical(names(res), paste(st, fin, sep = ":"))
  expect_identical(res[[1]]$path_detail, st)
  expect_true(is.na(res[[1]]$length))
  expect_identical(res[[2]], res[[3]])
})

test_that(".shortestPaths handles unreachable targets", {
  x <- worldgraph.10k[1:300]
  g <- getGraph(x)
  sets <- RBGL::connectedComp(g)
  sets <- sets[lengths(sets) > 1]
  st <- sets[[1]][1]
  fin <- c(sets[[2]][1], sets[[1]][2], sets[[2]][2])
  res <- .shortestPaths(g, rep(st, 3), fin)
  expect_identical(res, spRef(g, rep(st, 3), fin))
  expect_true(is.na(res[[1]]$path_detail))
})

test_that(".shortestPaths works on a directed graph", {
  et <- .edgeTable(g10)
  set.seed(4)
  keep <- et$from < et$to | runif(length(et$from)) < 0.7
  g <- .graphFromTable(graph::nodes(g10), et$from[keep], et$to[keep], et$w[keep] * runif(sum(keep)),
    edgemode = "directed"
  )
  st <- big[1]
  fin <- sample(setdiff(big, st), 100)
  res <- .shortestPaths(g, rep(st, 100), fin)
  expect_identical(res, spRef(g, rep(st, 100), fin))
  expect_true(any(grepl("->", names(unlist(lapply(res, `[[`, "length_detail"))))))
})

test_that("dijkstra functions return the same gPath as with sp.between", {
  hg <- hgdp[1:12]
  myGraph <- getGraph(worldgraph.40k)
  ref <- spRef(myGraph, rep("24988", 12), getNodes(hg))
  res <- dijkstraFrom(hg, "24988")
  expect_identical(unclass(res)[seq_along(res)], ref)

  nodes <- getNodes(hg)
  cmb <- utils::combn(length(nodes), 2)
  res <- dijkstraBetween(worldgraph.40k, nodes)
  ref <- spRef(myGraph, nodes[cmb[1, ]], nodes[cmb[2, ]])
  expect_identical(unclass(res)[seq_along(res)], ref)
})
