# built once for all tests to avoid repeating the slow extractFromLayer call
sf::sf_use_s2(FALSE)
world.countries <- rnaturalearth::ne_countries(
  scale       = "medium",
  returnclass = "sf"
)
test.graph <- extractFromLayer(
  rawgraph.10k,
  layer = world.countries,
  attr  = c("continent", "name")
)
test.graph <- setCosts(
  test.graph,
  node.values = rep(1, length(getNodes(test.graph)))
)

test_that("polygonBetween errors if g is not a gGraph or gData", {
  expect_error(
    polygonBetween(list(), layer = "name", from = "Spain", to = "Germany"),
    "Input g must be a gGraph or gData object"
  )
})

test_that("polygonBetween errors if 'from' polygon has no nodes", {
  expect_error(
    polygonBetween(test.graph, layer = "name",
                   from = "nonexistent", to = "Germany"),
    "One or both polygons contain no nodes"
  )
})

test_that("polygonBetween errors if 'to' polygon has no nodes", {
  expect_error(
    polygonBetween(test.graph, layer = "name",
                   from = "Spain", to = "nonexistent"),
    "One or both polygons contain no nodes"
  )
})

test_that("polygonBetween returns a gPath with outline = FALSE", {
  result <- polygonBetween(test.graph, layer = "name",
                           from = "Spain", to = "Germany",
                           outline = FALSE)
  expect_true(inherits(result, "gPath"))
})

test_that("polygonBetween returns a gPath with outline = FALSE", {
  result <- polygonBetween(test.graph, layer = "name",
                           from = "Spain", to = "Germany",
                           outline = TRUE)
  expect_true(inherits(result, "gPath") || is.na(result))
})


test_that("polygonBetween outline = TRUE returns fewer or equal paths than outline = FALSE", {
  result.all     <- polygonBetween(test.graph, layer = "name",
                                   from = "Spain", to = "Algeria",
                                   outline = FALSE)
  result.outline <- polygonBetween(test.graph, layer = "name",
                                   from = "Spain", to = "Algeria",
                                   outline = TRUE)
  dist.all <- gPath2dist(result.all, res = "vec")
  dist.outline <- gPath2dist(result.outline, res = "vec")
  if (!anyNA(result.outline)) {
    expect_lte(length(dist.outline), length(dist.all))
  }
})

test_that("polygonBetween distances are shorter for adjacent than distant countries", {
  # France and Spain share a border so should be closer than Spain and Poland
  result.adjacent <- polygonBetween(test.graph, layer = "name",
                                    from = "Spain", to = "France",
                                    outline = FALSE)
  result.distant  <- polygonBetween(test.graph, layer = "name",
                                    from = "Spain", to = "Poland",
                                    outline = FALSE)
  dist.adjacent <- gPath2dist(result.adjacent, res = "vec")
  dist.distant  <- gPath2dist(result.distant, res = "vec")
  
  expect_lt(min(dist.adjacent, na.rm = TRUE),
            min(dist.distant,  na.rm = TRUE))
})

