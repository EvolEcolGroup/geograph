test_that("Empty constructors work", {
  x <- new("gGraph")
  expect_true(inherits(x, "gGraph"))
  y <- new("gData")
  expect_true(inherits(y, "gData"))
})

test_that("Contructors fails with invalid coordinates", {
  na.coords <- data.frame(long = c(-24, NA), lat = c(31, 55))
  # Create gGraph with NA's
  expect_error(
    new("gGraph", coords = na.coords),
    "Argument coords includes NAs"
  )
})

test_that("Contructors fails with invalid matrix dimensions", {
  extra.coords <- data.frame(long = c(-24, 37), lat = c(31, 55), x = c(31, 31))
  # Create gGraph with three columns
  expect_error(
    new("gGraph", coords = extra.coords),
    "Argument coords must include"
  )
})

test_that("Contructors fails with invalid non-numeric matrix", {
  non.num.coords <- data.frame(long = c("lon1", 37), lat = c(31, 55))
  # Create gGraph with non numeric elements
  expect_error(
    new("gGraph", coords = non.num.coords),
    "Argument coords has to be numeric"
  )
})


test_that("Constructor accounts for different column names", {
  columnheading.names <-
    data.frame(longitude = c(31, 55), Latitude = c(-24, 37))
  columnheading.names <- new("gGraph", coords = columnheading.names)
  new.columnheading.names <-
    data.frame(lon = c(31, 55), lat = c(-24, 37))
  new.columnheading.names <-
    new("gGraph", coords = new.columnheading.names)
  expect_identical(
    columnheading.names,
    new.columnheading.names
  )
})

test_that("Constructor reverses coord column order", {
  column.heading <- data.frame(lat = c(-24, 37), lon = c(31, 55))
  # Create Ggraph with lat/lon headings
  correct.heading <- new("gGraph", coords = column.heading)
  column.heading <- data.frame(lon = c(31, 55), lat = c(-24, 37))
  # Create Ggraph with lon/lat headings
  swapped.heading <- new("gGraph", coords = column.heading)
  expect_identical(correct.heading, swapped.heading)
})

test_that("we give message when columns are not recognized", {
  column.heading <- data.frame(lon = c(31, 55), lat = c(-24, 37))
  # Create Ggraph with lat/lon headings
  correct.heading <- new("gGraph", coords = column.heading)
  column.heading <- data.frame(blah = c(31, 55), lat = c(-24, 37))
  # Create Ggraph with lon/lat headings
  expect_message(
    unrecognised.heading <- new("gGraph", coords = column.heading),
    "The coordinate column names are not part of the standardised list"
  )
  expect_identical(correct.heading, unrecognised.heading)
})

test_that("gData constructor keeps user-supplied nodes.id", {
  my.nodes <- getNodes(worldgraph.10k)[c(10, 20)]
  x <- new("gData",
    coords = cbind(c(10, 20), c(50, 40)),
    nodes.id = my.nodes, gGraph.name = "worldgraph.10k"
  )
  expect_identical(getNodes(x), my.nodes)
})

test_that("gData constructor warns (not errors) on unknown nodes.id", {
  expect_warning(
    x <- new("gData",
      coords = cbind(10, 50),
      nodes.id = "not_a_node", gGraph.name = "worldgraph.10k"
    ),
    "Some nodes were not found in the gGraph object worldgraph.10k"
  )
  expect_identical(getNodes(x), "not_a_node")
})

test_that("gData constructor errors when nodes.id does not match coords", {
  expect_error(
    new("gData", coords = cbind(c(10, 20), c(50, 40)), nodes.id = "1"),
    "one value per location"
  )
})

test_that("gData constructor names the missing gGraph in its warning", {
  expect_warning(
    new("gData", coords = cbind(10, 50), gGraph.name = "no_such_graph_xyz"),
    "The gGraph object no_such_graph_xyz is missing"
  )
})

test_that("gGraph constructor keeps the meta argument", {
  col.rules <- data.frame(habitat = c("land", "sea"), color = c("green", "blue"))
  x <- new("gGraph",
    coords = cbind(lon = c(0, 1), lat = c(0, 1)),
    nodes.attr = data.frame(habitat = c("land", "sea")),
    meta = list(colors = col.rules)
  )
  expect_identical(getColors(x, res.type = "rules"), col.rules)
  expect_error(
    new("gGraph", coords = cbind(lon = 0, lat = 0), meta = "not a list"),
    "meta must be a list"
  )
})

test_that("gGraph constructor accepts coordinates without column names", {
  expect_message(
    x <- new("gGraph", coords = matrix(c(31, 55, -24, 37), ncol = 2)),
    "use the order they were given in"
  )
  ref <- new("gGraph", coords = data.frame(lon = c(31, 55), lat = c(-24, 37)))
  expect_identical(x, ref)
})
