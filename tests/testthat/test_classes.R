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
