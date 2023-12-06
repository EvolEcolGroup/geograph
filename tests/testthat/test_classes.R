
test_that("Empty constructors work", {
  x <- new("gGraph")
  expect_true(inherits(x,"gGraph"))
  y <- new("gData")
  expect_true(inherits(y,"gData"))
})

test_that("Contructors fails with invalid coordinates",{
  NACoords <- data.frame(long = c(-24, NA), lat =  c(31,55))
  #Create gGraph with NA's
  expect_error(new("gGraph", coords = NACoords),
               "Argument coords includes NAs")

})

test_that("Contructors fails with invalid matrix dimensions",{
  extra_coords <- data.frame(long = c(-24, 37), lat =  c(31,55), x = c(31,31))
  #Create gGraph with three columns
  expect_error(new("gGraph", coords = extra_coords),
               "Argument coords must include")

})

test_that("Contructors fails with invalid non-numeric matrix",{
  non_num_coords <- data.frame(long = c("lon1", 37), lat =  c(31,55))
  #Create gGraph with non numeric elements
  expect_error(new("gGraph", coords = non_num_coords),
               "Argument coords has to be numeric")
  
})

test_that("Constructor checks column headings" , {
  column_heading <- data.frame(lat = c(-24, 37), lon =  c(31,55)) 
  #Create Ggraph with lat/lon headings
  correct_heading <- new("gGraph", coords = column_heading)
  column_heading <- data.frame(lon =  c(31,55), lat = c(-24, 37))
  #Create Ggraph with lon/lat headings
  swapped_heading <- new("gGraph", coords = column_heading)
  expect_identical(correct_heading, swapped_heading,
                  "Argument column names are not recognised")
})
  
