test_that("find land correctly",{
  # create a gGraph with one sea node and one land node
  myCoords <- data.frame(long = c(-24, 37), lat =  c(31,55))
  obj <- new("gGraph", coords = myCoords)
  obj <- findLand(obj) 
  # check that we classified the nodes correctly
  expect_true(all(obj@nodes.attr$habitat==c("sea","land")))
  
  # error if we pass an incorrect class
  expect_error(findLand("blah"),
               "unable to find an inherited method")
  
  #Create gGraph with NA's
  NACoords <- data.frame(long = c(-24, NA), lat =  c(31,55))
  NA_gGraph <- new("gGraph", coords = NACoords)
  
  #NA entries are recognised and produce error in plot
  expect_error(plot(NA_gGraph))
  
  #NA produces error in findLand
  expect_error(findLand(NA_gGraph))
  

})


#Test whether findLand generates an error with invalid matrix object
test_that("co-ordinate format",{
  #Create co-ordinates matrix
  obj <- matrix(c(-24, 31, 37, 55), nrow=2,ncol=2, byrow=TRUE)
  obj <- findLand(obj) 
  
  #check for factor output
  expect_is(obj,"factor")
  #check for correct output
  expect_equal(obj,factor(c('sea','land')))
  
  #Create co-ordinates matrix with NA
  NA_matrix <- matrix(c(-24, NA, 37, 55), nrow=2,ncol=2, byrow=TRUE)
  #NA entries are recognised and produce error in plot 
  expect_true(is.na(NA_matrix[1,2]))
  expect_error(plotEdges(NA_matrix))
  
  #NA produces error in findLand
  expect_error(findLand(NA_matrix))
  
  
})



