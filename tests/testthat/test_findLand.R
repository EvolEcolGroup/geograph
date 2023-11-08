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

})


#Test whether findLand generates an error with invalid object
test_that("co-ordinate format",{
  #Create co-ordinates
  obj <- matrix(c(-24, 31, 37, 55), nrow=2,ncol=2, byrow=TRUE)
  obj <- findLand(obj) 
  
  #check for factor output
  expect_is(obj,"factor")
  #check for correct output
  expect_equal(obj,factor(c('sea','land')))
  
  #obj1 <- matrix(c(-24, NA, 37, 55), nrow=2,ncol=2, byrow=TRUE)
  #NA entries in matrix do not produce error
  #expect_error(findLand(obj1))
  
  
  
  #obj2 <- data.frame(long = c(-24, 37), lat =  c(31,55))
  #expect_error(obj2 <- findLand())
})



