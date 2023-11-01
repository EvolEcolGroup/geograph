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