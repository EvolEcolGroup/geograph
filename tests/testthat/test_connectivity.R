test_that("areNeighbours correctly returns neighbours",{
  
  #Test that neighbour nodes are correctly identified
  out <- areNeighbours(6303,6304,worldgraph.40k@graph)
  expect_true(out[[1]])
  
  #Test that non-neighbour nodes are correctly identified
  out2 <- areNeighbours(6303,6306,worldgraph.40k@graph)
  expect_false(out2[[1]])
  
  #Test that vectors of different lengths throw an error 
  expect_error(areNeighbours(c(6303,6304),6306,worldgraph.40k@graph),"V1 and V2 have different lengths.")
  
  #Test vectors of neighbours are corectly identified
  V1 <- c(6303,6304)
  V2 <- c(6303,6305)
  out3 <- areNeighbours(V1,V2,worldgraph.40k@graph)
  
  #Expect false - the node is not its own neighbour
  expect_false(out3[[1]])
  expect_true(out3[[2]])
  
  #Test vectors of neighbours are corectly identified
  V3 <- c("6303","6304")
  V4 <- c("6303","6305")
  out4 <- areNeighbours(V1,V2,worldgraph.40k@graph)
  
  expect_false(out4[[1]])
  expect_true(out4[[2]])
  
  
})
