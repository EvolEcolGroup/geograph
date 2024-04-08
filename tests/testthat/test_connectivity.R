test_that("areNeighbours correctly returns neighbours",{
  
  #Test that neighbour nodes are correctly identified
  out <- areNeighbours(6303,6304,worldgraph.40k@graph)
  expect_true(out[[1]])
  
  #Test that non-neighbour nodes are correctly identified
  out2 <- areNeighbours(6303,6306,worldgraph.40k@graph)
  expect_false(out2[[1]])
  
  #Test that vectors of different lengths throw an error 
  expect_error(areNeighbours(c(6303,6304),6306,worldgraph.40k@graph),"V1 and V2 have different lengths.")
  
  #Test vectors of neighbours are correctly identified
  V1 <- c(6303,6304)
  V2 <- c(6303,6305)
  out3 <- areNeighbours(V1,V2,worldgraph.40k@graph)
  
  #Expect false - the node is not its own neighbour
  expect_false(out3[[1]])
  expect_true(out3[[2]])
  
  #Test vectors of neighbours are correctly identified
  V3 <- c("6303","6304")
  V4 <- c("6303","6305")
  out4 <- areNeighbours(V1,V2,worldgraph.40k@graph)
  
  expect_false(out4[[1]])
  expect_true(out4[[2]])
  
  
})


test_that("areConnected correctly returns neighbours",{
  
  #Test areConnected works with a gGraph and nodes set 
  max_set <- keepMaxConnectedSet(worldgraph.10k)
  coords_max_set <- getCoords(max_set)
  
  #Pass the node names to areConnected - (is this the behvious we wanted?)
  expect_true(areConnected(x = max_set, nodes = rownames(coords_max_set)))
  expect_error(areConnected(x = max_set, nodes = coords_max_set),"Some specified nodes were not found in the gGraph object.")
  
  #Check error if non gGraph object is passed
  expect_error(areConnected(x = hgdp, nodes = rownames(coords_max_set)),"x is not a valid gGraph object")
  
  
})







