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
  
  #Pass the node names to areConnected - (is this the behviours we wanted?)
  expect_true(areConnected(x = max_set, nodes = getNodes(max_set)))
  expect_error(areConnected(x = max_set, nodes = coords_max_set),"Some specified nodes were not found in the gGraph object.")
  
  #Check error if non gGraph object is passed
  expect_error(areConnected(x = hgdp, nodes = getNodes(max_set)),"x is not a valid gGraph object")
  
  
  
})





test_that("isConnected works on a gGraph",{

  max_set <- keepMaxConnectedSet(worldgraph.10k)
  expect_true(isConnected(max_set))
  
  
})


test_that("isConnected works on a gData",{
  
  # Select African populations Mandenka, Yoruba, and Biaka
  hgdp_sub <- hgdp[c(29,30,31),]
  
  #Check they are correctly identified as connected
  expect_true(isConnected(hgdp_sub))
  
  # But populations anywhere on the globe are also connected - is this the behavior we want?
  
  # "AMERICA", "EUROPE", "CENTRAL_SOUTH_ASIA",  "AFRICA"  
  hgdp_sub <- hgdp[c(24,1,13,27),]
  expect_true(isConnected(hgdp_sub))
  
})


test_that("isReachable works with a gData object",{
  
  
  # Select African populations Mandenka, Yoruba, and Biaka
  hgdp_sub <- hgdp[c(29,30,31),]
  
  #Get a location that is reachable 
  location <- getCoords(hgdp[32,])
  
  #Check these are reachable
  expect_true(all(isReachable(x = hgdp_sub, loc = location)))
  
  #Create a gGraph
  max_set <- keepMaxConnectedSet(worldgraph.10k)
  
  #Check error given when isReachable is not given a gData
  expect_error(isReachable(x = max_set, loc = location),"x is not a valid gData object.")
  
  #Create a new gData object
  Bordeaux <- c(-1, 45)
  Malaga <- c(-4, 37)
  Zagreb <- c(16, 46)
  cities.dat <- rbind.data.frame(Bordeaux, Malaga, Zagreb)
  colnames(cities.dat) <- c("lon", "lat")
  cities.dat$pop <- c(1e6, 5e5, 1.2e6)
  row.names(cities.dat) <- c("Bordeaux", "Malaga", "Zagreb")
  cities <- new("gData", coords = cities.dat[, 1:2], data = cities.dat[, 3, drop = FALSE])
  
  #Pick a arbitrary location
  location3 <- c(46,2)
  
  #Check error is triggered if no corresponding gGraph for this gData object can be found
  expect_error(isReachable(x = cities, loc = location3),"not associated with a gGraph object") 
  
  #Pick a location 
  location2 <- getCoords(hgdp[49,])
  
  #Check this is false 
  expect_warning(res <- isReachable(x = hgdp_sub, loc = location2),"The reference node is not connected to any node.") 
  expect_false(res)
  
  
})



