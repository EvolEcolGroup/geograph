test_that("dijkstra_between computes distances correctly",
          {
            # test the gData method
            # we take four locations from 4 different continents:
            # "AMERICA", "EUROPE", "CENTRAL_SOUTH_ASIA",  "AFRICA"  
            hgdp_sub <- hgdp[c(24,1,13,27),]
            hgdp_between <- dijkstraBetween(hgdp_sub)
            # plot(worldgraph.40k, pch = "")
            # points(hgdp_sub, lwd = 3)
            # plot(hgdp_between)
            dist_matrix <- gPath2dist(hgdp_between)
            # expect distance between South America and any other location to be 
            # to be larger than any other distance
            expect_true(min(dist_matrix[1:3])>max(dist_matrix[4:6]))
            
            #now test the same for the gGraph algorithm
            # we extract the nodes from the gData object
            graph_between <- dijkstraBetween(worldgraph.40k,
              from = hgdp_sub@nodes.id,
              to = hgdp_sub@nodes.id)
            graph_dist_matrix <- gPath2dist(graph_between)
            expect_true(identical(dist_matrix, graph_dist_matrix))
          })



#-this second test currently fails, problem with dijkstraFrom gGraph method?

testthat::test_that("DijkstraFrom works on a connected graph",{
  
  max_set <- keepMaxConnectedSet(worldgraph.10k)
  isConnected(max_set)
  
  #Choose a start point within the graph space
  coords_max_set <- getCoords(max_set)
  head(coords_max_set)
  #node 67 
  origin <- "67"
  dijkstraFrom(max_set,origin)
  
})

testthat::test_that("DijkstraFrom works on a gData object", {
  
  #Create a subset of hgdp data 
  hgdp_sub <- hgdp[c(1,2,3,4)]
  
  #Choose an origin node 
  start <- "24988"
  
  myPath <- dijkstraFrom(hgdp_sub, start)
  
  #Check that myPath has the expected pairs of nodes 
  testthat::expect_equal(names(myPath), c("24988:26898", "24988:11652", "24988:22532", "24988:23709"))
  #Bear in mind that so far this doesn't check the full structure of resulting gPath
  #gPath is not a formal defined class in classes.R @TODO ?
  
  #Check how dijkstra.sp (used in gData method for dijkstrafrom) 
  #differs from sp.between (used in gGraph method for dijkstraFrom)
  
  
})



