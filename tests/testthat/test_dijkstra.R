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


