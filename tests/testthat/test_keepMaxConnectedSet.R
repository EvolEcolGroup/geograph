test_that("keepMaxConnectedSet drops all small sets",
          {
            # we start with worldgraph.10k, where Eurasia and Africa form the biggest set
            # check that the graph is not all connected
            expect_false(isConnected(worldgraph.10k))
            max_set <- keepMaxConnectedSet(worldgraph.10k)
            # the max set should be all connected
            expect_true(isConnected(max_set))
            # check that the max set does not include the Americas
            coords_max_set <- getCoords(max_set)
            expect_true(sum(coords_max_set[,1]< -20)==0)
          })
            