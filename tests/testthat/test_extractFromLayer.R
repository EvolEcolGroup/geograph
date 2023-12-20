test_that("extractFromLayer assigns points correctly",
          {
            # create a matrix of locations including two continents and the sea
            myCoords <- data.frame(long = c(-24, 71.5, -46.5), lat =  c(31, 30,-23.5))
            # assign to continents
            continents <- extractFromLayer(myCoords, layer = "world", attr = "continent")
            expect_identical(as.character(continents$continent), c(NA,"Asia","South America"))
          })