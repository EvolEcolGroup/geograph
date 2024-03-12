#Set up a subset for testing
hgdp_sub <- hgdp[c(24,1,13,27),]

test_that("extractFromLayer assigns points correctly",
          {
            # create a matrix of locations including two continents and the sea
            myCoords <- data.frame(long = c(-24, 71.5, -46.5), lat =  c(31, 30,-23.5))
            # assign to continents
            continents <- extractFromLayer(myCoords, layer = "world", attr = "continent")
            expect_identical(as.character(continents$continent), c(NA,"Asia","South America"))
          })



test_that("extractFromLayer works on a gData object",{
  
  res <- extractFromLayer(hgdp_sub, layer = "world", attr = "continent")
  
  # we expect continents to be "AMERICA", "EUROPE", "CENTRAL_SOUTH_ASIA",  "AFRICA" 
  expect_identical(as.character(res$continent), c("South America","Europe","Asia","Africa"))

})

test_that("extractFromLayer works on a list",{
  
  #Create a list
  hgdp_sub_list <- list(hgdp_sub@coords)
  
  # we expect continents to be "AMERICA", "EUROPE", "CENTRAL_SOUTH_ASIA",  "AFRICA" 
  res <- extractFromLayer(hgdp_sub_list, layer = "world", attr = "continent")
  expect_identical(as.character(res$continent), c("South America","Europe","Asia","Africa"))
  
})

test_that("extractFromLayer works with a single line input",{
  
  
  #works for one line
  south_america <- hgdp_sub@coords[1,]
  res <- extractFromLayer(south_america, layer = "world", attr = "continent")
  expect_identical(as.character(res$continent), c("South America"))
  
  #returns error if missing element in coordinate pairs
  
  #If we enter a vector of three values
  hgdp_sub_odd <- hgdp_sub@coords[c(1,2,3),1]
  
  expect_error(res <- extractFromLayer(hgdp_sub_odd, layer = "world", attr = "continent"),
               "Vector must have even number of longitude and latitude entries")
  
  
})


