.onAttach <- function(libname, pkgname) {
  pkg.version <- utils::packageDescription("geoGraph", fields = "Version")

  startup.txt <- paste("\n   /// geoGraph ", pkg.version, " is loaded ////////////",
    "\n\n   > overview: '?geoGraph'",
    "\n   > tutorials/doc/questions: https://github.com/thibautjombart/geograph' \n",
    sep = ""
  )

  packageStartupMessage(startup.txt)
}



# store local temporary variables in an environment whose parent is an empty environment
# (which should be safe no matter where we are when we create it)
.geoGraphEnv <- new.env(parent = emptyenv())

## declar temporary variables
zoom.log <- matrix(c(-180, 180, -90, 90), ncol = 4)
colnames(zoom.log) <- c("x1", "x2", "y1", "y2")
temp <- list(psize = 0.5, pch = 19, col = "black")
bookmarks <- matrix(numeric(), ncol = 4)
colnames(bookmarks) <- c("x1", "x2", "y1", "y2")

## assign them to the environment
assign("zoom.log", zoom.log, envir = .geoGraphEnv)
assign("psize", 0.5, envir = .geoGraphEnv)
assign("last.plot.param", temp, envir = .geoGraphEnv)
assign("sticky.points", FALSE, envir = .geoGraphEnv)
assign("bookmarks", bookmarks, envir = .geoGraphEnv)

## remove temp variables
rm(zoom.log)
rm(temp)
rm(bookmarks)
