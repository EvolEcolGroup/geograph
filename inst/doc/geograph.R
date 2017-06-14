## ----setup, echo=FALSE---------------------------------------------------
knitr::opts_chunk$set(fig.width = 7, fig.height = 6, fig.path="figs/")
options(digits = 4)

## ----eval = FALSE--------------------------------------------------------
#  source("https://bioconductor.org/biocLite.R")
#  biocLite("graph")
#  biocLite("RBGL")

## ----eval = FALSE--------------------------------------------------------
#  install.packages("devtools")

## ----eval = FALSE--------------------------------------------------------
#  library(devtools)
#  install_github("thibautjombart/geoGraph")

## ----load, message = FALSE-----------------------------------------------
library("geoGraph")

## ------------------------------------------------------------------------
getClass("gGraph")

## ------------------------------------------------------------------------
new("gGraph")

## ------------------------------------------------------------------------
getClass("gData")

## ------------------------------------------------------------------------
new("gData")

## ----eval=FALSE----------------------------------------------------------
#  ?geoGraph

## ----eval=FALSE----------------------------------------------------------
#  help("geoGraph", package="geoGraph", html=TRUE)

## ----eval=FALSE----------------------------------------------------------
#  options(htmlhelp = FALSE)

## ----cities--------------------------------------------------------------
Bordeaux <- c(-1,45)
London <- c(0,51)
Malaga <- c(-4,37)
Zagreb <- c(16,46)
cities.dat <- rbind.data.frame(Bordeaux, London, Malaga, Zagreb)
colnames(cities.dat) <- c("lon","lat")
cities.dat$pop <- c(1e6, 13e6, 5e5, 1.2e6)
row.names(cities.dat) <- c("Bordeaux","London","Malaga","Zagreb")
cities.dat

## ----wg10plot, fig = TRUE, fig.width = 8---------------------------------
worldgraph.10k
plot(worldgraph.10k)

## ----citiesplot, fig=TRUE------------------------------------------------
cities <- new("gData", coords=cities.dat[,1:2], data=cities.dat[,3,drop=FALSE], gGraph.name="worldgraph.10k")
cities
plot(cities, type="both", reset=TRUE)
plotEdges(worldgraph.10k)

## ----closeNode, fig=TRUE-------------------------------------------------
cities <- closestNode(cities, attr.name="habitat", attr.value="land")
plot(cities, type="both", reset=TRUE)
plotEdges(worldgraph.10k)

## ------------------------------------------------------------------------
getCoords(cities)
getNodes(cities)
getData(cities)

## ------------------------------------------------------------------------
getCoords(cities, original=FALSE)

## ------------------------------------------------------------------------
getNodesAttr(cities)

## ----wg10kdefplot, fig=TRUE, fig.width=8---------------------------------
worldgraph.10k@meta$colors
head(getNodesAttr(worldgraph.10k))
table(getNodesAttr(worldgraph.10k))
plot(worldgraph.10k, reset=TRUE)
title("Default plotting of worldgraph.10k")

## ----eval=FALSE----------------------------------------------------------
#  X11.options(type="Xlib")

## ----eval=FALSE----------------------------------------------------------
#  X11.options(type="cairo")

## ----eval=FALSE----------------------------------------------------------
#  geo.zoomin()

## ----eval=FALSE----------------------------------------------------------
#  geo.zoomout()

## ----eval=FALSE----------------------------------------------------------
#  geo.slide()

## ------------------------------------------------------------------------
.geoGraphEnv
ls(env=.geoGraphEnv)
get("last.plot", .geoGraphEnv)

## ----citiesPlot2, fig=TRUE-----------------------------------------------
plot(cities, reset=TRUE)
text(getCoords(cities), rownames(getData(cities)))

## ----fig=TRUE------------------------------------------------------------
transp <- function(col, alpha=.5){
    res <- apply(col2rgb(col),2, function(c) rgb(c[1]/255, c[2]/255, c[3]/255, alpha))
    return(res)
}


plot(cities, reset=TRUE)
par(xpd=TRUE)
text(getCoords(cities)+-.5, rownames(getData(cities)))
symbols(getCoords(cities)[,1], getCoords(cities)[,2], circ=sqrt(unlist(getData(cities))), inch=.2, bg=transp("red"), add=TRUE)

## ----fig=TRUE------------------------------------------------------------
geo.zoomin(c(35,54,-26,-10))
plotEdges(rawgraph.10k)

## ------------------------------------------------------------------------
geo.bookmark("madagascar")

## ------------------------------------------------------------------------
rawgraph.10k@meta$costs
newGraph <- rawgraph.10k
newGraph@meta$costs[2:6,2] <- 100
newGraph@meta$costs[1,2] <- 1
newGraph@meta$costs

## ----fig = TRUE, fig.width = 8-------------------------------------------
newGraph <- setCosts(newGraph, attr.name="habitat")
plot(newGraph,edge=TRUE)

## ----fig=TRUE------------------------------------------------------------
newGraph <- dropDeadEdges(newGraph, thres=1.1)
plot(newGraph,edge=TRUE)

## ----fig=TRUE------------------------------------------------------------
geo.zoomin(c(110,130,-27,-12))
geo.bookmark("australia")

## ----eval=FALSE----------------------------------------------------------
#  geo.goto("autralia")
#  newGraph <- geo.remove.edges(newGraph)

## ----eval=TRUE, echo=FALSE-----------------------------------------------
load("Robjects/newGraph.RData")

## ----eval=FALSE----------------------------------------------------------
#  plot(newGraph, edge=TRUE)
#  temp <- geo.change.attr(newGraph, mode="area", attr.name="habitat", attr.value="shallowwater", newCol="deepskyblue")
#  temp <- geo.change.attr(temp, attr.name="habitat", attr.value="shallowwater", newCol="deepskyblue")
#  newGraph <- temp

## ----echo=FALSE----------------------------------------------------------
load("Robjects/newGraph2.RData")

## ----fig=TRUE------------------------------------------------------------
newGraph@meta$colors
plot(newGraph,edge=TRUE)

## ------------------------------------------------------------------------
library(maptools)
world.countries <- readShapePoly(system.file("files/shapefiles/world-countries.shp",package="geoGraph"))
class(world.countries)
summary(world.countries)

## ------------------------------------------------------------------------
summary(getNodesAttr(worldgraph.10k))
newGraph <- extractFromLayer(worldgraph.10k,  layer=world.countries, attr=c("CONTINENT","NAME"))
summary(getNodesAttr(newGraph))

## ----fig=TRUE------------------------------------------------------------
temp <- unique(getNodesAttr(newGraph)$"NAME")
col <- c("transparent", rainbow(length(temp)-1))
colMat <- data.frame(NAME=temp, color=col)
head(colMat)
tail(colMat)
plot(newGraph, col.rules=colMat, reset=TRUE)

## ------------------------------------------------------------------------
cities.dat
cities <- new("gData", coords=cities.dat[,1:2], data=cities.dat[,3,drop=FALSE], gGraph.name="newGraph")
cities <- closestNode(cities, attr.name="habitat", attr.value="land")
getData(cities)
getNodesAttr(cities)

## ----fig=TRUE, fig.width = 8---------------------------------------------
hgdp
plot(hgdp, reset=TRUE)

## ------------------------------------------------------------------------
isConnected(hgdp)

## ----connectivityPlot, fig.width = 8-------------------------------------
connectivityPlot(worldgraph.10k, edges=TRUE, seed=1)

## ----fig=TRUE------------------------------------------------------------
geo.zoomin(c(90,150,18,-25))
title("Different connected components\n in worldgraph.10k")

## ------------------------------------------------------------------------
myGraph <- dropCosts(worldgraph.40k)
hgdp@gGraph.name <- "myGraph"
addis <- cbind(38,9)
ori <- closestNode(myGraph, addis)
paths <- dijkstraFrom(hgdp, ori)

## ----fig=TRUE, fig.width = 8---------------------------------------------
addis <- as.vector(addis)
plot(newGraph, col=NA, reset=TRUE)
plot(paths)
points(addis[1], addis[2], pch="x", cex=2)
text(addis[1]+35, addis[2], "Addis abeba", cex=.8, font=2)
points(hgdp, col.node="black")

## ----fig=TRUE------------------------------------------------------------
div <- getData(hgdp)$"Genetic.Div"
dgeo.unif <- gPath2dist(paths, res.type="vector")
plot(div~dgeo.unif, xlab="GeoGraphic distance (arbitrary units)", ylab="Genetic diversity")
lm.unif <- lm(div~dgeo.unif)
abline(lm.unif, col="red")
summary(lm.unif)
title("Genetic diversity vs geographic distance \n uniform costs ")

## ------------------------------------------------------------------------
myGraph@meta$costs[7,] <- c("coast", 0.25)
myGraph@meta$costs
myGraph <- setCosts(myGraph, attr.name="habitat")
paths.2 <- dijkstraFrom(hgdp, ori)

## ----fig=TRUE, fig.width = 8---------------------------------------------
plot(newGraph, col=NA, reset=TRUE)
plot(paths.2)
points(addis[1], addis[2], pch="x", cex=2)
text(addis[1]+35, addis[2], "Addis abeba", cex=.8, font=2)
points(hgdp, col.node="black")

## ----fig=TRUE------------------------------------------------------------
dgeo.hab <- gPath2dist(paths.2, res.type="vector")
plot(div~dgeo.hab, xlab="GeoGraphic distance (arbitrary units)", ylab="Genetic diversity")
lm.hab <- lm(div~dgeo.hab)
abline(lm.hab, col="red")
summary(lm.hab)
title("Genetic diversity vs geographic distance \n habitat costs ")

