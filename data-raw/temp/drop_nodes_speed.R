library(geoGraph)
library(tictoc)
devtools::load_all()

x<-worldgraph.40k
myGraph <- getGraph(x)
connected_sets <- RBGL::connectedComp(myGraph)
# find the largest set
max_set <- connected_sets[[which.max(lapply(connected_sets, length))]]
max_set <- as.numeric(max_set)
# all cells NOT in the largest set need to be removed

# get the edges from the graph
edgeW <- edgeWeights(myGraph)
edgeL <- edgeL(myGraph)

tic()
# We create a new list and then copy over only the edges for which we have a node
# from the largest set
newEdgeL <- list()
for (i in 1:length(edgeL)) {
  newEdgeL[[i]] <- list()
  # if the source is in the set, we keep its edges but remove any destination not in the set
  if (i %in% max_set){
    newEdgeL[[i]]$edges <- edgeL[[i]]$edges[edgeL[[i]]$edges %in% max_set]
    newEdgeL[[i]]$weights <- edgeW[[i]][edgeL[[i]]$edges %in% max_set]
  } else { #we remove this edge
    newEdgeL[[i]]$edges <- numeric(0)
    newEdgeL[[i]]$weights <- numeric(0)
  }
}
toc()
# 6.974 secs

## use an apply function to loop over all elements
max_set
keep_selected_nodes <-function(i, edgeL, max_set){
  this_edge <- list()
  # if the source is in the set, we keep its edges but remove any destination not in the set
  if (i %in% max_set){
    this_edge$edges <- edgeL[[i]]$edges[edgeL[[i]]$edges %in% max_set]
    this_edge$weights <- edgeW[[i]][edgeL[[i]]$edges %in% max_set]
  } else { #we remove this edge
    this_edge$edges <- numeric(0)
    this_edge$weights <- numeric(0)
  }
  this_edge
}
tic()
newEdgeL2<-lapply(1:length(edgeL),FUN=keep_selected_nodes,edgeL=edgeL,max_set=max_set)
toc()
identical(newEdgeL,newEdgeL2)


## break down in two steps # not working yet????
tic()
newEdgeL3 <-lapply(1:length(edgeL),FUN=function(i) {list(edges=numeric(0), weights=numeric(0))})
for (i in max_set) {
  newEdgeL3[[i]] <- list()
  # if the source is in the set, we keep its edges but remove any destination not in the set
    newEdgeL3[[i]]$edges <- edgeL[[i]]$edges[edgeL[[i]]$edges %in% max_set]
    newEdgeL3[[i]]$weights <- edgeW[[i]][edgeL[[i]]$edges %in% max_set]
}
toc()
identical(newEdgeL,newEdgeL3)


##
