\name{gGraph-class}
\docType{class}
\alias{gGraph}
\alias{gGraph-class}
\alias{[,gGraph-method}
\alias{[,gGraph,ANY,ANY-method}
\alias{getCoords,gGraph-method}
% \alias{getDates,gGraph-method}
\alias{getGraph,gGraph-method}
% \alias{getHistory,gGraph-method}
\alias{getNodes,gGraph-method}
\alias{initialize,gGraph-method}
\alias{dropCosts,gGraph-method}
\alias{dropCosts}
\alias{show,gGraph-method}
\alias{is.gGraph}
% \alias{getHistory}
\alias{getGraph}
% \alias{getDates}
\alias{getCoords}
\alias{getNodes}
%%%%% THESE HAVE THEIR OWN MANPAGES
%\alias{getCosts,gGraph-method}
%\alias{closestNode,gGraph-method}
%\alias{dijkstraBetween,gGraph-method}
%\alias{dijkstraFrom,gGraph-method}
%\alias{extractFromLayer,gGraph-method}
%\alias{findLand,gGraph-method}
%\alias{getColors,gGraph-method}
%\alias{isInArea,gGraph-method}
%\alias{plot,gGraph,missing-method}
%\alias{points,gGraph-method}
%\alias{setEdges,gGraph-method}
%\alias{getEdges,gGraph-method}
%\alias{getNodesAttr}
%\alias{getNodesAttr,gGraph-method}
%%%%%%
\title{Formal class "gGraph"}
\description{The class \code{gGraph} is a formal (S4) class storing geographic
  data.\cr
  Such data are composed of a set of geographic coordinates of vertices
  (or 'nodes'), and a graph describing connectivity between these
  vertices. Data associated to the nodes can also be stored ('nodes attributes'), as well as
  meta-information used when plotting the object, or when computing
  weights associated to the edges based on nodes attributes.\cr
  % History associated to a \code{gGraph} object is stored in the slot
  % \code{history}, as an object of the class
  % \linkS4class{gGraphHistory}.\cr

  In all slots, nodes are uniquely identified by their name (reference
  is taken from the row names of \code{@coords} slot).
}
\section{Objects from the class gGraph}{
  \code{gGraph} objects can be created by calls to \code{new("gGraph",
    ...)}, where '...' can be the following arguments:

  \describe{
    \item{\code{coords}}{a matrix of spatial coordinates with two columns,
      being respectively longitude (from -180 to 180) and latitude. Positive
      numbers are intended as 'east' and 'north', respectively.}
    \item{\code{nodes.attr}}{a data.frame whose rows are nodes, and whose
      columns are different variables associated to the nodes.}
    \item{\code{meta}}{a list, most likely containing named data.frames
      (see Slots).}
    \item{\code{graph}}{an object of the class \linkS4class{graphNEL},
      from the \code{graph} package (see \code{class?graphNEL}),
      describing connectivity among nodes.}
    %   \item{\code{history}}{an object of the class
    %    \linkS4class{gGraphHistory}, storing a previous history.}
    % \item{\code{cmd}}{an unevaluated expression (see \code{?expression})
    %   passed to the \linkS4class{gGraphHistory} object.}
    % \item{\code{comments}}{a character string passed to the
    %   \linkS4class{gGraphHistory} object, commenting the object creation.}
    %  \item{\code{}}{}
    %   \item{\code{}}{}
    %   \item{\code{}}{}
  }

  Note that none of these is mandatory: \code{new("gGraph")} would work, and
  create an empty \code{gGraph} object.
}
\section{Slots}{
  \describe{
    \item{\code{coords}:}{Object of class \code{"matrix"} storing
      geographic coordinates (see above).}
    \item{\code{nodes.attr}:}{A \code{"data.frame"}
      storing nodes data (see above).}
    \item{\code{meta}:}{A \code{"list"} containing meta information
      about how data in the \code{nodes.attr} slot should be used in
      different contexts. This information is to be stored as named
      data.frames. Currently recognized meta-information are:\cr
      - \code{\$color}: a data.frame giving colors to be used when plotting
      the object, according to a node attribute taking a finite number of
      values (likely a factor). Each documented attribute is a column, the
      last one being reserved to (valid) named colors. See the
      \code{@meta\$color} slot in \code{\link{worldgraph.10k}} for an
      example.\cr
      -  \code{\$weights}: a data.frame giving weights to be used when computing
      edge weights using \code{\link{setCosts}}. The first column is a
      node attribute, and the second column gives corresponding weights. See the
      \code{@meta\$costs} slot in \code{\link{worldgraph.10k}} for an
      example.}
    \item{\code{graph}:}{Object of class \linkS4class{graphNEL} (see
      \code{class?graphNEL} for more information).}
    % \item{\code{history}:}{Object of class
    %   \linkS4class{gGraphHistory}, recording the history associated to the
    %   object.}
  }
}
\section{Methods}{
  Here is a list of methods available for \code{gGraph}
  objects. Specific manpages exist for accessors with more than one
  argument. These are indicated by a '*' symbol next to the method's name.
  \describe{
    \item{[}{\code{signature(x = "gGraph")}: usual method to subset
      objects in R. First indice corresponds to nodes (it can be a logical,
      a numeric, or a character); optionnaly, a second indice can be
      provided to subset columns of \code{nodes.attr} (i.e., variables
      associated to nodes) as well.}
    \item{closestNode}{\code{signature(x = "gGraph")}: find the node
      closest to a given location.}
    \item{dijkstraBetween}{\code{signature(x = "gGraph")}: least-cost
      path between two sets of nodes.}
    \item{dijkstraFrom}{\code{signature(x = "gGraph")}: least-cost
      path between a set of nodes and a 'source node'.}
    \item{dropCosts}{\code{signature(x = "gGraph")}: remove costs
      associated to edges.}
    \item{extractFromLayer}{\code{signature(x = "gGraph")}: retrieve
      information from a shapefile and add it to \code{nodes.attr}.}
    \item{findLand}{\code{signature(x = "gGraph")}: find which nodes are
      on land, and which are not.}
    \item{getColors*}{\code{signature(x = "gGraph")}: get colors
      corresponding to the nodes, as specified in the \code{@meta\$color} slot.}
    \item{getCoords}{\code{signature(x = "gGraph")}: get geographic
      coordinates of the object.}
    % \item{getDates}{\code{signature(x = "gGraph")}: get dates of modifications of the object.}
    \item{getEdges*}{\code{signature(x = "gGraph")}: get edges of the object.}
    \item{getGraph}{\code{signature(x = "gGraph")}: get the graph of the
      object (as a \code{graphNEL} object).}
    % \item{getHistory}{\code{signature(x = "gGraph")}: get the history of
    %   the object.}
    \item{getNodesAttr*}{\code{signature(x = "gGraph")}: get the nodes
      attributes of the object.}
    \item{getNodes}{\code{signature(x = "gGraph")}: get the names of the
      nodes of the object.}
    \item{getCosts*}{\code{signature(x = "gGraph")}: get the weights of
      the edges of the object.}
    \item{initialize}{\code{signature(.Object = "gGraph")}: internal
      definition of the constructor.}
    \item{isInArea}{\code{signature(x = "gGraph")}: check which nodes
      are in a given rectangular area.}
    \item{plot}{\code{signature(x = "gGraph", y = "missing")}: plot the object.}
    \item{points}{\code{signature(x = "gGraph")}: add points to an
      existing plot.}
    \item{setEdges*}{\code{signature(x = "gGraph")}: add/remove edges
      to/from the object.}
    \item{show}{\code{signature(object = "gGraph")}: prints the object
      to screen.}
  }

  Note that other functions not directly related to gGraph objects are
  of important use in \code{\link{geoGraph}}. See package documentation
  (\code{?geoGraph}) for a more complete list of implemented functions.
}
\author{Thibaut Jombart (\email{t.jombart@imperial.ac.uk})}

% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  Related classes are:\cr
  % -  \code{\linkS4class{gGraphHistory}}: slot \code{@history} in \code{gGraph}.\cr
  -  \code{\linkS4class{graphNEL}} (graph package): slot \code{@graph} in \code{gGraph}.\cr
}
\examples{
## create an empty object
new("gGraph")


## plotting the object
data(rawgraph.10k)
plot(rawgraph.10k, reset=TRUE)

## zooming in
geo.zoomin(list(x=c(-6,38), y=c(35,73)))
title("Europe")

## to play interactively with graphics, use:
# geo.zoomin()
# geo.zoomout()
# geo.slide()
# geo.back()

## defining a new object restrained to visible nodes
x <- rawgraph.10k[isInArea(rawgraph.10k)]
plot(x,reset=TRUE, edges=TRUE)
title("x does just contain these visible nodes.")

## define weights for edges
x <- setCosts(x, attr.name="habitat", method="prod")
plot(x,edges=TRUE)
title("costs defined by habitat (land/land=1, other=100)")

## drop 'dead edges' (i.e. with weight 0)
x <- dropDeadEdges(x, thres=10)
plot(x,edges=TRUE)
title("after droping edges with null weight")

}
\keyword{classes}
\keyword{spatial}
\keyword{graphs}
