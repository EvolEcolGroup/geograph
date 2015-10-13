\encoding{UTF-8}
\name{Auxiliary methods}
\alias{hasCosts}
\alias{rebuild}
\alias{geo.segments}
\title{Auxiliary methods for geograph}
\description{
  These methods are low-level functions called by other procedures of
  \code{geograph}. Some can, however, be useful in themselves. Note that
  unlike other functions in \code{geograph}, these functions do not
  generally test for the validity of the provided arguments (for speed
  purposes).\cr

  - \code{hasCosts}: tests whether a \linkS4class{gGraph} has costs
  associated to its edges.\cr

  - \code{geo.segments}: a substitute to \code{segments} which correctly
    draws segments between locations distant by more than 90 degrees of longitude.\cr

  - \code{rebuild}: in development.
}
\usage{
hasCosts(x)
geo.segments(x0, y0, x1, y1, col = par("fg"), lty = par("lty"), 
    lwd = par("lwd"), \dots)
}
\arguments{
  \item{x}{a valid \linkS4class{gGraph}.}
  \item{x0,y0}{coordinates of points *from* which to draw.}
  \item{x1,y1}{coordinates of points *to* which to draw.}
  \item{col}{a character string or an integer indicating the color of the segments.}
  \item{lty}{a character string or an integer indicating the type of line.}
  \item{lwd}{an integer indicating the line width.}
  \item{\dots}{further graphical parameters (from 'par') passed to the
    \code{segments} function.}
}
\value{
   For \code{hasCost}, a logical value is returned. \code{geo.segments}
   returns NULL.
}
\author{Thibaut Jombart (\email{t.jombart@imperial.ac.uk})}
\examples{
data(worldgraph.10k)
hasCosts(worldgraph.10k)
}
\keyword{utilities}
\keyword{methods}
