\name{gGraphHistory}
% \docType{class}
\alias{gGraphHistory}
% \alias{gGraphHistory-class}
% \alias{[,gGraphHistory-method}
% \alias{getDates,gGraphHistory-method}
% \alias{initialize,gGraphHistory-method}
% \alias{show,gGraphHistory-method}
% \alias{is.gGraphHistory}
% \title{Formal class "gGraphHistory"}
\title{Disabled functionality}
\description{Disabled functionality}
% \description{Objects of the class \code{gGraphHistory} are designed to
%   store the history of modifications made to a \code{gGraph}
%   object. Each modification is stored in three components: the command
%   (unevaluated expression) that changed the object, the date (in POSIXct
%   format) and an optional comment (character string).}
% \section{Objects from the class \code{gGraphHistory}}{
%   \code{gGraphHistory} objects can be created by calls to
%   \code{new("gGraphHistory", ...)}, where '...' can be the following
%   arguments:
%   \describe{
%     \item{\code{cmd}}{an unevaluated expression.}
%     \item{\code{date}}{a character string being a syntaxically
%       valid POSIXct expression (see \code{?POSIXct}), indicating the
%       date (to the second) at which modification occured.}
%     \item{\code{comments}}{a character string commenting the change.}
%     \item{\code{\dots}}{if another \code{gGraphHistory} object is
%       provided when creating a \code{gGraphHistory} object, the new
%       object is appended to the previous one.}
%   }
% }
% \section{Slots}{
%   \describe{
%     \item{\code{cmd}:}{A list of unevaluated expressions (see \code{?expression}).}
%     \item{\code{dates}:}{A vector of characters with a valid POSIXct
%       format (see ?POSIXct).}
%     \item{\code{comments}:}{A vector of characters}
%   }
% }
% \section{Methods}{
%   \describe{
%     \item{[}{\code{signature(x = "gGraphHistory")}: usual subsetting
%       operator. Can handle logical and numeric indices.}
%     \item{getDates}{\code{signature(x = "gGraphHistory")}: get the dates
%     of the object.}
%     \item{initialize}{\code{signature(.Object = "gGraphHistory")}: internal
%       definition of the constructor.}
%     \item{show}{\code{signature(object = "gGraphHistory")}: prints the object
%       to screen.}
%   }
% }
% \author{Thibaut Jombart (\email{t.jombart@imperial.ac.uk})}
% \seealso{
%  Related class:\cr
%   -  \code{\linkS4class{gGraph}}: slot \code{@history} in \code{gGraph}.\cr
% }
% \examples{
% showClass("gGraphHistory")

% ## object creation
% x1 <- new("gGraphHistory", comments="My first gGraphHistory")

% new("gGraphHistory", cmd=expression(plot(1:10)), comments="My nice plot", x1)

% }
% \keyword{classes}
