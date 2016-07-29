\name{lls}
\alias{lls}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{A more descriptive version of \code{ls}.
%%  ~~function to do ... ~~
}
\description{A more descriptive version of \code{ls}.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
lls(name, pos = -1, envir = as.environment(pos), all.names = FALSE, pattern, classFilter)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{name}{
%%     ~~Describe \code{name} here~~
}
  \item{pos}{
%%     ~~Describe \code{pos} here~~
}
  \item{envir}{
%%     ~~Describe \code{envir} here~~
}
  \item{all.names}{
%%     ~~Describe \code{all.names} here~~
}
  \item{pattern}{
%%     ~~Describe \code{pattern} here~~
}
  \item{classFilter}{
%%     ~~Describe \code{classFilter} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(name, pos=-1, envir=as.environment(pos), all.names=FALSE, 
  pattern, classFilter)
{ 
  if( !missing(name) )
  {
    envir = name
  } else
  {
    envir = parent.frame()
  }
  
  lsList = ls(name, pos, envir, all.names, pattern)   

  cat("\nName: Class, Length, Size\n")
  cat("-------------------------\n")
  for( item in lsList )
  {
    realItem = eval(parse(text = item), envir)
    itemClass = class(realItem)
    itemSize = object.size(realItem)
    itemDimension = paste(dim(realItem), collapse="x" )
    if( itemDimension == "" )
    {
      itemDimension = length(realItem)
    }
    
    classFilterMatches = !missing(classFilter) && itemClass == classFilter
    if( classFilterMatches || missing(classFilter) )
    {
      format(cat(item, ": ", itemClass, ", ", itemDimension, ", ", itemSize, 
"\n", 
        sep=""), justify="centre")
    } 
  } 
  cat("\n") 
  
  invisible(lsList)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
