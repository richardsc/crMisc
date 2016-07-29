\name{objectSize}
\alias{objectSize}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{R Object size, function needed for \code{lls}.
%%  ~~function to do ... ~~
}
\description{R Object size, function needed for \code{lls}.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
objectSize(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{object for which size is desired
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
function(x)
{
    size = .Internal(object.size(x)) 
    
    if (size<1024)
    {
        size=paste(size, "B")
    }
    else if (size>=1024 & size < 1024*1024)
    {
        size=paste( round(size/1024, 1), "KB")
    }
    else
    {
        size=paste( round( size/1024/1024, 1 ),"MB")
    }
    
    return(size)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
