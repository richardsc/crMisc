\name{logLines}
\alias{logLines}
\title{Add lines to a \code{logPlot}}
\description{
  Add lines to a \code{logPlot}
}
\usage{
logLines(x, y, , ...)

}
\arguments{

  \item{x,y}{the coordinates of points in the plot, not log-transformed.}

  \item{...}{other plotting arguments; see \code{lines}}

}
\details{
  Add lines to a log-log plot produce with \code{logPlot}
}
\value{
  No return value.
}
\examples{

x <- abs(rnorm(1000))
y <- abs(rnorm(1000))
logPlot(x, y, xlab='x', ylab='y')
logLines(2*x, 2*y, col=2)

}
\author{Clark Richards}
