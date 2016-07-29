\name{logPoints}
\alias{logPoints}
\title{Add points to a \code{logPlot}}
\description{
  Add points to a \code{logPlot}
}
\usage{
logPoints(x, y, , ...)

}
\arguments{

  \item{x,y}{the coordinates of points in the plot, not log-transformed.}

  \item{...}{other plotting arguments; see \code{points}}

}
\details{
  Add points to a log-log plot produce with \code{logPlot}
}
\value{
  No return value.
}
\examples{

x <- abs(rnorm(1000))
y <- abs(rnorm(1000))
logPlot(x, y, xlab='x', ylab='y')
logPoints(2*x, 2*y, col=2)

}
\author{Clark Richards}
