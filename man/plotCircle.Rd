\name{plotCircle}
\alias{plotCircle}
\title{Add one or more circles to a plot}
\description{
  Add one or more circles to a plot
}
\usage{
plotCircle(centre = c(0,0), radius = 1, ...)
}
\arguments{

  \item{centre}{xy coordinates for the centre of the circle. Can have
    multiple centres in the form \code{rbind(c(x1, y1), c(x2, y2), c(x3,
    y3), ...)}. If there are multiple radii, but the centre coordinates
    are the same for all, only one centre is needed.}

  \item{radius}{the desired radius. Can specify multiple radii with
    \code{c(r1, r2, r3, ...)}}
  
  \item{...}{other plotting options (line type, width, color, etc).}
}
\value{
  none.
}
\examples{

plot(rnorm(100), rnorm(100))
plotCircle(centre=c(0,0), radius=1, col='red')
plotCircle(centre=c(0,0), radius=1.2, lty=3)


}
\author{Clark Richards}
