\name{gradient}
\alias{gradient}
\title{Calculate the x and y gradients for a 2D matrix}
\description{
  Calculate the x and y gradients for a 2D matrix
}
\usage{
gradient(f, x, y, dx, dy)
}
\arguments{

  \item{f}{the 2D field for which to calculate the gradient}

  \item{x,y}{vectors of the \code{x,y} locations of the cells in \code{f}}

  \item{dx,dy}{scalar giving the \code{dx,dy} in each direction. If only \code{dx} is supplied it is assumed \code{dy} is the same and spits out a warning.}

}
\details{
  Calculates the gradient in each of the two directions, \code{x} and
  \code{y}, corresponding to rows and columns of \code{f}. Uses
  centred differencing in the interior, and forward/backward
  differencing at the edges. Is more or less equivalent to Matlab's
  gradient function, except that the sense of \code{x,y} is different
  in that they correspond to rows/columns.
}
\value{
  A list containing the directional gradients corresponding to the \code{x,y} directions.
}
\examples{

library(oce) # for drawDirectionFields
x <- seq(-2, 2, 0.2)
y <- seq(-2, 2, 0.2)
X <- matrix(expand.grid(x,y)[,1], nrow=length(x))
Y <- matrix(expand.grid(x,y)[,2], nrow=length(x))
F <- X * exp(-X^2 - Y^2) 
g <- gradient(F, dx=0.2, dy=0.2)
contour(x, y, F)
drawDirectionField(as.vector(X), as.vector(Y), g$gx, g$gy, scalex=1, add=TRUE)

}
\author{Clark Richards}
