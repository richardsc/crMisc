\name{movingAverage}
\alias{movingAverage}
\title{Calculates a moving average of a vector using `filter`}
\description{
  Calculates a moving average of a vector using `filter`
}
\usage{
movingAverage(x, n=11, ...)
}
\arguments{

  \item{x}{the vector for which to calculate the moving average}

  \item{n}{the width of the moving average window, in indices. For a
  true symmetric moving average, \code{n} should be odd, representing
  an average of the current value, and the \code{(n-1)/2} values before
  and after.}

  \item{...}{other options for filter}

}
\details{
	Performs a symmetric moving average of the vector \code{x},
	using a linear filter of the form \code{rep(1/n, n)}, with the
	standard \code{filter()} function from the \code{stats}
	package. Note that the default options apply (but can be
	changed with the \code{...} argument), so that \code{sides=2}
	and \code{method='convolution'} .

	Currently doesn't handle missing values, so that any values
	missing in \code{x} will be missing from the result. In
	addition, the length of the vector is reduced by \code{n},
	owing to the truncation at the beginning and end of the
	series.
}
\value{
	A vector the same length as \code{x}, but with NAs at the
	beginning and end of the series (which \code{sides=2}) so that
	the total number of values is \code{length(x) - (n-1)}.
}
\examples{
x <- 1:300
y <- sin(x/20) + rnorm(300,sd=.1)
plot(x,y)
lines(x, movingAverage(y, 21), col=2) ## symmetric
lines(x, movingAverage(y, 21, sides=1), col=3) ## one-sided
}
\author{Clark Richards}
