\name{symLim}
\alias{symLim}
\title{Symmetric plot limits for plus/minus variables}
\description{
  Symmetric plot limits for plus/minus variables
}
\usage{
symLim(x)
}
\arguments{

  \item{x}{object (vector or array) for which the symmetric limits are desired}

  \item{ceil}{logical indicating whether or not to take the ceiling of the value}

}
\value{

  A two element vector containing the largest absolute value from the
  range, as a negative and positive. NA's in the input are ignored
  using the \code{na.rm=TRUE} option in \code{range()}.

}
\examples{

symLim(rnorm(1:100))

}
\author{Clark Richards}
