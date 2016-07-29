\name{findNearest}
\alias{findNearest}
\title{Find the index for a value (or values) in a vector nearest one provided}
\description{
  Find the index for a value (or values) in a vector nearest one provided
}
\usage{
findNearest(x, value, na.val=-9999)
}
\arguments{

  \item{x}{vector in which the value is searched}

  \item{value}{single value or vector of values for which the index in
    \code{x} is desired}

  \item{na.val}{replacement value for NAs in \code{x}. May need to be
  changed depending on \code{value}}

}
\value{
  The index (or indices) for the value of \code{x} nearest \code{value}.
}
\examples{

findNearest(1:10, 5.4)

}
\author{Clark Richards}
