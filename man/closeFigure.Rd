\name{closeFigure}
\alias{closeFigure}
\title{Close a device opened with \code{makeFigure}}
\description{
  Close a device opened with \code{makeFigure}
}
\usage{
closeFigure(dev=dev.cur())
}
\arguments{

  \item{dev}{Number of the device to close. Defaults to the current one,
  which is standard for plots make non-interactively with \code{makeFigure}.}

}
\value{
  none.
}
\details{
  This function is meant to be paired with \code{makeFigure}, and is
  just a shortcut for closing the device (for non-interactive sessions).
}
\examples{

makeFigure('test', kind='pdf', height=3)
plot(rnorm(100), rnorm(100))
closeFigure()

}
\author{Clark Richards}
