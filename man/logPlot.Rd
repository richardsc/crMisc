\name{logPlot}
\alias{logPlot}
\title{Make a nice log-log plot (base 10)}
\description{
  Make a nice log-log plot (base 10), with exponent labels and logged gridlines.
}
\usage{
logPlot(x, y, xlim=NULL, ylim=NULL,
	   majGrid=TRUE, minGrid=TRUE, 
	   minGrid.ticks=TRUE, 
	   majGrid.lty=2, minGrid.lty=majGrid.lty, 
	   majGrid.lwd=1, minGrid.lwd=majGrid.lwd/2, 
	   majGrid.col='lightgrey', minGrid.col=majGrid.col, 
	   xlab='', ylab='', labelType='exponent', 
	   asp=1, 
	   ...)
}
\arguments{

  \item{x,y}{the coordinates of points in the plot, not log-transformed.}

  \item{xlim, ylim}{optional xlim and ylim}

  \item{majGrid,minGrid}{logicals specifying whether to draw major and minor grids.}

  \item{minGrid.ticks}{logical specifying whether to draw the minor grid ticks on the axes.}

  \item{majGrid.lty,majGrid.lwd,majGrid.col,minGrid.lty,minGrid.lwd,minGrid.col}{line parameters for the grid.}

  \item{xlab,ylab}{x and y labels.}

  \item{labelType}{default is an exponent with base 10, otherwise just labels with the number (e.g. 0.001).}

  \item{asp}{the aspect ratio of the plot. Defaults to 1 because otherwise it will probably just look ugly.}

  \item{...}{other plotting parameters.}

}
\details{
  Makes a nice log-log plot with major and minor grids, as well as
  nicely formatting axis labels. Works best for data that span at
  least one order of magnitude (for labelling).
}
\value{
  No return value.
}
\examples{

x <- abs(rnorm(1000))
y <- abs(rnorm(1000))
logPlot(x, y, xlab='x', ylab='y')

}
\author{Clark Richards}
