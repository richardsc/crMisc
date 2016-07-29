\name{axis10exp}
\alias{axis10exp}
\title{Format axis labels using pretty \code{10^exp} notation}
\description{
  Format axis labels using pretty 10^exp notation
}
\usage{
axis10exp(side, constantExp=TRUE, at=NULL, labels=TRUE, tick=TRUE, line=NA,
                  pos=NA, outer=FALSE, font=NA, lty='solid',
                  lwd=1, lwd.ticks=lwd, col=NULL, col.ticks=NULL,
                  hadj=NA, padj=NA, ...)
}
\arguments{

	\item{side}{as in \code{axis}}

	\item{constantExp}{Whether to use a constant exponent for the labels or not. Defaults to TRUE.}

	\item{at}{as in \code{axis}}	

	\item{labels}{as in \code{axis}}	

	\item{tick}{as in \code{axis}}	

	\item{line}{as in \code{axis}}	

	\item{pos}{as in \code{axis}}	

	\item{outer}{as in \code{axis}}	

	\item{font}{as in \code{axis}}	

	\item{lty}{as in \code{axis}}	

	\item{lwd}{as in \code{axis}}	

	\item{lwd.ticks}{as in \code{axis}}	

	\item{col}{as in \code{axis}}	

	\item{col.ticks}{as in \code{axis}}	

	\item{hadj}{as in \code{axis}}	

	\item{padj}{as in \code{axis}}	
}
\value{
	None.
}
\details{
Formats axis labels using the \code{B times 10^E} notation, in place of the default \code{BeEE}, which tends to be ugly.
}
\examples{

plot(1/10000*rnorm(100), yaxt='n', xaxt='n')
axis10exp(1)
axis10exp(2)

}
\author{Clark Richards}
