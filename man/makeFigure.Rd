\name{makeFigure}
\alias{makeFigure}
\title{Open a device to plot a figure}
\description{
  Open a device to plot a figure
}
\usage{
makeFigure(file, kind='pdf', device=getOption('device'), type='Xlib', imtype='cairo', width=7, height=7, units='in',
res=300, quality=95...)
}
\arguments{

  \item{file}{filename for figure when run in non-interactive mode. If no filename is given for a non-interactive mode call, pdf files are named \code{Rplot.pdf}, while png files are named \code{Rplot-\%03d.png}}

  \item{kind}{the desired kind of plot. Supports one of 'pdf', 'png', 'jpeg', or 'tiff'}

  \item{device}{the desired device, when run in interactive mode. This can be either \code{'quartz'} or \code{'x11'}. Uses whatever is set as the default if left empty.}
  
  \item{type}{the type of device, for x11 devices, when run in interactive mode. This can be 'Xlib', 'cairo', etc. See \code{?x11}}

  \item{imtype}{the type of device to use when writing to an image file (png and jpeg). Defaults to \code{'cairo'} because of the antialiasing lines on filled contour plots when using \code{'quartz'}}

  \item{width, height}{desired width and height of the figure}

  \item{units}{units to use for the figure. \code{kind='pdf'} defaults to inches, and this is usually the norm to make png files look the same, and so is the default}

  \item{res}{resolution to use for the png}

  \item{quality}{jpeg quality percentage to use; defaults to 95}

  \item{...}{other device options, including options specific to particular devices}
}
\value{
  none.
}
\details{
  This function is just a shortcut for making figures in a consistent way, to avoid copying and pasting code chunks. It will produce an on-screen figure when run in interactive mode (of default type x11). When run from the command line (e.g. using \code{R --no-save < script.R}), it will produce a graphics file of the specified kind.

\code{kind='jpeg'} uses a default quality of 95.

Note that for the file creation to function properly in non-interactive mode, the function call \emph{must} be accompanied by:

\code{if (!interactive()) dev.off()} or by \code{closeFigure()}

to turn the graphics device off.
}
\examples{

makeFigure('test', kind='pdf', height=3)
plot(rnorm(100), rnorm(100))
closeFigure()

}
\author{Clark Richards}
