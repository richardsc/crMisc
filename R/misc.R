findNearest <- function(x, value, na.val=-9999) {
    if (inherits(x, 'POSIXt')) x <- as.numeric(x); value <- as.numeric(value)
    na <- is.na(x)
    x[na] <- na.val
    out <- NULL
    for (i in 1:length(value)) {
        outtmp <- which(abs(x-value[i])==min(abs(x-value[i])))
        if (length(outtmp) > 1) outtmp <- outtmp[1] ## simple way to resolve ties
        out <- c(out, outtmp)
    }
    return(out)
}

lls <- function(name, pos=-1, envir=as.environment(pos), all.names=FALSE, 
  pattern, classFilter)
{ 
  if( !missing(name) )
  {
    envir = name
  } else
  {
    envir = parent.frame()
  }
  
  lsList = ls(name, pos, envir, all.names, pattern)   

  cat("\nName: Class, Length, Size\n")
  cat("-------------------------\n")
  for( item in lsList )
  {
    realItem = eval(parse(text = item), envir)
    itemClass = class(realItem)
    itemSize = object.size(realItem)
    itemDimension = paste(dim(realItem), collapse="x" )
    if( itemDimension == "" )
    {
      itemDimension = length(realItem)
    }
    
    classFilterMatches = !missing(classFilter) && itemClass == classFilter
    if( classFilterMatches || missing(classFilter) )
    {
      format(cat(item, ": ", itemClass, ", ", itemDimension, ", ", itemSize, 
"\n", 
        sep=""), justify="centre")
    } 
  } 
  cat("\n") 
  
  invisible(lsList)
}

plotCircle <- function(centre=c(0,0), radius=1, ...) {
  nr <- length(radius)
  th <- c(seq(0, 2*pi, 2*pi/60))
  for (i in 1:nr) {
    if (is.null(dim(centre))) {
      circle <- list(x=radius[i]*cos(th)+centre[1], y=radius[i]*sin(th)+centre[2])
      lines(circle, ...)
    } else {
      circle <- list(x=radius[i]*cos(th)+centre[i,1], y=radius[i]*sin(th)+centre[i,2])
      lines(circle, ...)
    }
  }
}

symLim <- function(x, ceil=FALSE) {
  maxvalue <- max(abs(range(x, na.rm=TRUE)))
  if (ceil) maxvalue <- ceil(maxvalue)
  c(-maxvalue, maxvalue)
}

movingAverage <- function(x, n=11, ...)
{
  f <- rep(1/n, n)
  xf <- as.numeric(stats::filter(x, f, ...)) # as.numeric is to keep it from getting cast as a "ts"
}

logPlot <- function(x, y, xlim=NULL, ylim=NULL, majGrid=TRUE, minGrid=TRUE, minGrid.ticks=TRUE, majGrid.lty=2, minGrid.lty=majGrid.lty, majGrid.lwd=1, minGrid.lwd=majGrid.lwd/2, majGrid.col='lightgrey', minGrid.col=majGrid.col, xlab='', ylab='', labelType='exponent', asp=1, ...)
{
  plot(log10(x), log10(y), xlim=xlim, ylim=ylim, xaxt='n', yaxt='n', xlab=xlab, ylab=ylab, asp=asp, ...)
  if (missing(xlim)) {
    xAxLab <- unique(round(pretty(log10(x)))) # make sure whole number exponentsb
  } else {
    xAxLab <- round(pretty(xlim)) # make sure whole number exponentsb
  }
  if (labelType=='exponent') {
    xAxLabText <- vector('expression', length(xAxLab))
    for (i in seq_along(xAxLab)) {
      xAxLabText[i] <- as.expression(substitute(10^{exp}, list(exp=xAxLab[i])))
      ## if (xAxLab[i] == 0) xAxLabText[i] <- as.expression(1)
      ## if (xAxLab[i] == 1) xAxLabText[i] <- as.expression(10)
    }
  } else {
    xAxLabText <- 10^xAxLab
  }

  if (missing(ylim)) {
    yAxLab <- unique(round(pretty(log10(y)))) 
  } else {
    yAxLab <- round(pretty(ylim))
  }
  if (labelType=='exponent') {
    yAxLabText <- vector('expression', length(yAxLab))
    for (i in seq_along(yAxLab)) {
      yAxLabText[i] <- as.expression(substitute(10^{exp}, list(exp=yAxLab[i])))
      ## if (yAxLab[i] == 0) yAxLabText[i] <- as.expression(1)
      ## if (yAxLab[i] == 1) yAxLabText[i] <- as.expression(10)
    }
  } else {
    yAxLabText <- 10^yAxLab
  }

  if (minGrid | minGrid.ticks) {
    xStart <- min(xAxLab) - 1
    xEnd <- max(xAxLab) + 1
    xMajTmp <- seq(xStart, xEnd, 1)
    xMinAx <- NULL
    for (i in 1:(length(xMajTmp)-1)) {
      xMinAx <- c(xMinAx, log10(seq(10^xMajTmp[i], 10^xMajTmp[i+1], length.out=10)))
    }
    yStart <- min(yAxLab) - 1
    yEnd <- max(yAxLab) + 1
    yMajTmp <- seq(yStart, yEnd, 1)
    yMinAx <- NULL
    for (i in 1:(length(yMajTmp)-1)) {
      yMinAx <- c(yMinAx, log10(seq(10^yMajTmp[i], 10^yMajTmp[i+1], length.out=10)))
    }
    if (minGrid) {
      abline(v=xMinAx, lty=minGrid.lty, col=minGrid.col, lwd=minGrid.lwd)
      abline(h=yMinAx, lty=minGrid.lty, col=minGrid.col, lwd=minGrid.lwd)
    }
    if (minGrid.ticks) {
      axis(1, at=xMinAx, labels=FALSE, tcl=par('tcl')/2)
      axis(2, at=yMinAx, labels=FALSE, tcl=par('tcl')/2)
    }
  }
  axis(1, at=xAxLab, labels=xAxLabText)
  axis(2, at=yAxLab, labels=yAxLabText)

  if (majGrid) {
    abline(v=xAxLab, lty=majGrid.lty, col=majGrid.col)
    abline(h=yAxLab, lty=majGrid.lty, col=majGrid.col)
  }

}

logPoints <- function(x, y, ...)
{
  points(log10(x), log10(y), ...)
}

logLines <- function(x, y, ...)
{
  lines(log10(x), log10(y), ...)
}

axis10exp <- function(side, constantExp=TRUE, at=NULL, labels=TRUE, tick=TRUE, line=NA,
                      pos=NA, outer=FALSE, font=NA, lty='solid',
                      lwd=1, lwd.ticks=lwd, col=NULL, col.ticks=NULL,
                      hadj=NA, padj=NA, ...)
{
    if (is.null(at)) at <- axTicks(side)
    atexp <- round(log10(abs(at)))
    zero <- is.infinite(atexp)
    atexp[zero] <- 0
    if (constantExp) { # find the smallest non-zero exponent
        zeroExp <- atexp == 0
        newexp <- min(atexp[!zeroExp])
        atexp[!zeroExp] <- newexp
    }
    atbase <- at/10^atexp
    atlabel <- NULL
    for (ilab in seq_along(at)) {
        if (zero[ilab]) {
            lab <- expression(0)
        } else if (atexp[ilab]==0) {
            lab <- substitute(b, list(b=atbase[ilab]))
        } else {
            lab <- substitute(b%*%10^e, list(b=atbase[ilab], e=atexp[ilab]))
        }
        atlabel <- c(atlabel, lab)
    }
    atlabel <- as.expression(atlabel)
    axis(side, at=at, labels=atlabel, tick, line,
                      pos, outer, font, lty,
                      lwd, lwd.ticks, col, col.ticks,
                      hadj, padj, ...)
}
