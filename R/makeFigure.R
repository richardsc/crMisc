makeFigure <- function(file, kind='pdf',
                       device=getOption('device'), type='Xlib',
                       imtype='cairo',
                       width=7, height=7, units='in', res=300, quality=95,
                       ...) {
  if (missing(file) & kind=='pdf') file <- 'Rplot'
  if (missing(file) & kind=='png') file <- 'Rplot-%03d'
  if (missing(file) & kind=='jpeg') file <- 'Rplot-%03d'
  if (missing(file) & kind=='tiff') file <- 'Rplot-%03d'
  if (!interactive()) {
    if (kind=='pdf') {
      pdf(paste(file, '.pdf', sep=''), width=width, height=height, ...)
    } else if (kind=='png') {
      png(paste(file, '.png', sep=''), width=width, height=height, units=units, res=res, type=imtype, ...)
    } else if (kind=='jpeg') {
      jpeg(paste(file, '.jpg', sep=''), width=width, height=height, units=units, res=res, quality=quality, type=imtype, ...) 
    } else if (kind=='tiff') {
      tiff(paste(file, '.tiff', sep=''), width=width, height=height, units=units, res=res, antialias='default', ...)
    } else {
      stop('kind not recognized: use pdf, png, or tiff.')
    }
  } else {
    if (is.null(dev.list())) {
        if (device=='x11') {
            x11(type=type, width=width, height=height, ...)
        } else if (device=='cairo') {
            x11(type='cairo', width=width, height=height, ...)
        } else if (device=='quartz') {
            quartz(width=width, height=height, ...)
        } else {
            warning(paste('Unknown device:', device))
        }
    }
  }
}

closeFigure <- function(dev=dev.cur()) {
    if (!interactive()) dev.off(dev)
}
