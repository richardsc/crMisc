gradient <- function(f, x, y, dx, dy)
{
  nx <- dim(f)[1]
  ny <- dim(f)[2]

  if ( (!missing(x) & missing(y)) | (missing(x) & !missing(y)) )
    stop('Must provide both x and y')
  if ( (!missing(x) | !missing(y)) & (!missing(dx) | !missing(dy)) )
    stop('Must provide either (x,y) or (dx,dy)')
  if ( (!missing(dx) & missing(dy)) ) {
    warning('Assuming dx=dy')
    dy <- dx
  }
  if ( (missing(dx) & !missing(dy)) ) {
    warning('Assuming dx=dy')
    dx <- dy
  }
  if (!missing(dx)) {
    x <- seq(0, dx*(nx-1), dx)
    y <- seq(0, dy*(ny-1), dy)
  }
  
  gx <- f*0
  gy <- f*0
  ## do cols first:
  ## forward/backward difference on left and right
  gy[,1] <- (f[,2] - f[,1])/(y[2] - y[1])
  gy[,ny] <- (f[,ny] - f[,(ny-1)])/(y[ny] - y[ny-1])

  ## centred difference on interior points
  gy[,2:(ny-1)] <- (f[,3:ny] - f[,1:(ny-2)])/(y[3:ny] - y[1:(ny-2)])

  ## now do rows:
  ## forward/backward difference on top and bottom
  gx[1,] <- (f[2,] - f[1,])/(x[2] - x[1])
  gx[nx,] <- (f[nx,] - f[(nx-1),])/(x[nx] - x[nx-1])

  ## centred difference on interior points
  gx[2:(nx-1),] <- (f[3:nx,] - f[1:(nx-2),])/(x[3:nx] - x[1:(nx-2)])

  return(list(gx=gx, gy=gy))
}
