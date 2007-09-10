`kernel.plot` <-
function (y,y2=NULL,col=1,ty="l",new=T,grid=T,...) {
  dt <- density(y,from=0,to=1)
  if (!is.null(y2)) {dt2 <- density(y2,from=0,to=1); maxy <- max(c(dt$y),c(dt2$y))} else maxy <- max(dt$y)
  plot (c(0,1),c(0,maxy),ty="n",...)
  lines (dt$x,dt$y,ty=ty,col=col)
  if (!is.null(y2)) lines (dt2$x,dt2$y,col=col+1)
  if (grid) for (ii in 0:10) lines(rep(ii/10,2),c(0,maxy),col=8,lty=2)
}

