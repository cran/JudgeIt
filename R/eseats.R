`eseats` <-
function (judgeit.object,year,Ev) {
  mu <- judgeit.object$mu
  std <- judgeit.object$std
  pwin <- judgeit.object$pwin
  out <- NULL
  out$delta <- Ev-judgeit.object$meanvote
  rcmat <- crossadd (mu,out$delta)
  findp <- function (colt) {
    xx <- c(1-pnorm(0.5,colt,std),v2s(judgeit.object$extra.districts[,1]))
    ww <- c(judgeit.object$seatvec,judgeit.object$extra.districts[,3])
    weighted.mean(xx,ww)
  }
  out$Es <- apply (rcmat,2,findp)
  return(out)
}

