`winprob` <-
function (judgeit.object,year,lohi,Ev=NULL,delta=NULL) {
  if (is.null(lohi)) lohi <- c(0,1)
  if (lohi[1]<=0) lohi[1] <- -1e6; if (lohi[2]>=1) lohi[2] <- 1e6
  vsim <- hyp.elects(judgeit.object,year,c(NA,NA),F)
  if (is.null(Ev)) delta.v <- delta else 
    if (is.null(delta)) delta.v <- Ev-judgeit.object$meanvote else
      stop("Trouble in winprob: Neither mean vote or delta specified.")
  out <- sapply(delta.v,FUN=function(ii) {
    quant <- inrange(apply(trunc01(rb(vsim+ii,judgeit.object$extra.districts[,1])),
         2,weighted.mean,c(judgeit.object$seatvec,judgeit.object$extra.districts[,3])),
                        lohi[1],lohi[2])
    c(mean(quant),sd(quant)/sqrt(judgeit.object$sims)) })
  out <- t(out)
  colnames(out) <- c("Mean","SD of Mean")
  return(out)
}

