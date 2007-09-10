`freq` <-
function (judgeit.object,year,vprop) { #,condition=NULL) { 
#  if (is.null(condition)) condition <- 1:length(judgeit.object$mu)
  sim <- hyp.elects(judgeit.object,year,truncit=T)
  sim <- rb(sim,judgeit.object$extra.districts[,1])
  seats <- c(judgeit.object$seatvec,judgeit.object$extra.districts[,3])
  
  sim2 <- apply(inrange(sim,vprop[1],vprop[2]),2,weighted.mean,seats)
  out <- array(c(mean(sim2),sd(sim2)),c(1,2))
  colnames(out) <- c("Mean Seat Share","SD")
  rownames(out) <- paste(signif(vprop[1],3),"-",signif(vprop[2],3),sep="")
  out
}

