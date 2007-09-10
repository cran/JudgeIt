`sv` <-
function (judgeit.object,year,Ev) {
  if (is.null(Ev)) stop ("No mean vote shares specified.")
  eres <- eseats (judgeit.object,year,Ev)
  vsim0 <- hyp.elects(judgeit.object,year,truncit=F)
  SDsv <- Ev
  for (ii in 1:length(Ev)) {
    vsim <- vsim0+eres$delta[ii]
    vsim <- rb(vsim,judgeit.object$extra.districts[,1]) #a difference here. 9-6-05
    vbar <- apply(trunc01(vsim),2,weighted.mean,c(judgeit.object$weightvec,judgeit.object$extra.districts[,4]))
    sbar <- apply(v2s(vsim),2,weighted.mean,c(judgeit.object$seatvec,judgeit.object$extra.districts[,3])) 
    SDsv[ii] <- sqrt(var(sbar)-(cov(vbar,sbar)^2/var(vbar)))  #conditional variance formula.
  }
  out <- NULL
  out$Es <- eres$Es
  out$SDsv <- SDsv
  out <- as.data.frame(out); rownames(out) <- Ev; colnames(out) <- c("Seats Mean","SD"); out
}

