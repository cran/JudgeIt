`pv` <-
function (judgeit.object,year,Ev=NULL,delta=NULL,lohi=NULL) {
  if (is.null(Ev)&is.null(delta)) stop ("Error in probability-vs-votes: no proportions or shifts specified.")
  if (is.null(lohi)|all(lohi==c(0.5,1))) lohi <- c(0.5,1e6)
  delvbar <- c(NA,NA) #no adjustments here.
  if (is.null(delta)) delta <- Ev-judgeit.object$meanvote
  
  vsim <- hyp.elects(judgeit.object,year,delvbar,truncit=F) #generate elections.
  probs <- sapply (delta,FUN=function(ii) apply(inrange(vsim+ii,lohi[1],lohi[2]),2,mean))

  out <- cbind (apply(probs,2,mean),apply(probs,2,sd))
  colnames(out) <- c("Mean","SD")
  if (!is.null(Ev)) rownames(out) <- Ev else rownames(out) <- delta
  out
}

