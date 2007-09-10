`voting.power` <-
function (judgeit.object,year,vot,tot) {
  if (is.null(vot)) stop ("No voting groups have been specified.")
  twogroups <- T
  if (is.null(tot)) {writeLines ("No total population groups specified; continuing without.")
                twogroups <- F}
  if (any(dim(vot)!=dim(tot))) stop ("Dimensions of voting group and total groups don't match.")
  mu <- judgeit.object$mu
  std <- judgeit.object$std
  pwin <- 1-pnorm(0.5,mu,std)
  find.p.all <-  function (m,s,wt) {
    lvp <- tiep(m,s,wt)
    mp <-  weighted.mean(lvp,wt)
    sdp <- sqrt(sum((lvp-mp)^2)/(length(m)-1))
    cbind(exp(lvp),lvp,sdp)
  }

  turnout <- apply(vot,1,sum)
  eligible.voters <- apply(tot,1,sum)

  if (any(tot-vot<0)) stop ("The number of eligible voters in one group is smaller than the observed number.")
  
  out <- find.p.all(mu,std,turnout)
  colnames(out) <- c("P(decisive voter)","ln(P(decisive))","SD(ln(P(decisive)))")
  if (twogroups) { out <- cbind(out,
               find.p.all(mu,std,eligible.voters-turnout),
               find.p.all(mu,std,eligible.voters))
    colnames(out) <- c("P(decisive voter)","ln(P(decisive))","SD(ln(P(decisive)))",
                   "P(decisive non-voter)","ln(P(decisive))","SD(ln(P(decisive)))",
                          "P(decisive both)","ln(P(decisive))","SD(ln(P(decisive)))")
                 }
  out2 <- NULL; out2$full.results <- out

  #decisiveness of voters.
  eieio <- NULL

  p <- function(a,b) a*b
  mn <- function (a,b) a-b

  ei.worker <- function(eiwt,num) {
    vdmat <- apply (eiwt,2,p,out[,num])

    #Adding the contribution of multiple seats. Could be wrong, but hey. 9-5-05
    vdmat <- apply (vdmat,2,p,judgeit.object$seatvec)
    
    sdvdm <- t(apply (vdmat,1,mn,apply(vdmat,2,mean)))
    cbind(apply(vdmat,2,mean),sqrt(apply(sdvdm^2,2,mean)/(dim(vdmat)[1]-1)))
  }

  eieio <- cbind(ei.worker(vot,1))
  colnames (eieio) <- c("Voter Power","SD")
  
  if (twogroups) { eieio <- cbind(eieio,ei.worker(tot-vot,4),ei.worker(tot,7))
  colnames (eieio) <- c("Voter Power","SD","Non-Voter Power","SD","Member power","SD")}

  eieio <-  eieio/max(eieio[,1]) #compare all values relative to 1.
  
  out2$group.power <- eieio
  return(out2)
}

