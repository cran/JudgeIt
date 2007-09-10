`dists` <-
function (judgeit.object,year) {  #this method works for all of the above
  mu <- judgeit.object$mu
  std <- judgeit.object$std
  pwin <- judgeit.object$pwin
  
  lnvprb <- tiep(mu,std,judgeit.object$turnoutvec) #should be turnout.
  out <- cbind(mu,std,pwin)

# juju <<- judgeit.object
  if (judgeit.object$svexpected.value.only) item <- "P(E(vote)>0.5)" else item <- "P(vote>0.5)"
  if (!judgeit.object$predict) {
    out <- cbind(judgeit.object$votevec,out)
    item2 <- c("Observed Vote","Expected Vote")
  } else item2 <- "Predicted Vote"
  colnames (out) <- c(item2,"Std. Dev.",item)
  cn <- colnames(out)
  out <- cbind (out,exp(lnvprb),lnvprb)
  colnames (out) <- c(cn,"P(decisive)","ln(P(decisive))")

  return(out)
}

