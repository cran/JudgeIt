`reg` <-
function (covars,voteshare,wt=rep(1,length(voteshare))) {
  wtmatinv <- diag(as.numeric(wt)^(-1))
  mom <- t(covars)%*%wtmatinv%*%covars
  beta <- solve(mom)%*%t(covars)%*%wtmatinv%*%voteshare
    
  vshat <- covars%*%beta
  e <- voteshare-vshat
  sig2 <- as.numeric(t(e)%*%wtmatinv%*%e/(dim(covars)[1]-dim(covars)[2]))
  vc <- solve(mom)*sig2

  out <- NULL
  out$beta <- as.matrix(beta)  #beta vector.
  out$vc <- as.matrix(vc)  #variance-covariance matrix.
  out$sig2 <- sig2  #homoskedastic variance parameter.
  out$vshat <- vshat
  return(out)
}

