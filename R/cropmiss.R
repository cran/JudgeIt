`cropmiss` <-
function (covars,voteshare=NULL,w=NULL) {
  z <- cbind(covars,voteshare,w)
  set.inc <- NULL
  set.out <- NULL 
  for (ii in 1:(dim(z)[1])) if (!(any(is.na(z[ii,])))) set.inc <- c(set.inc,ii) else set.out <- c(set.out,ii)
  out <- NULL
  out$covars <- as.matrix(covars[set.inc,])
  if (!is.null(voteshare)) out$voteshare <- matrix(voteshare[set.inc])
  if (!is.null(w)) out$w <- matrix(w[set.inc])

  out$set.out <- set.out
  out$set.inc <- set.inc
  out
}

