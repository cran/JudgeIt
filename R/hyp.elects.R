`hyp.elects` <-
function(judgeit.object,year=404,delvbar=c(NA,NA),truncit=T,just.mean.sd=F) {

  mvrnorm <- function (n = 1, mu, Sigma, tol = 1e-06, empirical = FALSE)
  { #cribbed from MASS.
    p <- length(mu)
    if (!all(dim(Sigma) == c(p, p))) 
        stop("incompatible arguments")
    eS <- eigen(Sigma, sym = TRUE, EISPACK = TRUE)
    ev <- eS$values
    if (!all(ev >= -tol * abs(ev[1]))) 
        stop("'Sigma' is not positive definite")
    X <- matrix(rnorm(p * n), n)
    if (empirical) {
        X <- scale(X, TRUE, FALSE)
        X <- X %*% svd(X, nu = 0)$v
        X <- scale(X, FALSE, TRUE)
    }
    X <- drop(mu) + eS$vectors %*% diag(sqrt(pmax(ev, 0)), p) %*% 
        t(X)
    nm <- names(mu)
    if (is.null(nm) && !is.null(dn <- dimnames(Sigma))) 
        nm <- dn[[1]]
    dimnames(X) <- list(nm, NULL)
    if (n == 1) 
        drop(X)
    else t(X)
  }
  dennycrane <<- judgeit.object$baseline
  
  #baseline, mult, stoned
  bets <-
    t(mvrnorm(judgeit.object$sims,judgeit.object$beta[[year]],judgeit.object$vc[[year]]))

  if (!just.mean.sd) {
    rs <- length(judgeit.object$mu)
    elects <-
      array(rnorm(judgeit.object$sims*rs,judgeit.object$baseline,judgeit.object$stoned),c(rs,judgeit.object$sims))

  #print(dim(bets))
  
    elects <- elects+judgeit.object$mult%*%bets
  
    delta <- 0
    if (is.na(delvbar[2])&!is.na(delvbar[1])) delta <- delvbar[1] else
    if (!is.na(delvbar[2])&is.na(delvbar[1])) delta <- delvbar[2]-judgeit.object$meanvote
    elects <- elects+delta
    if (truncit) elects <- trunc01(elects)
  } else {
    delta <- 0
    if (is.na(delvbar[2])&!is.na(delvbar[1])) delta <- delvbar[1] else
    if (!is.na(delvbar[2])&is.na(delvbar[1])) delta <- delvbar[2]-judgeit.object$meanvote

    elects <- NULL
    elects$means <- judgeit.object$mult%*%bets+as.vector(judgeit.object$baseline)+delta
    elects$sds <- judgeit.object$stoned
  }

  return(elects)
}

