`colm` <-
function (matrx,index) {
  #extracts columns from a matrix and keeps it in matrix form.
  out <- cbind(matrx[,index])
  colnames(out) <- index
  return(out)
}

`cropmiss` <-
function (covars,voteshare=NULL,w=NULL) {
  #crops rows with missing values from covars/voteshare/weights 
  z <- cbind(covars,voteshare,w)
  set.inc <- NULL
  set.out <- NULL
  
  for (ii in 1:(dim(z)[1])) if (!(any(is.na(z[ii,])))) set.inc <- c(set.inc,ii) else set.out <- c(set.out,ii)
  out <- NULL
  out$covars <- cbind(covars[set.inc,])
  if (!is.null(voteshare)) out$voteshare <- cbind(voteshare[set.inc])
  if (!is.null(w)) out$w <- cbind(w[set.inc])

  out$set.out <- set.out
  out$set.inc <- set.inc
  return(out)
}

`crossadd` <-
function (vert,horiz) {
  vl <- length(vert); hl <- length(horiz)
  vert <- matrix(rep(as.numeric(vert),hl),vl)
  horiz <- matrix(rep(as.numeric(horiz),vl),vl,byrow=T)
  return(vert+horiz)
}

`delunc` <-
function(voteshare,uncL,uncU) {
  #replaces uncontested vote values with "missing".
  f1 <- function (a,b,c) ifelse (is.na(a),NA,
                                 ifelse(a<b,NA, ifelse(a>c,NA,a)))
  return(sapply (voteshare,f1,uncL,uncU))
}

`fix.uncontested` <-
function (judgeit.object,same.districts,uncontesteds.method) {
  dists <- length(judgeit.object$covars)
  for (ii in 1:dists) {
    judgeit.object <- switch (uncontesteds.method,
          remove = uncontested.remove(judgeit.object,ii),
          impute = uncontested.impute(judgeit.object,ii),
          default = uncontested.default(judgeit.object,ii),
          nochange = judgeit.object)
  }
  #this seems superfluous.
  for (ii in 1:length(judgeit.object$voteshare)) {
    judgeit.object$voteshare[[ii]] <- cbind(judgeit.object$voteshare[[ii]])
    judgeit.object$covars[[ii]] <- cbind(judgeit.object$covars[[ii]])
    judgeit.object$eligible.voters[[ii]] <- cbind(judgeit.object$eligible.voters[[ii]])
    judgeit.object$turnout[[ii]] <- cbind(judgeit.object$turnout[[ii]])
    judgeit.object$seats[[ii]] <- cbind(judgeit.object$seats[[ii]])
  }
  return(judgeit.object)
}

`inrange` <-
function(r1,l,h) return(1*((r1>l)&(r1<h)))

`missed` <-
function (mat) {one <- function(row) !any(is.na(row)); return(which(apply(mat,1,one)>0))}


`model.preds` <-
function(frame) {
  checker <- attr(attr(frame,"terms"),"factors")
  preds <- NULL
  if (!is.null(dim(checker)[2])) for (ii in 1:dim(checker)[2]) {
    thinglonger <- as.matrix(frame[,which(checker[,ii]>0)])
    preds <- cbind(preds,apply(thinglonger,1,prod))
  }
  
  if (attr(attr(frame,"terms"),"intercept")) {
    preds <- cbind(rep(1,dim(frame)[1]),preds)
    colnames(preds) <- c("(Intercept)",attr(attr(frame,"terms"),"term.labels"))
  } else colnames(preds) <- attr(attr(frame,"terms"),"term.labels")
  rownames(preds) <- rownames(frame)
  return(preds)
}


`nacheck` <-
function (stuff) {
  work <- function(stiff) all(is.na(stiff))
  return(any(apply(stuff,2,work)))
}

`new.list` <-
function(len) {
  out <- list(NA)
  if (len<1) stop ("Can't make a list of length ",len,".")
  for (ii in 1:len) out[[ii]] <- NA
  return(out)
}

`mvrnorm.cribbed` <- function (n = 1, mu, Sigma, tol = 1e-06, empirical = FALSE)
{ #cribbed from MASS.
  p <- length(mu)
  if (!all(dim(Sigma) == c(p, p))) 
    stop("incompatible arguments")
  eS <- eigen(Sigma, symmetric = TRUE, EISPACK = TRUE)
  ev <- eS$values
  if (!all(ev >= -tol * abs(ev[1]))) 
    stop("'Sigma' is not positive definite")
  X <- matrix(rnorm(p * n), n)
  if (empirical) {
    X <- scale(X, TRUE, FALSE)
    X <- X %*% svd(X, nu = 0)$v
    X <- scale(X, FALSE, TRUE)
  }
  X <- drop(mu) + eS$vectors %*% diag(sqrt(pmax(ev, 0)), p) %*% t(X)
  nm <- names(mu)
  if (is.null(nm) && !is.null(dn <- dimnames(Sigma))) 
    nm <- dn[[1]]
  dimnames(X) <- list(nm, NULL)
  if (n == 1) 
    drop(X)
  else t(X)
}

`rb` <-
function (dr,bits) {
  if (length(bits)>0) out <- rbind(dr,array(bits,c(length(bits),dim(dr)[2]))) else out <- dr
  return(out)
}

`reg` <-
function (covars,voteshare,wt=rep(1,length(voteshare))) {
  #while we're aware that this is duplicate work, it remains to keep things simple. -AT, 8-1-08
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
          
`repunc` <-
function (voteshare,l,u,lr,ur) {
  f1 <- function (a,b,c,d,e) if (!is.na(a)) {
    if (a<b) a <- d else if (a>c) a <- e else a
  } else NA
  return(sapply (voteshare,f1,l,u,lr,ur))
}

`rowm` <-
function (matrx,index) {
  out <- matrix(matrx[index,],length(index))
  rownames(out) <- index
  return(out)
}

`tiep` <-
function (mean,std,tn) dnorm(0.5,mean,std,log=T)-log(tn)

`trunc01` <-
function(r1) {
  below <- r1<0
  above <- r1>1
  between <- !(below|above)
  0*r1+r1*between+1*above
}

`u.c` <-
function (x1,x2,x3) unique.columns (unique.columns(x1,x2),x3)

`unc` <-
function (r1) -1*(r1<0.05)+1*(r1>0.95)

`uncontested.default` <-
function (judgeit.object,year) {
  judgeit.object$voteshare[[year]] <- repunc(judgeit.object$voteshare[[year]],judgeit.object$uncL,judgeit.object$uncU,judgeit.object$uncLR,judgeit.object$uncUR)
  return(judgeit.object)
}

`uncontested.impute` <-
function(judgeit.object,year) {
  judgeit.object$voteshare[[year]] <- delunc(judgeit.object$voteshare[[year]],judgeit.object$uncL,judgeit.object$uncU)

  voteshare <- judgeit.object$voteshare[[year]]
  covars <- judgeit.object$covars[[year]]
  present <- judgeit.object$fullrow[[year]]
  rstf <- reg(covars[present,],voteshare[present])
  pv <- trunc01(rnorm(length(rstf$vshat),rstf$vshat,sqrt(rstf$sig2)))
  voteshare[is.na(voteshare)] <- pv[is.na(voteshare)]
  judgeit.object$voteshare[[year]] <- cbind(voteshare)
  return(judgeit.object)
}

`uncontested.remove` <-
function (judgeit.object,year) {
  judgeit.object$voteshare[[year]] <- delunc(judgeit.object$voteshare[[year]],judgeit.object$uncL,judgeit.object$uncU)
  to.keep <- which (!is.na(judgeit.object$voteshare[[year]]))
  judgeit.object$voteshare[[year]] <- rowm(judgeit.object$voteshare[[year]],to.keep)
  judgeit.object$covars[[year]] <- rowm(judgeit.object$covars[[year]],to.keep)
  judgeit.object$turnout[[year]] <- rowm(judgeit.object$turnout[[year]],to.keep)
  judgeit.object$eligible.voters[[year]] <- rowm(judgeit.object$eligible.voters[[year]],to.keep)
  judgeit.object$seats[[year]] <- rowm(judgeit.object$seats[[year]],to.keep)
  return(judgeit.object)
}

`unique.columns` <-
function (x1,x2=NULL,keepers=NULL) {
  copies <- NULL
  jo <- cbind(x1,x2)
  ju <- jo; if (!is.null(keepers)) jo <- cbind(jo[keepers,])
  #cut any completely missing columns
  somedata <- function(col) !all(is.na(col)); jo <- as.matrix(jo[,apply(jo,2,somedata)])
  j <- cropmiss(jo)$covars
  if (dim(j)[2]>1) for (i1 in 1:(dim(j)[2]-1)) for (i2 in (i1+1):dim(j)[2]) {
    cond1 <- any(j[,i2]==0)|any(j[,i1]==0)
    if (is.na(cond1)) cond1 <- F
    if (cond1) remv <- j[,i1]==j[,i2] else remv <- all(j[1,i1]/j[1,i2] == j[,i1]/j[,i2])
    remv <- remv[!is.na(remv)]
    if ((length(remv)>0)&prod(remv)) copies <- c(copies,i2)#; writeLines(paste(length(remv),prod(remv),i1,i2))}
  }  
  if (length(copies)>0) return(as.matrix(ju[,-unique(copies)])) else return(as.matrix(ju))
}

`v2s` <-
function(inp) 1*(inp>0.5) 

`vshar` <-
function(r1,r2) r1/(r1+r2)

`share` <- function(...) vshar(...)
