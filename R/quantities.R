`dists` <-
function (judgeit.object,year) {  #this method works for all of the above
  mu <- judgeit.object$mu
  std <- judgeit.object$std
  pwin <- judgeit.object$pwin
  
  lnvprb <- tiep(mu,std,judgeit.object$turnoutvec) #should be turnout.
  out <- cbind(mu,std,pwin)

  if (judgeit.object$svexpected.value.only) item <- "P(E(vote)>0.5)" else
    item <- "P(vote>0.5)"
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

`eseats` <-
function (judgeit.object,year,Ev) {
  mu <- judgeit.object$mu
  std <- judgeit.object$std
  pwin <- judgeit.object$pwin
  out <- NULL
  out$delta <- Ev-judgeit.object$meanvote
  rcmat <- crossadd (mu,out$delta)
  findp <- function (colt) {
    xx <- c(1-pnorm(0.5,colt,std),v2s(judgeit.object$extra.districts[,1]))
    ww <- c(judgeit.object$seatvec,judgeit.object$extra.districts[,3])
    weighted.mean(xx,ww)
  }
  out$Es <- apply (rcmat,2,findp)
  return(out)
}

`freq` <-
function (judgeit.object,year,vprop) { #,condition=NULL) { 
#  if (is.null(condition)) condition <- 1:length(judgeit.object$mu)
  sim <- hyp.elects(judgeit.object,year,truncit=T)
  sim <- rb(sim,judgeit.object$extra.districts[,1])
  seats <- c(judgeit.object$seatvec,judgeit.object$extra.districts[,3])
  
  sim2 <- apply(inrange(sim,vprop[1],vprop[2]),2,weighted.mean,seats)
  out <- array(c(mean(sim2),quantile(sim2,c(0.025,0.975))),c(1,3))
  colnames(out) <- c("Mean Seat Share","2.5%","97.5%")
  rownames(out) <- paste(signif(vprop[1],3),"-",signif(vprop[2],3),sep="")
  out
}

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
  
  #baseline, mult, stoned
  bets <-
    t(mvrnorm(judgeit.object$sims,judgeit.object$beta[[year]],judgeit.object$vc[[year]]))

  if (!just.mean.sd) {
    rs <- length(judgeit.object$mu)
    elects <-
      array(rnorm(judgeit.object$sims*rs,judgeit.object$baseline,judgeit.object$stoned),c(rs,judgeit.object$sims))

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

`pv` <-
function (judgeit.object,year,Ev=NULL,delta=NULL,lohi=NULL) {
  
  if (is.null(Ev)&is.null(delta)) stop ("Error in probability-vs-votes: no proportions or shifts specified.")
  if (is.null(lohi)|all(lohi==c(0.5,1))) lohi <- c(0.5,1e6)
  delvbar <- c(NA,NA) #no adjustments here.
  if (is.null(delta)) delta <- Ev-judgeit.object$meanvote
  
  vsim <- hyp.elects(judgeit.object,year,delvbar,truncit=F) #generate elections.
  probs <- sapply (delta,FUN=function(ii) apply(inrange(vsim+ii,lohi[1],lohi[2]),2,mean))

  out <- t(rbind (apply(probs,2,mean),apply(probs,2,quantile,c(0.025,0.975))))
  
  colnames(out) <- c("Mean","2.5%","97.5%")
  if (!is.null(Ev)) rownames(out) <- Ev else rownames(out) <- delta
  out
}

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

`sv` <-
function (judgeit.object,year,Ev) {
  if (is.null(Ev)) stop ("No mean vote shares specified.")
  eres <- eseats (judgeit.object,year,Ev)
  vsim0 <- hyp.elects(judgeit.object,year,truncit=FALSE,just.mean.sd=TRUE)
  vsim <- vsim0$means
  sds <- vsim0$sds

  outty <- sapply(1:length(Ev),FUN=function(ii) {
    vmn <- vsim+eres$delta[ii]
    tt <- 1-pnorm(0.5,vmn,sds)
    if (length(judgeit.object$extra.districts)>0) 
      tt <- rbind(tt,array(1*(judgeit.object$extra.districts[,1]>0.5),
                       c(dim(judgeit.object$extra.districts[,1])[1],dim(tt)[2])))
    
    res <-
      apply(tt,2,weighted.mean,c(judgeit.object$weightvec,judgeit.object$extra.districts[,4]))
    return(c(mean(res),quantile(res,c(0.025,0.975))))
  })
  
  out <- as.data.frame(t(outty))
  rownames(out) <- Ev; colnames(out) <- c("Seats Mean","2.5%","97.5%")
                  return(out)
}

`svsum` <-
function (judgeit.object,year) {
  judgeit.object$svexpected.value.only <- F; #samebg<-T
  seats <- c(judgeit.object$seatvec,judgeit.object$extra.districts[,3])
  weights <- c(judgeit.object$weightvec,judgeit.object$extra.districts[,4])
  delvbar <- c(NA,0.5)
  #quantities desired in each district: bias and responsiveness for range, center

  intvl <- 45:55/100
  out <- rbind(rep(0,4))
  rsim <- hyp.elects(judgeit.object,year,delvbar,just.mean.sd=T)
  vsim <- rsim$means
#  print(dim(vsim))
  sds <- rsim$sds
#  print(length(sds))
  o1 <- o2 <- o3 <- o4 <- o5 <- NA

  #New approach: rather than simulate elections, just simulate posterior.
  
  qofi <- sapply(1:dim(vsim)[2],FUN=function(ii) {  #each simulation. 

    #if (ii %% 50 == 0) print(ii)
    tt <- crossadd(vsim[,ii],intvl-0.5)
    tt <- 1-pnorm(0.5,tt,sds)
    tt <-
      rbind(tt,array(1*(judgeit.object$extra.districts[,1]>0.5),c(length(judgeit.object$extra.districts[,1]),11) ) )
        
    sts <- apply(tt,2,weighted.mean,seats)  
    #midpoint bias
    o1 <- 2*sts[6]-1
          
    #spread bias
    o2 <- mean(sts[11:7]+sts[1:5]-1)
          
    #responsiveness
    o3 <- (sts[11]-sts[1])/(intvl[11]-intvl[1])

    mv <- weighted.mean(judgeit.object$mu,judgeit.object$weightvec)
    intvl2 <- mv+c(-0.01,0.01)    
    
    tt <- crossadd(vsim[,ii],intvl2-0.5)
    tt <- 1-pnorm(0.5,tt,sds)
    tt <- rbind(tt,array(1*(judgeit.object$extra.districts[,1]>0.5),
                         c(length(judgeit.object$extra.districts[,1]),2) ) )
        
    sts <- apply(tt,2,weighted.mean,seats)

    o4 <- (sts[2]-sts[1])/(intvl2[2]-intvl2[1])
    
    return(c(o1,o2,o3,o4))

  })

  vs <- apply(qofi,1,var)
  out <- cbind(apply(qofi,1,mean)[1:4],sqrt(vs))
  
  colnames(out) <- c("Mean","SD")
  rownames(out) <- c("Partisan Bias (0.5)","Partisan Bias (0.45-0.55)",
                    "Responsiveness (0.45-0.55)","Responsiveness (observed)")
  return(signif(out,4))
}

`voting.power` <-
function (judgeit.object,year,vot,tot) {
  #Voting power routine. Only compares groups - don't care about power in each district yet.
  if (is.null(vot)) stop ("No voting groups have been specified.")
  if (is.null(tot)) {
    writeLines ("No total population groups specified; continuing without.")
    tot <- vot
  }
  if (any(dim(vot)!=dim(tot))) stop ("Dimensions of voting group and total groups don't match.")

  single.result <- function (means,sds,voters) {
    group.pops <- apply(voters,1,sum)
    log.voting.power <- tiep(means,sds,group.pops)
    return(sapply(1:dim(voters)[2],FUN=function(ii)
                  weighted.mean(exp(log.voting.power),voters[,ii])))
  }
  vs0 <- hyp.elects(judgeit.object,year,c(NA,NA),truncit=FALSE,just.mean.sd = TRUE)
  
  turnout <- apply(vot,1,sum)
  eligible.voters <- apply(tot,1,sum)
  if (any(tot-vot<0)) stop ("The number of eligible voters in one group is smaller than the observed number.")

  raw.total <- apply(vs0$means,2,single.result,vs0$sds,vot)
  output1 <- rbind(apply(raw.total,1,mean),apply(raw.total,1,quantile,c(0.025,0.975)))

  if (tot!=vot) {
    raw.total <- apply(vs0$means,2,single.result,vs0$sds,tot-vot)
    output1 <- rbind(output1,apply(raw.total,1,mean),apply(raw.total,1,quantile,c(0.025,0.975)))
    raw.total <- apply(vs0$means,2,single.result,vs0$sds,tot)
    output1 <- rbind(output1,apply(raw.total,1,mean),apply(raw.total,1,quantile,c(0.025,0.975)))
    colnames(output1) <- c("Mean voter Power","2.5%","97.5%","Non-Voter Power","2.5%","97.5%","Member power","2.5%","97.5%")
  } else colnames(output1) <- c("Mean voter Power","2.5%","97.5%")
  
  return(output1)
}

`winprob` <-
function (judgeit.object,year,lohi,Ev=NULL,delta=NULL) {
  #what's the story here? Wish I knew. 3-16-08
  if (is.null(lohi)) lohi <- c(0,1)
  if (lohi[1]<=0) lohi[1] <- -1e6; if (lohi[2]>=1) lohi[2] <- 1e6
  vsim <- hyp.elects(judgeit.object,year,c(NA,NA),FALSE)
  if (is.null(Ev)) delta.v <- delta else 
    if (is.null(delta)) delta.v <- Ev-judgeit.object$meanvote else
      stop("Trouble in winprob: Neither mean vote or delta specified.")
  out <- sapply(delta.v,FUN=function(ii) {
    quant <- inrange(apply(trunc01(rb(vsim+ii,judgeit.object$extra.districts[,1])),
         2,weighted.mean,c(judgeit.object$seatvec,judgeit.object$extra.districts[,3])),
                        lohi[1],lohi[2])
    c(mean(quant),sd(quant)/sqrt(judgeit.object$sims))
  })
  out <- t(out)
  colnames(out) <- c("Mean","SD of Mean")
  return(out)
}

`winvote` <-
function (judgeit.object,year,pr) {
  work <- function(in.mean,in.sd,weights) {
    lo <- 0; hi <- 1; probwin <- 9999; res <- 0.5; ii <- 0; bogus <- F
    patho <- NULL
    if ((pr>=0)&(pr<=1)) while (abs(probwin-pr)>0.0001) {
      res <- mean(c(lo,hi))
      vsim <- in.mean-0.5+res
      big.mean <- weighted.mean(vsim,weights)
      big.sd <- sqrt(weighted.mean(in.sd^2,weights))
      probwin <- 1-pnorm(0.5,big.mean,big.sd)
      if (pr>probwin) lo <- mean(c(lo,res)) else hi <- mean(c(hi,res))
#      writeLines (paste(signif(probwin,4),signif(lo,4),signif(hi,4),signif(big.mean),signif(big.sd)))
      patho <- rbind(patho,c(pr,probwin,lo,hi))
      ii <- ii+1
      if (ii == 100) {
        writeLines ("winvote: Too many repetitions...")
        probwin <- pr
        bogus <- T
      }
      if (ii>10) if (all(patho[ii,2]==patho[(ii-9):(ii-1),2])) {
        writeLines(paste(ii,"Trouble in winvote:",signif(probwin,4),"stuck for",pr,":",patho[1:3,2]))
        res <- -probwin
        probwin <- pr
      }
    } else stop ("In winvote, p(win) isn't between 0 and 1.")
    if (!bogus) return(res) else {
      writeLines(paste("Winvote attempt",ii,"failed to converge."))
      return(NA)
    }
  }

  if (length(pr)>1) {
    warning("Winvote routine takes one probability value only. Using first value in vector.")
    pr <- pr[1]
  }
  if (length(pr)==0) {
    warning("Winvote routine did not receive a p(win). Setting to 0.5.")
    pr <- 0.5
  }
  
  vs0 <- hyp.elects(judgeit.object,year,c(NA,0.5),truncit=F,just.mean.sd = TRUE)
  outcome <- apply(vs0$means,2,work,vs0$sds,judgeit.object$weightvec)

  if (any(is.na(outcome))) stop("Problem with the winvote routine.") else {
    out <- array(c(mean(outcome),quantile(outcome,c(0.025,0.975))),c(1,3))
    colnames(out) <- c("Vote Share","2.5%","97.5%")
    rownames(out) <- paste("P(win) =",pr)
  }

  return(out)
}

`chopcollege` <-
function(judgeit.object,year,state.ind,disaggregate=NULL) {

  state.ind <- state.ind[judgeit.object$rowcodes]
  Ev <- seq(0.4,0.6,by=0.002)
  #disaggregation only when asked.
  state.max <- max(state.ind)
  if (is.null(disaggregate)) disaggregate <- rep(FALSE,state.max) else
    if (length(disaggregate)<state.max) {
      disag <- rep(FALSE,state.max); disag[disaggregate] <- TRUE
      disaggregate <- disag
    } else
      disaggregate <- as.logical(disaggregate)

  vsim0 <- hyp.elects(judgeit.object,year,truncit=FALSE,just.mean.sd=TRUE)
  vsim <- vsim0$means
  st.dev <- vsim0$sds
  meanvote <- mean(vsim)
  delta <- Ev-meanvote
  
  #mean matrix, sd vector, weight vector
  elec.mean <- array(0,c(state.max,dim(vsim)[2]))
  elec.sd <- elec.seats <- rep(0,length(state.max))

  #hardcode DC.
  extra.mean <- rbind(rep(0.8,dim(vsim)[2]))
  extra.sd <- 0.001
  extra.seats <- 3


  
  #senatorial electors, plus aggregation.
  for (ii in 1:state.max) {
    sub <- which(state.ind==ii)
    if (length(sub)>0) {
      elec.mean[ii,] <- apply(rbind(vsim[sub,]),2,weighted.mean,judgeit.object$weightvec[sub])
      elec.sd[ii] <- mean(st.dev[sub]^2)/sqrt(length(sub))
      elec.seats[ii] <- 2

      if (disaggregate[ii]) {
        extra.mean <- rbind(extra.mean,vsim[sub,])
        extra.sd <- c(extra.sd,st.dev[sub])
        extra.seats <- c(extra.seats,rep(1,length(sub)))
      } else
        elec.seats[ii] <- elec.seats[ii]+length(sub)
    }
  }

  elec.mean <- rbind(elec.mean,extra.mean)
  elec.sd <- c(elec.sd,extra.sd)
  elec.seats <- c(elec.seats,extra.seats)

  jic <- which(elec.seats>0)
  elec.mean <- elec.mean[jic,]
  elec.sd <- elec.sd[jic]
  elec.seats <- elec.seats[jic]

  outty <- sapply(1:length(Ev),FUN=function(ii) {
    elecs <- elec.mean+delta[ii]
    ex.win <- 1-pnorm(0.5,elecs,elec.sd)
    res <- apply(ex.win,2,weighted.mean,elec.seats)
    
    return(c(mean(res),quantile(res,c(0.025,0.975)),mean(res>=0.5)))
  })
  
  outty <- t(outty)
  rownames(outty) <- Ev
  colnames(outty) <- c("Mean Seats","2.5%","97.5%","P(Party 1 wins)")

  return(list(meanvote,outty))
  
}

