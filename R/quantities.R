`dists` <-
function (judgeit.object) {  #this method works for all of the above
  mu <- judgeit.object$mu
  std <- judgeit.object$std
  pwin <- judgeit.object$pwin
  
  lnvprb <- tiep(mu,std,judgeit.object$turnoutvec) #should be turnout.
  out <- cbind(mu,std,pwin)

  item <- "P(vote>0.5)"
  if (!judgeit.object$predict) {
    out <- cbind(judgeit.object$votevec,out)
    item2 <- c("Observed Vote","Expected Vote")
  } else item2 <- "Predicted Vote"
  colnames (out) <- c(item2,"Std. Dev.",item)
  cn <- colnames(out)
  out <- cbind (out,exp(lnvprb),lnvprb)
  colnames (out) <- c(cn,"P(decisive)","ln(P(decisive))")

  out.object <- list(output=out,
                     year=judgeit.object$year,
                     cal.year=judgeit.object$years[judgeit.object$year])
  class(out.object) <- "judgeit.district.report"
  
  return(out.object)
}

`freq` <-
function (judgeit.object) { #,condition=NULL) { 
#  if (is.null(condition)) condition <- 1:length(judgeit.object$mu)
  h.elec <- hyp.elects(judgeit.object,results=TRUE)
  vprop <- judgeit.object$vote.range
  
  sim <- h.elec$votes
  seats <- h.elec$seats
    
  sim2 <- apply(inrange(sim,vprop[1],vprop[2]),2,weighted.mean,seats)
  out <- rbind(c(mean(sim2),quantile(sim2,c(0.025,0.5,0.975))))
  colnames(out) <- c("Mean Seat Share","2.5%","50%","97.5%")
  rownames(out) <- paste(signif(vprop[1],3),"-",signif(vprop[2],3),sep="")
  out.object <- list(output=out,
                     year=judgeit.object$year,
                     vote.range=vprop,
                     cal.year=judgeit.object$years[judgeit.object$year])
  class(out.object) <- "judgeit.conditional.seats"
  
  return(out.object)
}


`hyp.elects` <- function(judgeit.object,zero.mean=FALSE,results=FALSE) {
  #results is obsolete.
  
  if (is.null(judgeit.object$extra.districts)) judgeit.object$extra.districts <- array(0,c(0,4))
  
  j.o <- judgeit.object
  year <- j.o$year

  bets <- t(mvrnorm.cribbed(j.o$sims,j.o$beta[[year]],j.o$vc[[year]]))
  means <- j.o$mult%*%bets+as.vector(j.o$baseline) + #beta generation.
    array(rnorm(j.o$sims*dim(j.o$mult)[1],0,j.o$gamma.std),c(dim(j.o$mult)[1],j.o$sims)) #gamma.
 
  vsim <- rb(means,j.o$extra.districts[,1])
  sds <- c(j.o$stoned,j.o$extra.districts[,2])
  seats <- c(j.o$seatvec,j.o$extra.districts[,3])
  weight.d <- c(j.o$weightvec,j.o$extra.districts[,4])
  mean.diff <- apply(vsim,2,weighted.mean,weight.d)
  if (zero.mean) vsim <- vsim - t(array(mean.diff,dim(t(vsim))))
  
  scatter <- array(rnorm(length(vsim),0,sds),dim(vsim))

  votes <- scatter+vsim
  if (zero.mean) {
    md <- apply(votes,2,weighted.mean,weight.d)
    votes <- votes-t(array(md,dim(t(votes))))
  }
  
  out.object <- list(means=vsim,sds=sds,seats=seats,weights=weight.d,
              mean.diff=mean.diff,votes=votes,scatter=scatter)

  return(out.object)
}


`pv` <-
function (judgeit.object) {
  #fraction of districts(seats) with a probability of winning in the specified probability range.
  delta <- judgeit.object$shift.in.votes
  mean.votes <- judgeit.object$mean.votes
  prob.range <- judgeit.object$prob.range
  
  h.elec <- hyp.elects(judgeit.object)   #seats, weights, mean.diff, votes
  means <- h.elec$means
  sds <- h.elec$sds
  mean.diff <- h.elec$mean.diff
  outtype <- NULL
  
  if (!is.null(delta)) {
    pv.del <- t(sapply(delta,FUN=function(ii) {
      means.t <- means+ii
      ir.probs <- inrange(1-pnorm(0.5,means.t,sds),prob.range[1],prob.range[2])
      res <- apply(ir.probs,2,weighted.mean,h.elec$seats)
      return(c(mean(res),quantile(res,c(0.025,0.975))))
    }))
    colnames(pv.del) <- c("Mean","2.5%","97.5%")
    rownames(pv.del) <- delta
    outtype <- "delta"
  } else {
    means <- means-t(array(mean.diff,dim(t(means)))) #corrected to zero mean.
    pv.del <- t(sapply(mean.votes,FUN=function(ii) {
      means.t <- means+ii
      ir.probs <- inrange(1-pnorm(0.5,means.t,sds),prob.range[1],prob.range[2])
      res <- apply(ir.probs,2,weighted.mean,h.elec$seats)
      return(c(mean(res),quantile(res,c(0.025,0.975))))
    }))
    colnames(pv.del) <- c("Mean","2.5%","97.5%")
    rownames(pv.del) <- mean.votes
    outtype <- "mean.votes"
  }
  
  out.object <- list(output=pv.del,outtype=outtype,
                     meanvote=judgeit.object$meanvote,
                     cal.year=judgeit.object$years[judgeit.object$year],
                     prob.range=prob.range,
                     mean.votes=mean.votes,
                     year=judgeit.object$year)
  
  class(out.object) <- "judgeit.prob"
  return(out.object)
}

`winprob.raw` <-
function (judgeit.object) {
  #q of i: probability that party 1 gets seat share in the seat range.

  delta <- judgeit.object$shift.in.votes
  mean.votes <- judgeit.object$mean.votes
  seat.range <- judgeit.object$seat.range

  h.elec <- hyp.elects(judgeit.object)

  means <- h.elec$means
  sds <- h.elec$sds
  scatter <- h.elec$scatter
  mean.diff <- h.elec$mean.diff
  outtype <- NULL

  outcome.one <- function(mean.vec) {
    spread <- apply(scatter+mean.vec,2,weighted.mean,h.elec$seats)
    return(mean(inrange(spread,seat.range[1],seat.range[2])))
  }
  
  if (!is.null(delta)) {
    pv.del <- t(sapply(delta,FUN=function(ii) {
      means.t <- means+ii
      ir.probs <- apply(means.t,2,outcome.one)
      return(c(mean(ir.probs),quantile(ir.probs,c(0.025,0.975))))
    }))
    colnames(pv.del) <- c("Mean","2.5%","97.5%")
    rownames(pv.del) <- delta
    outtype <- "delta"
  } else {
    means <- means-t(array(mean.diff,dim(t(means)))) #corrected to zero mean.
    pv.del <- t(sapply(mean.votes,FUN=function(ii) {
      means.t <- means+ii
      ir.probs <- apply(means.t,2,outcome.one)
      return(c(mean(ir.probs),quantile(ir.probs,c(0.025,0.975))))
    }))
    colnames(pv.del) <- c("Mean","2.5%","97.5%")
    rownames(pv.del) <- mean.votes
    outtype <- "mean.votes"
  }

  out.object <- list(output=pv.del,outtype="outtype",
                     meanvote=judgeit.object$meanvote,
                     seat.range=seat.range,
                     cal.year=judgeit.object$years[judgeit.object$year],
                     year=judgeit.object$year)
  class(out.object) <- "judgeit.winprob"

  return(out.object)
}


`sv` <-
function (judgeit.object) {
  
  mean.votes <- judgeit.object$mean.votes
  if (is.null(mean.votes)) stop ("No mean vote shares specified.")

  quants <- hyp.elects(judgeit.object,zero.mean=TRUE)
  big.res <- quants$votes
  sds <- quants$sds
  seats <- quants$seats
  weights <- quants$weights
  
  outty <- sapply(1:length(mean.votes),FUN=function(ii) {
    vmn <- big.res+mean.votes[ii]
    tt <- 1*(vmn>0.5)     
    res <- apply(tt,2,weighted.mean,weights)
    return(c(mean(res),quantile(res,c(0.025,0.975))))
  })
  
  out <- as.data.frame(t(outty))
  rownames(out) <- mean.votes
  colnames(out) <- c("Seats Mean","2.5%","97.5%")
  out.object <- list(output=out,
                     mean.votes=mean.votes,
                     obsvotes=judgeit.object$obsvotes,
                     obsseats=judgeit.object$obsseats,
                     cal.year=judgeit.object$years[judgeit.object$year])
  class(out.object) <- "judgeit.seats"
  return(out.object)
}

`svsum.raw` <-
function (judgeit.object) {
  #quantities desired in each district: bias and responsiveness for range, center
  intvl <- 45:55/100

  rsim <- hyp.elects(judgeit.object,zero.mean=TRUE)
  vsim <- rsim$means
  sds <- rsim$sds
  seats <- rsim$seats
  weights <- rsim$weights
  
  svquants <- function (h.mn,h.sd,h.st,h.wt,intvl,meanvote) {
    intvl <- c(intvl,meanvote+c(-0.01,0.01))
    tt <- crossadd(h.mn,intvl)
    tt <- 1-pnorm(0.5,tt,h.sd)
    sts <- apply(tt,2,weighted.mean,seats)  

    #midpoint bias
    o1 <- 2*sts[6]-1
    #spread bias
    o2 <- mean(sts[11:7]+sts[1:5]-1)
    #responsiveness
    o3 <- (sts[11]-sts[1])/(intvl[11]-intvl[1])
    o4 <- (sts[13]-sts[12])/(intvl[13]-intvl[12])
    
    return(c(o1,o2,o3,o4,sts[1:11]))
  }
  
  qofi <- apply(vsim,2,svquants,sds,seats,weights,
                intvl,judgeit.object$meanvote)
  
  out <- cbind(apply(qofi,1,mean),
               apply(qofi,1,sd),
               t(apply(qofi,1,quantile,c(0.025,0.5,0.975))))
  
  colnames(out) <- c("Mean","SD","2.5%","50%","97.5%")
  rownames(out) <- c("Partisan Bias (0.5)","Partisan Bias (0.45-0.55)",
                    "Responsiveness (0.45-0.55)",
                     "Responsiveness (observed)",intvl)
  
  out.object <- list(svsums=out[1:4,],svplot=out[5:15,],
                     obsvotes=judgeit.object$obsvotes,
                     obsseats=judgeit.object$obsseats,
                     cal.year=judgeit.object$years[judgeit.object$year])
  class(out.object) <- "judgeit.svsum"
  
  return(out.object)
}

`voting.power.raw` <-
function (judgeit.object) {
  #Voting power routine. Only compares groups - don't care about power in each district yet.

  year <- judgeit.object$year
  vot <- judgeit.object$pop.mat
  group.pops <- judgeit.object$eligible.vec
    
  #get rid of all missing columns.
  missing.columns <- which(sapply(1:dim(vot)[2],FUN=function(cc) all(is.na(vot[,cc]))))
  if (length(missing.columns)>0) vot <- vot[,-missing.columns]
  
  vs0 <- hyp.elects(judgeit.object)

  ok.rows <- missed(vot)
  vot <- cbind(vot[ok.rows,])
  means.all <- vs0$means[ok.rows,]
  sds <- vs0$sds[ok.rows]
  group.pops <- group.pops[ok.rows]
  
  single.result <- function (means) {
    log.voting.power <- tiep(means,sds,group.pops)
    return(sapply(1:dim(vot)[2],FUN=function(ii)
                  weighted.mean(exp(log.voting.power),vot[,ii])))
  }
 
  raw.total <- rbind(apply(means.all,2,single.result))
  output1 <- rbind(apply(raw.total,1,mean),apply(raw.total,1,quantile,c(0.025,0.975)))

  rownames(output1) <- c("Mean voter Power","2.5%","97.5%")
  colnames(output1) <- colnames(vot)

  out.object <- list(output=output1,
                     cal.year=judgeit.object$years[judgeit.object$year])
  class(out.object) <- "judgeit.voting.power"
  return(out.object)
}

`winvote.raw` <-
function (judgeit.object) {
  p.win <- judgeit.object$prob.win
  vs0 <- hyp.elects(judgeit.object)

  e.sd <- vs0$sds
  weights <- vs0$weights
  seats <- vs0$seats
  scatter <- vs0$scatter

  winvote.one.improved <- function(e.means) {

    single.result.winning.vote <- function(res) {
                                        #row 1: outcomes
                                        #row 2: seats
      
      res.obj <- rbind(res,seats)
      res.obj <- res.obj[,order(res.obj[1,])]
      cs.seats <- cumsum(res.obj[2,])
      item.of.interest <- min(which(cs.seats>cs.seats[length(cs.seats)]/2))
      retty <- res.obj[1,item.of.interest]
      
      return(0.5-retty)
    }
    
    blueprint <- scatter+e.means
    mean.adjust <- t(array(apply(blueprint,2,weighted.mean,weights),
                           dim(t(blueprint))))
    blueprint <- blueprint-mean.adjust
    
    winning.votes <- apply(blueprint,2,single.result.winning.vote)
    ret <- quantile(winning.votes,p.win)  
    return(ret)
    
  }
  
  outcome <- rbind(apply(vs0$means,2,winvote.one.improved))
  
  if (any(is.na(outcome))) stop("Missing values produced by the winvote routine.") else {
    out <- t(rbind(apply(outcome,1,mean),apply(outcome,1,quantile,c(0.025,0.5,0.975))))
    colnames(out) <- c("Mean Vote Share","2.5%","50%","97.5%")
    rownames(out) <- paste("P(win) =",p.win)
  }
  out.object <- list(output=out,prob.win=p.win,
                     year=judgeit.object$year,
                     cal.year=judgeit.object$years[judgeit.object$year])
  class(out.object) <- "judgeit.winvote"

  return(out.object)
}
