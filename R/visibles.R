`judgeit.core` <-
function (routine,judgeit.object,year,#predict=F,#covarsnew,
                    expected.value.only,distselect,
                    vote.range,mean.votes,
                    shift.in.votes,winvote,
                    probability.range,voting.groups,all.groups,
                    state.group,disaggregate) {
  verbose <- FALSE
  if (verbose) writeLines ("in the core.")
  
  if (is.null(distselect)) distselect <- 1:length(judgeit.object$voteshare[[year]]) else
    if (length(distselect)>length(judgeit.object$voteshare[[year]])) stop("Your district selection vector doesn't have the right number of districts.")
  
  judgeit.object$svexpected.value.only <- expected.value.only

  output <- "Execution failure." #should anything go wrong, this is the function's output.

  #Recalculate weights, just in case.
  
  weighttype <- c("constant","turnout","seats","eligible.voters")
  judgeit.object$distweights[[year]] <- switch(judgeit.object$weight,
               "constant" = array(1,c(dim(judgeit.object$covarsnew[[year]])[1],1)),
               "turnout" = judgeit.object$turnout[[year]],
               "eligible.voters" = judgeit.object$eligible.voters[[year]],
               "seats" = judgeit.object$seats[[year]]
             )

  #get mean, error, seats in advance.
  #for now, only omit unobserved covariate rows.
  #addition, 7-16-07. - fix ugly defaults in plot.
  if (verbose) writeLines ("in the core 2.")

  if (!judgeit.object$predict) {
    lamb <- judgeit.object$lam
    qun <- judgeit.object$covarsnew[[year]]-lamb*judgeit.object$covars[[year]]
    mu <- lamb*judgeit.object$voteshare[[year]] + qun%*%judgeit.object$beta[[year]]
    std <- sqrt((1-lamb^2)*judgeit.object$distweights[[year]]*judgeit.object$sig^2*
      (!judgeit.object$svexpected.value.only) + cbind(diag(qun%*%judgeit.object$vc[[year]]%*%t(qun))))
    base <- c(lamb*judgeit.object$voteshare[[year]])
    stoned <- sqrt((1-lamb^2)*judgeit.object$distweights[[year]]*judgeit.object$sig^2*
                   (!judgeit.object$svexpected.value.only))
  } else {
    qun <- judgeit.object$covarsnew[[year]]
    mu <- qun%*%judgeit.object$beta[[year]]
    std <- sqrt(judgeit.object$distweights[[year]]*judgeit.object$sig^2*
           (!judgeit.object$svexpected.value.only)+cbind(diag(qun%*%judgeit.object$vc[[year]]%*%t(qun))))
    base <- c(0*mu)
    stoned <- sqrt(judgeit.object$distweights[[year]]*judgeit.object$sig^2*(!judgeit.object$svexpected.value.only))
  }
  pwin <- 1-pnorm(0.5,mu,std)
  tbo <- cbind(mu,std,pwin,judgeit.object$seats[[year]])
  inn <- intersect(missed(tbo),distselect)

  if (verbose) writeLines ("in the core 3.")

  if (!is.null(voting.groups)) {
    if (length(mu)!=dim(voting.groups)[1]) stop ("Error: voting group matrix has an incorrect number of rows.")
    voting.groups <- voting.groups[inn,]
  }
  if (!is.null(all.groups)) {
    if (length(mu)!=dim(all.groups)[1]) stop ("Error: total group matrix has an incorrect number of rows.")
    all.groups <- all.groups[inn,]
  }

  mu <- mu[inn]
  std <- std[inn]
  pwin <- pwin[inn]

  judgeit.object$baseline <- cbind(base[inn])
  judgeit.object$mult <- qun[inn,]
  judgeit.object$stoned <- stoned[inn]

  judgeit.object$mu <- mu
  judgeit.object$std <- std
  judgeit.object$pwin <- pwin
  judgeit.object$seatvec <- cbind(judgeit.object$seats[[year]][inn])
  judgeit.object$weightvec <- cbind(judgeit.object$distweights[[year]][inn])
  judgeit.object$turnoutvec <- cbind(judgeit.object$turnout[[year]][inn])  
  judgeit.object$votevec <- cbind(judgeit.object$voteshare[[year]][inn])
  judgeit.object$rows <- rownames(judgeit.object$covars[[year]])[inn]
  judgeit.object$rowcodes <- inn
  meanvote <- judgeit.object$meanvote <- weighted.mean(mu,judgeit.object$weightvec)

  if (is.null(vote.range)) vote.range <- c(0.4,0.6)
  if (is.null(mean.votes)) mean.votes <- seq(meanvote-0.15,meanvote+0.15,length.out=31)

  #writeLines("Test core")
    if (verbose) writeLines ("in the core 4.")

  output <- switch (routine,
          "seats" = sv(judgeit.object,year,mean.votes),
          "prob" = {
            if (is.null(mean.votes)&is.null(shift.in.votes)) {
              warning ("Neither mean votes nor shifts in the mean vote were specified. Setting the observed votes to a range about the observed outcome.")
              mean.votes <- seq(meanvote-0.05,meanvote+0.05,by=0.01)
            }
            if (!is.null(mean.votes)) pv(judgeit.object,year,Ev=mean.votes,lohi=probability.range) else pv(judgeit.object,year,delta=shift.in.votes,lohi=probability.range)}, 
          "conditional.seats" = freq (judgeit.object,year,vote.range),
          "svsum" = svsum(judgeit.object,year),
          "distreport" = dists(judgeit.object,year),

          "voting.power" = voting.power(judgeit.object,year,voting.groups,all.groups),

            "winvote" = winvote(judgeit.object,year,winvote),
          "winprob" = {
            if (is.null(mean.votes)&is.null(shift.in.votes)) {
              writeLines ("Neither mean votes nor shifts in the mean vote were specified. Setting the observed votes to a range about the observed outcome.")
              mean.votes <- seq(meanvote-0.05,meanvote+0.05,by=0.01)
            }
            if (!is.null(mean.votes)) winprob(judgeit.object,year,probability.range,Ev=mean.votes) else 
              winprob(judgeit.object,year,probability.range,delta=shift.in.votes)},
          "chopcollege" = chopcollege(judgeit.object,year,state.ind=state.group,disaggregate=disaggregate),
            
          writeLines ("An invalid routine was entered.")
          )
  
 return(output)
}

`judgeit` <-
function (model.formula=~1,vote.formula=NULL,same.districts=NULL,data,
                    uncontesteds.method="default",uncontested.low=0.05,
                    uncontested.low.new=0.25,uncontested.high=0.95,
                    uncontested.high.new=0.75,
                    use.last.votes=T,
                    simulations=1000, 

                    weight="constant", 
                    
                    routine=NULL,year=NULL,judgeit.object=NULL,
                    predict=FALSE,
                    new.covariates=NULL,  
                    new.covariate.matrix=NULL,  
                    new.seats=NULL,
                    new.actual.voters=NULL,
                    new.eligible.voters=NULL,
          
                    extra.districts=NULL, 
                    expected.value.only=F,
                    distselect=NULL,
                    vote.range=NULL, 

                    mean.votes=NULL,
                    shift.in.votes=NULL,
                    winvote=0.5,
          
                    probability.range=c(0.5,1),
                    voting.groups=NULL,
                    all.groups=NULL,
                    state.group=NULL,
                    disaggregate=NULL,
          
                    ...    #for the model.frame work.
                   )
  { #judgeit
    
  verbose <- FALSE
  if (is.null(judgeit.object)) { #data loading stage.
    yy <- xx <- wts <- wtv <- wtt <- NULL
    a.n <- function(...) as.numeric(...)
    if (!is.data.frame(data)) { #more than one year contained in frame? If so...
    
     years <- names(data)
     for (ii in 1:length(data)) {
      framer <- model.frame(formula=model.formula,data=data[[ii]],na.action=na.pass,...)
      yy[[ii]] <- a.n(model.response(framer))
      if (length(yy[[ii]])==0) stop (paste("Year",ii,"has no vote results. Check your formula?"))
      xx[[ii]] <- model.preds(framer)
      if (!is.null(vote.formula)) {
        fr2 <- model.frame(formula=vote.formula,data=data[[ii]],na.action=na.pass,...)
        if (length(fr2)>0) vt <- as.matrix(model.response(fr2)) else
          vt <- matrix(rep(1,length(yy[[ii]])))
        if (!is.null(vt)) {  #at least one column
        if (dim(vt)[2]>1) {wtv[[ii]] <- a.n(vt[,1]); wtt[[ii]] <- a.n(vt[,2])} else
          wtv[[ii]] <- wtt[[ii]] <- a.n(vt[,1]) }
        st <- model.preds(fr2); wts[[ii]] <- a.n(st[,1])
      }
    }
   
  } else { #only one year this time.
    framer <- model.frame(formula=model.formula,data=data,na.action=na.pass,...)
    years <- "Only year"
    yy[[1]] <- a.n(model.response(framer))
    xx[[1]] <- model.preds(framer)
    if (!is.null(vote.formula)) {
      fr2 <- model.frame(formula=vote.formula,data=data,na.action=na.pass,...)
      if (length(fr2)>0) vt <- as.matrix(model.response(fr2)) else
        vt <- matrix(rep(1,length(yy[[1]])))
      if (!is.null(vt)) {  #at least one column
        if (dim(vt)[2]>1) {wtv[[1]] <- a.n(vt[,1]); wtt[[1]] <- a.n(vt[,2])} else
          wtv[[1]] <- wtt[[1]] <- a.n(vt[,1]) }

      st <- model.preds(fr2); wts[[1]] <- a.n(st[,1])
    }
  }
  
  judgeit.object <- judgeit.setdata(xx,yy,wtv,wtt,wts,
                  same.districts=same.districts,uncontesteds.method=uncontesteds.method,simulations=simulations)
  judgeit.object$years <- years
   #this concludes the formulation of the judgeit.object.

  ###############################
  #prelim routine.

  weighttype <- c("constant","turnout","seats","eligible.voters")
  if (!any(weight==weighttype)) stop("Problem in preliminary analysis: unknown choice for district weights. Please select 'constant', 'turnout', 'eligible.voters' or 'seats'.") else 
    distweights <- switch(weight,
               "constant" = {
                 tw <- new.list(length(judgeit.object$covars));
                 for (ii in 1:length(judgeit.object$covars)) tw[[ii]] <- array(1,c(dim(judgeit.object$covars[[ii]])[1],1));
                 tw},
               "turnout" = judgeit.object$turnout,
               "eligible.voters" = judgeit.object$eligible.voters,
               "seats" = judgeit.object$seats
             ) 

  judgeit.object$distweights <- distweights
  judgeit.object$weight <- weight
  nyears <- length(judgeit.object$covars)
  redist <- judgeit.object$sam
  judgeit.object$beta <- judgeit.object$vc <- list(NA)
  judgeit.object$sind <- judgeit.object$lind <- NULL
#  judgeit.object$resid <- list(NA)
  
  for (ii in 1:nyears) { #sig/lam estimation.
    ury <- judgeit.object$fullrow[[ii]]
    zt <- reg(cbind(judgeit.object$covars[[ii]][ury,]),
              cbind(judgeit.object$voteshare[[ii]][ury]),
              judgeit.object$distweights[[ii]][ury])
    lambda <- NA
    l2 <- NA

    if (ii<nyears) if (redist[ii+1]) {
      ur <- intersect(judgeit.object$fullrow[[ii]],judgeit.object$fullrow[[ii+1]])
      zt2 <- reg(u.c(judgeit.object$voteshare[[ii]],judgeit.object$covars[[ii]],judgeit.object$covars[[ii+1]])[ur,],
                 matrix(judgeit.object$voteshare[[ii+1]][ur]),
                 judgeit.object$distweights[[ii]][ur])
      lambda <- as.numeric(zt2$beta[1])
    }

    judgeit.object$beta[[ii]] <- zt$beta
    judgeit.object$vc[[ii]] <- zt$vc
    judgeit.object$sind[ii] <- sqrt(zt$sig2)
    judgeit.object$lind[ii] <- lambda

  }

  good.sigma <- judgeit.object$sind[!is.na(judgeit.object$sind)]
  judgeit.object$sigma <- sqrt(sum(good.sigma^2)/length(good.sigma))
  judgeit.object$lambda <- mean(judgeit.object$lind[!is.na(judgeit.object$lind)])

  }  #first step done.

  if (is.matrix(extra.districts)) extra.districts <- as.data.frame(extra.districts)
  if (!is.null(extra.districts)) {
    if (!is.data.frame(extra.districts)) {
      writeLines ("Error in extra districts: not a data frame. Will be ignored.")
      extra.districts <- NULL
    } else
    if (dim(extra.districts)[2]!=3) {
      writeLines ("Error in extra districts: the number of columns is not equal to 3 (vote share,voters,seats). Will be ignored.")
      extra.districts <- NULL
    } else
      extra.districts2 <- switch (judgeit.object$weight,
                                "constant" = rep(1,dim(extra.districts)[1]),
                                "eligible.voters" = extra.districts[,2],
                                "turnout" = extra.districts[,2],
                                "seats" = extra.districts[,3])
  } else extra.districts2 <- NULL
  judgeit.object$extra.districts <- cbind(extra.districts,extra.districts2)
  #OK, fourth column is now the weight to be applied.
  
  if (!is.null(routine)) {
    judgeit.object$predict <- predict
    if (is.na(judgeit.object$lambda)) {
      judgeit.object$lambda <- 0.5 #what to do if no information? what would Bayes do?
      writeLines ("Lambda is undefined, and is therefore estimated as 0.5.")
    }  
      
    if (is.null(year)) {
      writeLines (paste("No year was given: the most recent, year ",length(judgeit.object$voteshare),", will be used.",sep=""))
      year <- length(judgeit.object$voteshare)
    }

    if ((year>length(judgeit.object$voteshare))|(year<=0)) stop("The year you have selected is not in the system.")
    
    if (!is.null(new.covariates)) {  #implement modificatins as requested
      if (!is.list(new.covariates)) stop ("Counterfactual predictor task list must be of class 'list'.")
      if (as.logical(length(new.covariates)%%2)) stop ("Counterfactual predictor task terms must come in pairs.")
      covarsnew <- judgeit.object$covars[[year]]
      for (ii in 1:floor(length(new.covariates)/2))
        covarsnew[,new.covariates[[2*ii-1]] ] <- new.covariates[[2*ii]]
        
    } else covarsnew <- new.covariate.matrix

    if (verbose) writeLines("In the main 1.")

    if (is.null(judgeit.object$covarsnew)) judgeit.object$covarsnew <- new.list(length(judgeit.object$covars))
    if (is.null(covarsnew)) judgeit.object$covarsnew[[year]] <- judgeit.object$covars[[year]] else
      judgeit.object$covarsnew[[year]] <- covarsnew

    if (dim(judgeit.object$covars[[year]])[2]!=dim(judgeit.object$covarsnew[[year]])[2]) stop("There is a different number of covariates in the new group compared to the old.")


    #in case of counterfactual, these could change.
    judgeit.object$seathold <- judgeit.object$seats[[year]]
    judgeit.object$actvotehold <- judgeit.object$turnout[[year]]
    judgeit.object$elgvotehold <- judgeit.object$eligible.voters[[year]]

    if (!is.null(new.seats)) {
      judgeit.object$seats[[year]] <- new.seats
      judgeit.object$predict <- T
    }
    if (!is.null(new.eligible.voters)) {
      judgeit.object$eligible.voters[[year]] <- new.eligible.voters
      judgeit.object$predict <- T
    }
    if (!is.null(new.actual.voters)) {
      judgeit.object$turnout[[year]] <- new.actual.voters
      judgeit.object$predict <- T
    }
    if (verbose) writeLines("In the main 2.")

#    writeLines("test main")
    
    if ((any(dim(judgeit.object$covars[[year]])!=dim(judgeit.object$covarsnew[[year]])))&(!judgeit.object$predict)) {
      writeLines (paste("Warning: in year ",year,", old and new covariates have a different number of observations. Proceeding under prediction mode, with one seat per district, and equal population.",sep=""))
      judgeit.object$predict <- T
      judgeit.object$seats[[year]] <- cbind(rep(1,dim(judgeit.object$covarsnew[[year]])[1]))   #all set to 1.
      judgeit.object$eligible.voters[[year]] <- judgeit.object$turnout[[year]] <- cbind(rep(404,dim(judgeit.object$covarsnew[[year]])[1]))   #all set to 404.
    }

    with (judgeit.object, if (!all(c(length(seats[[year]])==dim(covarsnew[[year]])[1],length(turnout[[year]])==dim(covarsnew[[year]])[1],length(eligible.voters[[year]])==dim(covarsnew[[year]])[1]))) stop (paste("Error in evaluation stage: the number of covariate, seat, eligible and actual voter rows do not match each other.")))
 
   
    #this attaches the output from judgeit.core and returns the original object. This means that whatever we add to the object within judgeit.core is temporary.
    try({judgeit.object$output <- judgeit.core(routine=routine,judgeit.object=judgeit.object,
                 year=year,
                 expected.value.only=expected.value.only,
                 distselect=distselect, #subset?
                                               
                 vote.range=vote.range,mean.votes=mean.votes, #options.
                 shift.in.votes=shift.in.votes, winvote=winvote,
                 voting.groups=voting.groups,all.groups=all.groups,probability.range=probability.range,
                                               state.group=state.group,disaggregate=disaggregate)
       judgeit.object$outputclass <- routine
       judgeit.object$outputyear <- year}) #formula based?

    judgeit.object$seats[[year]] <- judgeit.object$seathold
    judgeit.object$turnout[[year]] <- judgeit.object$actvotehold
    judgeit.object$eligible.voters[[year]] <- judgeit.object$elgvotehold

  }

  return(judgeit.object)
}

`judgeit.setdata` <-
function (covars=NULL,voteshare=NULL,turnout=NULL,eligible.voters=NULL,
          seats=NULL,same.districts=NULL,use.last.votes=T,
          uncontesteds.method="impute",uncontested.low=0.05,
          uncontested.low.new=0.25,uncontested.high=0.95,
          uncontested.high.new=0.75,simulations=1000,weight="constant") {

  flag <- F

  out <- list(covars=covars,voteshare=voteshare,turnout=turnout)

  if (is.null(same.districts)) {
    quant <- 0
    if (length(voteshare)>1) for (ii in 2:length(voteshare)) quant <- c(quant,1*(length(voteshare[[ii]])==length(voteshare[[ii-1]])))
    same.districts <- quant
  }
  
  if (is.null(turnout)) {
    writeLines ("Number of actual voters unknown, so let's say 404 per district.")
    turnout <- new.list(length(covars))
    for (ii in 1:length(covars)) turnout[[ii]] <- rep(404,dim(covars[[ii]])[1])
    out$turnout <- turnout
  }

  if (is.null(eligible.voters)) {
    writeLines ("The total number of eligible voters was not given. Eligible voters set to actual voters.")
    out$eligible.voters <- turnout
    eligible.voters <- turnout
  } else out$eligible.voters <- eligible.voters

  
  for (ii in 1:length(covars)) {
    if (is.null(covars[[ii]])|all(is.na(covars[[ii]]))) {
      writeLines (paste("No valid covariates given for year ",ii,". Substituting constant row only.",sep=""))
      covars[[ii]] <- as.matrix(rep(1,length(voteshare[[ii]])))
    }
    nas <- NULL
    for (k in 1:dim(covars[[ii]])[2]) {
      if (all(is.na(covars[[ii]][,k]))) nas <- c(nas,k)
    }
    if (!is.null(nas)) covars[[ii]] <- as.matrix(covars[[ii]][,-nas])
    out$covars <- covars
  }

  if (is.null(voteshare)) stop ("Can't do an analysis without results.")

  for (ii in 1:length(voteshare)) {
    ct <- voteshare[[ii]]
    ct <- ct[!is.na(ct)]
    if (any(trunc01(ct)!=ct)) stop (paste("Some districts have vote proportions outside the [0,1] range, in particular year",ii,"."))
  }

  if (is.null(seats)) {
    writeLines ("The total number of seats per district was not given, and is therefore assumed to be 1 in all elections.")
    onus <- function (vs) 1*(vs>=0)
    seats <- lapply (voteshare,onus)
    out$seats <- seats
  } else out$seats <- seats

  possible.uncs <- c("impute","default","remove","nochange")
  if (!any(uncontesteds.method==possible.uncs)) {
    writeLines ("Your choice for removing uncontesteds does not match an existing option. Assuming no change.")
    uncontesteds.method <- "nochange"
  }
  
  #Now all five variables are in place. Put them in matrix form. Missing values stay for now.
  out$fullrow <- new.list(length(out$covars))
  for (ii in 1:length(out$voteshare)) {
    caddy <- cbind (out$voteshare[[ii]],out$covars[[ii]],out$eligible.voters[[ii]],out$turnout[[ii]],out$seats[[ii]])
    out$fullrow[[ii]] <- as.numeric(missed(caddy))

    out$voteshare[[ii]] <- as.matrix(out$voteshare[[ii]])
    out$covars[[ii]] <- as.matrix(out$covars[[ii]])
    out$eligible.voters[[ii]] <- as.matrix(out$eligible.voters[[ii]])
    out$turnout[[ii]] <- as.matrix(out$turnout[[ii]])
    out$seats[[ii]] <- as.matrix(out$seats[[ii]])
  }

  
  out$uncL <- uncontested.low    #maximum for Repub. uncontested
  out$uncLR <- uncontested.low.new   #new level of Rep. uncontested
  out$uncU <- uncontested.high    #min. for Dem uncontested
  out$uncUR <- uncontested.high.new   #new level of Dem uncontested

  out$svexpected.value.only <- F    #Do we want the expected value (1) or the total variation (0)?
  out$sims <- simulations  #default number of simulations to run within one block.
  out$simd <- simulations  #Number of blocks.
  
  out$covarsnew <- new.list(length(covars))
  out$same.dists <- same.districts
  out$outputclass <- "none"
  out$outputyear <- NULL
  
  #now, diagnose the state of the system.
  if ((!flag)&(length(covars)!=length(voteshare))) {
    stop("Error: Covariates and votes don't have the same number of years.")
    out <- NULL
  } else
  if ((length(voteshare)!=length(turnout))|(length(voteshare)!=length(seats))|(length(voteshare)!=length(eligible.voters))) {
    stop("Error: Votes and weights don't have the same number of years.")
    out <- NULL
  } else for (ii in 1:length(covars)) {
      if (dim(covars[[ii]])[1]!=length(voteshare[[ii]])) {
        stop(paste("Error: Covariates and votes in data set",ii,"do not have the same number of districts. ",dim(covars[[ii]])[1],length(voteshare[[ii]])))
        out <- NULL
      }  
      if ((length(voteshare[[ii]])!=length(turnout[[ii]]))|(length(voteshare[[ii]])!=length(seats[[ii]]))|(length(voteshare[[ii]])!=length(eligible.voters[[ii]]))) {
        stop(paste("Error: Votes and weights in data set",ii,"do not have the same number of districts."))
        out <- NULL
	}
    } 
 
#OK. If all is well, out isn't NULL. So now we look at imputation, replacement or whatever.
#options: impute, remove, default, nochange

  if (flag) {
    out <- NULL
    stop("There was an error loading the data.")
  } else {
    class(out) <- "judgeit"
    writeLines ("Election data has been loaded into specified JudgeIt object.")
  }

  if (is.null(out)) stop() else {
    out <- fix.uncontested (out,same.districts,uncontesteds.method)
    if ((use.last.votes)&(length(out$covars)>1)) for (ii in 2:length(out$covars)) 
      if (same.districts[[ii]]) {out$covars[[ii]] <- unique.columns(cbind(out$covars[[ii]],out$voteshare[[ii-1]])); colnames(out$covars[[ii]])[dim(out$covars[[ii]])[2]] <- "lastvote"}

    for (ii in 1:length(out$voteshare)) {
      caddy <- cbind (out$voteshare[[ii]],out$covars[[ii]],out$eligible.voters[[ii]],out$turnout[[ii]],out$seats[[ii]])
      out$fullrow[[ii]] <- as.numeric(missed(caddy))
      out$covars[[ii]] <- unique.columns(out$covars[[ii]],keepers=out$fullrow[[ii]])
    }
  }
  return(out)
}

"JudgeIt" <-
function (...) judgeit(...)

`kernel.plot` <-
function (y,y2=NULL,col=1,ty="l",new=T,grid=T,...) {
  dt <- density(y,from=0,to=1)
  if (!is.null(y2)) {dt2 <- density(y2,from=0,to=1); maxy <- max(c(dt$y),c(dt2$y))} else maxy <- max(dt$y)
  plot (c(0,1),c(0,maxy),ty="n",...)
  lines (dt$x,dt$y,ty=ty,col=col)
  if (!is.null(y2)) lines (dt2$x,dt2$y,col=col+1)
  if (grid) for (ii in 0:10) lines(rep(ii/10,2),c(0,maxy),col=8,lty=2)
}

`plot.judgeit` <-
function(x,straight.up=F,year=1,...) {
  judgeit.object <- x
  
  if (length(judgeit.object$outputyear)>0) yrfull <- judgeit.object$years[judgeit.object$outputyear] else
    yrfull <- year

  switch (judgeit.object$outputclass,
          "seats" = {
            plot(c(0,1),c(0,1),ty="n",main=paste("Seats-Votes Plot",yrfull),
                 xlab="Vote Proportion",ylab="Seat Proportion",...)
            year <- judgeit.object$outputyear
            t.x <- as.numeric(rownames(judgeit.object$output))
            t.y <- judgeit.object$output[,1]
            line.bottom <- judgeit.object$output[,2]
            line.top <- judgeit.object$output[,3]
            lines(t.x,t.y)
            lines(t.x,pmin(1,line.top),col=3)
            lines(t.x,pmax(line.bottom,0),col=3)
            lines(c(0.5,0.5),c(0,1),col=8)
            lines(c(0,1),c(0.5,0.5),col=8)
            xx <- weighted.mean(c(judgeit.object$voteshare[[year]],judgeit.object$ext[,1]),
                                c(judgeit.object$distweights[[year]],judgeit.object$ext[,4]),na.rm=T)
            yy <- weighted.mean(v2s(c(judgeit.object$voteshare[[year]],judgeit.object$ext[,3])),
                                    c(judgeit.object$seats[[year]],judgeit.object$ext[,4]),na.rm=T)
            points(xx,yy,col=4)
            text(xx,yy,paste("Vote:",signif(xx,4)),pos=4)
            text(xx,yy-0.05,paste("Seats:",signif(yy,4)),pos=4)
          },
          "chopcollege" = {
            plot(c(0,1),c(0,1),ty="n",main=paste("Electoral Coilege Plot",yrfull),
                 xlab="Vote Proportion",ylab="Elector Proportion",...)
            year <- judgeit.object$outputyear
            t.x <- as.numeric(rownames(judgeit.object$output[[2]]))
            t.y <- judgeit.object$output[[2]][,1]
            line.bottom <- judgeit.object$output[[2]][,2]
            line.top <- judgeit.object$output[[2]][,3]
            lines(t.x,t.y)
            lines(t.x,pmin(1,line.top),col=3)
            lines(t.x,pmax(line.bottom,0),col=3)
            lines(c(0.5,0.5),c(0,1),col=8)
            lines(c(0,1),c(0.5,0.5),col=8)
            lines(rep(judgeit.object$output[[1]],2),c(0,1),lty=2,col=4)
          },

          "prob" = {
            plot(c(0,1),c(0,1),ty="n",main=paste("Probability-Votes
Plot",yrfull),xlab="Vote Proportion",ylab="Seat Proportion with
Indicated Probability",...)
            t.x <- as.numeric(rownames(judgeit.object$output))
            t.y <- judgeit.object$output[,1]
            line.bottom <- judgeit.object$output[,2]
            line.top <- judgeit.object$output[,3]
            lines(t.x,t.y)
            lines(t.x,pmin(1,line.top),col=3)
            lines(t.x,pmax(line.bottom,0),col=3)
            lines(c(0.5,0.5),c(0,1),col=8)
            lines(c(0,1),c(0.5,0.5),col=8)
          },
          "distreport" = {
            if (colnames(judgeit.object$output)[2]!="Std. Dev.") {
              kernel.plot (judgeit.object$output[,1],judgeit.object$output[,2],
                           xlab="Vote Share",ylab="Likelihood of District Vote",main=paste("District Vote Likelihood Plot for",yrfull),...)
              text(0.9,1.3,"Observed Votes",col=1)
              text(0.9,1.4,"Model Prediction",col=2)
            } else {
              kernel.plot (judgeit.object$output[,1],
                           xlab="Vote Share",ylab="Likelihood of District Vote",main=paste("District Vote Likelihood Plot for",yrfull),...)
              text(0.9,1.4,"Model Prediction",col=1)
            }
          },
          {year <- judgeit.object$outputyear
           writeLines (paste("Outputting a kernel plot of districts in year ",yrfull,".",sep=""))
           yy <- judgeit.object$voteshare[[year]]; yy <- yy[!is.na(yy)]; kernel.plot(yy)})
}

`print.judgeit` <-
function(x,...) {
  if (is.null(x$lambda)) "Object has not undergone preliminary analysis." else
    {writeLines ("Value of object$output:"); print(x$output)}                  
}

`summary.judgeit` <-
function (object,year=NA,...) {
  qsum <- function (set) c(mean(set[!is.na(set)]),
                           sd(set[!is.na(set)]),
                           var(set[!is.na(set)]),
                           min(set[!is.na(set)]),
                           max(set[!is.na(set)]),
                           sum(1*!is.na(set)),
                           sum(1*is.na(set)))
  #x,y,wtv,eligible.voters,seats
  out <- NULL
  if (!is.na(year)) {
    out <- rbind(t(apply(object$covars[[year]],2,qsum)),qsum(object$voteshare[[year]]),
               qsum(object$turnout[[year]]),qsum(object$eligible.voters[[year]]),qsum(object$seats[[year]]))

    jj <- dim(out)[1]
    rownames(out)[(jj-3):jj] <- c("Vote Proportion",
                                "Turnout","Eligible Voters",
                                "Seats per District")
    colnames(out) <- c("Mean","SD","Variance","Min","Max","Valid","Missing")
  } else {
    writeLines(paste("This Judgeit object contains data for",length(object$covars),"elections."))
    writeLines (paste("Lambda = ",object$lambda,sep=""))
    writeLines (paste("Sigma = ",object$sigma,sep=""))
  }
  if (!is.null(out)) return(out)
}

