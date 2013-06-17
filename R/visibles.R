`judgeit.get.sigma.lambda` <- function(judgeit.object, weight) {

  weighttype <- c("constant","turnout","seats","eligible.voters")
  if (!any(weight==weighttype)) stop("Problem in preliminary analysis: unknown choice for district weights. Please select 'constant', 'turnout', 'eligible.voters' or 'seats'.") else 
  distweights <- switch(weight,
                        "constant" = {
                          tw <- new.list(length(judgeit.object$covars));
                          for (ii in 1:length(judgeit.object$covars))
                            tw[[ii]] <- array(1,c(dim(judgeit.object$covars[[ii]])[1],1));
                          tw
                        },
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
      
  #    print(ii)
      
    ury <- judgeit.object$fullrow[[ii]]
    zt <- reg(cbind(judgeit.object$covars[[ii]][ury,]),
              cbind(judgeit.object$voteshare[[ii]][ury]),
              judgeit.object$distweights[[ii]][ury])
    lambda <- NA
    l2 <- NA
    
    if (ii<nyears) if (redist[ii+1]) {
      ur <- intersect(judgeit.object$fullrow[[ii]],
                      judgeit.object$fullrow[[ii+1]])
      zt2 <- reg(u.c(judgeit.object$voteshare[[ii]],
                     judgeit.object$covars[[ii]],
                     judgeit.object$covars[[ii+1]])[ur,],
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
  
  return(judgeit.object)
}







`judgeit.preprocess` <- function (judgeit.object,
                                  year=NULL,
                                  district.select=NULL,

                                            #general terms
                                  predict=FALSE,
                                  new.covariates=NULL,  
                                  new.covariate.matrix=NULL,  
                                  new.seats=NULL,
                                  new.actual.voters=NULL,
                                  new.eligible.voters=NULL,

                                  extra.districts=NULL,

                                  new.pop.groups=NULL,
                                  
                                  vote.range=c(0.4,0.6), 
                                  seat.range=c(0.5,1),
                                  prob.range=c(0.5,1),
                                  prob.win=c(0.1,0.5,0.9),
                                  
                                  mean.votes=NULL,
                                  shift.in.votes=NULL,
                                  state.group=NULL,
                                  disaggregate=NULL,

                                  add.dc=FALSE,

                                  verbose=NULL)
  
{

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




  
  judgeit.object$predict <- predict
  if (is.na(judgeit.object$lambda)) {
    judgeit.object$lambda <- 0.5 #what to do if no information? what would Bayes do?
    warning ("Lambda is undefined: using replacement value of 0.5.")
  }
  
  if (is.null(year)) {
    warning (paste("No year was given: the most recent, year ",
                   judgeit.object$years[length(judgeit.object$voteshare)],", will be used.",sep=""))
    year <- length(judgeit.object$voteshare)
    judgeit.object$year <- year
  } else {
    #first priority: is it a calendar year in the set?
    yeary <- which (year==judgeit.object$years)
    if (length(yeary)>0) {year <- judgeit.object$year <- yeary[1]} else {
      if ((year>length(judgeit.object$voteshare))|(year<=0)) stop("The year you have selected is not in the system.")    
      judgeit.object$year <- year
    }
  }
  
  if (!is.null(new.covariates)) {  #implement modifications as requested
    if (!is.list(new.covariates))
      stop ("Counterfactual predictor task list must be of class 'list'.")
    if (as.logical(length(new.covariates)%%2))
      stop ("Counterfactual predictor task terms must come in pairs.")
    covarsnew <- judgeit.object$covars[[year]]
    for (ii in 1:floor(length(new.covariates)/2))
      covarsnew[,new.covariates[[2*ii-1]] ] <- new.covariates[[2*ii]]
  } else covarsnew <- new.covariate.matrix
  
  if (is.null(judgeit.object$covarsnew)) judgeit.object$covarsnew <- new.list(length(judgeit.object$covars))
  if (is.null(covarsnew)) judgeit.object$covarsnew[[year]] <- judgeit.object$covars[[year]] else
  judgeit.object$covarsnew[[year]] <- covarsnew

  if (dim(judgeit.object$covars[[year]])[2]!=dim(judgeit.object$covarsnew[[year]])[2])
    stop(paste0("Covariates in new group:",
                dim(judgeit.object$covarsnew[[year]])[2],"; Old group: ",
                dim(judgeit.object$covars[[year]])[2],"."))

  #in case of counterfactual, these could change.
  judgeit.object$seathold <- judgeit.object$seats[[year]]
  judgeit.object$actvotehold <- judgeit.object$turnout[[year]]
  judgeit.object$elgvotehold <- judgeit.object$eligible.voters[[year]]

  if (!is.null(new.seats)) {
    judgeit.object$seats[[year]] <- new.seats
    judgeit.object$predict <- TRUE
  }
  if (!is.null(new.eligible.voters)) {
    judgeit.object$eligible.voters[[year]] <- new.eligible.voters
    judgeit.object$predict <- TRUE
  }
  if (!is.null(new.actual.voters)) {
    judgeit.object$turnout[[year]] <- new.actual.voters
    judgeit.object$predict <- TRUE
  }
  
  if ((any(dim(judgeit.object$covars[[year]])!=dim(judgeit.object$covarsnew[[year]])))&(!judgeit.object$predict)) {
    warning (paste("In year ",year,", old and new covariates have a different number of observations. Proceeding under prediction mode, with one seat per district, and equal population.",sep=""))
    judgeit.object$predict <- TRUE
    judgeit.object$seats[[year]] <- cbind(rep(1,dim(judgeit.object$covarsnew[[year]])[1]))   #all set to 1.
    judgeit.object$eligible.voters[[year]] <- judgeit.object$turnout[[year]] <- cbind(rep(404,dim(judgeit.object$covarsnew[[year]])[1]))   #all set to 404.
  }


  with (judgeit.object,
        if (!all(c(length(seats[[year]])==dim(covarsnew[[year]])[1],
                   length(turnout[[year]])==dim(covarsnew[[year]])[1],
                   length(eligible.voters[[year]])==dim(covarsnew[[year]])[1])))
        stop (paste0("Row length mismatch in evaluation stage: covariate rows: ",dim(covarsnew[[year]])[1],
                     "; seat rows: ",length(seats[[year]]),
                     "; eligible voter rows: ",length(eligible.voters[[year]]),
                     "; actual turnout rows: ",length(turnout[[year]])))
        )



  
  if (is.null(new.pop.groups)) judgeit.object$pop.mat <- judgeit.object$pop.group.matrix[[year]] else {
    if (dim(new.pop.groups)[1]!=length(judgeit.object$turnout[[year]]))
      stop ("Population groups do not have the correct number of districts.")
    judgeit.object$pop.mat <- new.pop.groups
  }

  
  if (is.null(district.select)){
    district.select <- 1:length(judgeit.object$voteshare[[year]])
    if ((any(dim(judgeit.object$covars[[year]])!=dim(judgeit.object$covarsnew[[year]])))&(judgeit.object$predict)){
     district.select <- 1:dim(judgeit.object$covarsnew[[year]])[1]
     }
  } else{
  if (length(district.select)>length(judgeit.object$voteshare[[year]]))
    stop("In preprocess: Your district selection vector doesn't have the right number of districts.")
       }
  

  
  weighttype <- c("constant","turnout","seats","eligible.voters")
  judgeit.object$distweights[[year]] <- switch(judgeit.object$weight,
               "constant" = array(1,c(dim(judgeit.object$covarsnew[[year]])[1],1)),
               "turnout" = judgeit.object$turnout[[year]],
               "eligible.voters" = judgeit.object$eligible.voters[[year]],
               "seats" = judgeit.object$seats[[year]]
             )

  if (!judgeit.object$predict) {
    #has to be split, because predicted electoral maps can have a different number of seats.
    lamb <- judgeit.object$lam
    qun <- judgeit.object$covarsnew[[year]]-lamb*judgeit.object$covars[[year]]
    mu <- lamb*judgeit.object$voteshare[[year]] + qun%*%judgeit.object$beta[[year]]
    std <- sqrt((1-lamb^2)*judgeit.object$distweights[[year]]*judgeit.object$sig^2 +
                cbind(diag(qun%*%judgeit.object$vc[[year]]%*%t(qun))))
    base <- c(lamb*judgeit.object$voteshare[[year]])
    gamma.std <- sqrt(lamb*(1-lamb)*judgeit.object$distweights[[year]]*
                   judgeit.object$sig^2)
    stoned <- sqrt((1-lamb)*judgeit.object$distweights[[year]]*
                   judgeit.object$sig^2)
  } else {
    qun <- judgeit.object$covarsnew[[year]]
    mu <- qun%*%judgeit.object$beta[[year]]
    std <- sqrt(judgeit.object$distweights[[year]]*judgeit.object$sig^2+
                cbind(diag(qun%*%judgeit.object$vc[[year]]%*%t(qun))))
    base <- c(0*mu)
    stoned <- sqrt(judgeit.object$distweights[[year]]*judgeit.object$sig^2)
    gamma.std <- 0*stoned
  }

  pwin <- 1-pnorm(0.5,mu,std)
  tbo <- cbind(mu,std,pwin,judgeit.object$seats[[year]])
  inn <- intersect(missed(tbo), district.select)

  judgeit.object$baseline <- cbind(base[inn])
  judgeit.object$mult <- cbind(qun[inn,])
  judgeit.object$stoned <- stoned[inn]
  judgeit.object$gamma.std <- gamma.std

  
  judgeit.object$mu <- mu[inn]
  judgeit.object$std <- std[inn]
  judgeit.object$pwin <- pwin[inn]
  judgeit.object$seatvec <- cbind(judgeit.object$seats[[year]][inn])
  judgeit.object$weightvec <- cbind(judgeit.object$distweights[[year]][inn])
  judgeit.object$turnoutvec <- cbind(judgeit.object$turnout[[year]][inn])  
  judgeit.object$eligible.vec <- cbind(judgeit.object$eligible.voters[[year]][inn])
  judgeit.object$votevec <- cbind(judgeit.object$voteshare[[year]][inn])
  judgeit.object$rows <- rownames(judgeit.object$covars[[year]])[inn]
  judgeit.object$rowcodes <- inn
  judgeit.object$meanvote <- weighted.mean(judgeit.object$mu, judgeit.object$weightvec)

  
  judgeit.object$vote.range <- vote.range
  judgeit.object$prob.range <- prob.range
  judgeit.object$seat.range <- seat.range
  judgeit.object$prob.win <- prob.win

  judgeit.object$state.group <- state.group
  judgeit.object$disaggregate <- disaggregate

  
  if (is.null(judgeit.object$verbose) | !is.null(verbose)) judgeit.object$verbose <- verbose
  if (is.null(judgeit.object$verbose)) judgeit.object$verbose <- FALSE
  
  judgeit.object$add.dc <- add.dc

  
  if (!is.null(judgeit.object$pop.mat))
    judgeit.object$pop.mat <- judgeit.object$pop.mat[inn,] else
      judgeit.object$pop.mat <- cbind(judgeit.object$turnoutvec)

  meanvote <- round(100*judgeit.object$meanvote)/100
  if (is.null(mean.votes)) mean.votes <- seq(meanvote-0.15,
                                             meanvote+0.15,
                                             length.out=31)
  judgeit.object$mean.votes <- mean.votes
  judgeit.object$shift.in.votes <- shift.in.votes


  #message(paste(length(judgeit.object$voteshare[[year]]),
  #              length(judgeit.object$ext[,1]),
  #              length(judgeit.object$distweights[[year]]),
  #              length(judgeit.object$ext[,4])))

    
  if (!judgeit.object$predict) {
    judgeit.object$obsvotes <- weighted.mean(c(judgeit.object$voteshare[[year]],
                                               judgeit.object$ext[,1]),
                                             c(judgeit.object$distweights[[year]],
                                               judgeit.object$ext[,4]),na.rm=TRUE)
    judgeit.object$obsseats <- weighted.mean(v2s(c(judgeit.object$voteshare[[year]],
                                                   judgeit.object$ext[,3])),
                                             c(judgeit.object$seats[[year]],
                                               judgeit.object$ext[,4]),na.rm=TRUE)
  }
  
  return(judgeit.object)
}

`judgeit` <-
function (model.formula=~1,
          vote.formula=NULL,
          same.districts=NULL,
          data,
          pop.groups=NULL,

          uncontesteds.method="default",
          uncontested.low=0.05, uncontested.low.new=0.25,
          uncontested.high=0.95, uncontested.high.new=0.75,

          use.last.votes=TRUE,
          simulations=201, 
          
          weight="constant", 
          years=NULL,
          
          routine=NULL, year=NULL, judgeit.object=NULL,
          verbose=FALSE,

          #arguments for preprocess.
          #predict=FALSE,
          #new.covariates=NULL,  
          #new.covariate.matrix=NULL,  
          #new.seats=NULL,
          #new.actual.voters=NULL,
          #new.eligible.voters=NULL,
          
          ...    
          )
{ #judgeit
    
  #verbose <- FALSE
  if (is.null(judgeit.object)) { #data loading stage.
    yy <- xx <- wts <- wtv <- wtt <- NULL
    pop.group.matrix <- new.list(length(data))
    a.n <- function(...) as.numeric(...)
    
    if (!is.data.frame(data)) { #more than one year contained in frame? If so...

      if (!is.list(data)) stop ("Data object is of unknown format.")
      
      if (is.null(years)) {
        years <- names(data)
        if (is.null(years)) years <- 1:length(data)
      } else
        if (length(years)!=length(data)) stop("The number of years does not match the length of the data list.")
      
      for (ii in 1:length(data)) {
        framer <- model.frame(formula=model.formula,data=data[[ii]],
                              na.action=na.pass,...)
        yy[[ii]] <- a.n(model.response(framer))
        if (length(yy[[ii]])==0) stop (paste("Year",ii,"has no vote results. Check your formula?"))
        xx[[ii]] <- model.preds(framer)
        
        if (!is.null(vote.formula)) {
          fr2 <- model.frame(formula=vote.formula,data=data[[ii]],
                             na.action=na.pass,...)
          if (length(fr2)>0) vt <- as.matrix(model.response(fr2)) else
          vt <- matrix(rep(1,length(yy[[ii]])))
          if (!is.null(vt)) {  #at least one column
            if (dim(vt)[2]>1) {
              wtv[[ii]] <- a.n(vt[,1]);
              wtt[[ii]] <- a.n(vt[,2])
            } else
            wtv[[ii]] <- wtt[[ii]] <- a.n(vt[,1]) }
          st <- model.preds(fr2); wts[[ii]] <- a.n(st[,1])
        }
        
        #adding pop.by.group formula until I figure something better.
        if (!is.null(pop.groups)) {
          pop.frame <- model.frame(formula=pop.groups,data=data[[ii]],
                                   na.action=na.pass,...)
          pop.hold <- model.preds(pop.frame)
          pop.group.matrix[[ii]] <- cbind(pop.hold[,2:dim(pop.hold)[2]])
        } else pop.group.matrix[[ii]] <- NULL
      }
    } else { #only one year this time.
      framer <- model.frame(formula=model.formula,data=data,na.action=na.pass,...)
      if (is.null(years)) years <- 1 else if (length(years)>1)
        stop ("The input suggests that only one year of data is present, but more than one year name has been entered.")
      
      yy[[1]] <- a.n(model.response(framer))
      xx[[1]] <- model.preds(framer)
      if (!is.null(vote.formula)) {
        fr2 <- model.frame(formula=vote.formula,data=data,na.action=na.pass,...)
        if (length(fr2)>0) vt <- as.matrix(model.response(fr2)) else
        vt <- matrix(rep(1,length(yy[[1]])))
        if (!is.null(vt)) {  #at least one column
          if (dim(vt)[2]>1) {wtv[[1]] <- a.n(vt[,1]); wtt[[1]] <- a.n(vt[,2])} else
          wtv[[1]] <- wtt[[1]] <- a.n(vt[,1])
        }
        
        st <- model.preds(fr2); wts[[1]] <- a.n(st[,1])
      }

        if (!is.null(pop.groups)) {
          pop.frame <- model.frame(formula=pop.groups,
                                   data=data[[ii]],
                                   na.action=na.pass, ...)
          pop.group.matrix[[1]] <- model.preds(pop.frame)
        }

    }
    
    judgeit.object <-
      judgeit.setdata(xx,yy,wtv,wtt,wts,
                      same.districts=same.districts,
                      uncontesteds.method=uncontesteds.method,
                      uncontested.low=uncontested.low, uncontested.low.new=uncontested.low.new,
                      uncontested.high=uncontested.high, uncontested.high.new=uncontested.high.new,
                      simulations=simulations, weight=weight,
                      use.last.votes=use.last.votes)

    judgeit.object$years <- years
    #prelim routine.
    judgeit.object <- judgeit.get.sigma.lambda(judgeit.object, weight)

    judgeit.object$pop.groups <- pop.group.matrix
  }  #first step done.

  judgeit.object$verbose <- verbose
  
  if (!is.null(routine)) {
    
    judgeit.object <- judgeit.preprocess(judgeit.object, year,
#                                         predict=TRUE,
#                                         new.covariate.matrix=enacted.matrix,
#                                         new.seats=rep(1,dim(enacted.matrix)[1]),
#                                         new.actual.voters=rep(1,dim(enacted.matrix)[1]),
#                                         new.eligible.voters=rep(1,dim(enacted.matrix)[1]),
                                         ...)

    output <-
      switch (routine,
              "seats" = sv(judgeit.object),              
              "prob" = pv(judgeit.object),
              "conditional.seats" = freq (judgeit.object),
              "svsum" = svsum.raw(judgeit.object),
              "svsums" = svsum.raw(judgeit.object),
              "distreport" = dists(judgeit.object),
              "voting.power" = voting.power.raw(judgeit.object),
              "winvote" = winvote.raw(judgeit.object),
              "winprob" = winprob(judgeit.object),
              "chopcollege" = chopcollege.raw(judgeit.object),
              
              writeLines ("An unknown routine was entered.")
              )

    judgeit.object$output <- output
    judgeit.object$outputyear <- year #formula based?

    judgeit.object$seats[[year]] <- judgeit.object$seathold
    judgeit.object$turnout[[year]] <- judgeit.object$actvotehold
    judgeit.object$eligible.voters[[year]] <- judgeit.object$elgvotehold

  }

  return(judgeit.object)
}


`judgeit.setdata` <-
function (covars=NULL, voteshare=NULL, turnout=NULL, eligible.voters=NULL,
          seats=NULL, same.districts=NULL, use.last.votes=TRUE,
          uncontesteds.method="impute", uncontested.low=0.05,
          uncontested.low.new=0.25, uncontested.high=0.95,
          uncontested.high.new=0.75, simulations=200, weight="constant") {

  flag <- FALSE

  out <- list(covars=covars,voteshare=voteshare,turnout=turnout)

  if (is.null(same.districts)) {
    same.districts <- rep(0,length(voteshare)) 
    if (length(voteshare)>1) for (ii in 2:length(voteshare)) same.districts[ii] <- 1*(length(voteshare[[ii]])==length(voteshare[[ii-1]]))
  } else if (length(same.districts)!=length(voteshare)) stop("Number of terms in same.districts vector does not match the number of elections.")
  
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

  if (flag) stop("There was an error loading the data.") else class(out) <- "judgeit"

  if (is.null(out)) stop() else {
    out <- fix.uncontested (out,same.districts,uncontesteds.method)
    if ((use.last.votes)&(length(out$covars)>1)) for (ii in 2:length(out$covars)) 
      if (same.districts[[ii]]) {out$covars[[ii]] <- unique.columns(cbind(out$covars[[ii]],out$voteshare[[ii-1]])); colnames(out$covars[[ii]])[dim(out$covars[[ii]])[2]] <- "lastvote"}

#    obj.hold <<- out 
    for (ii in 1:length(out$voteshare)) {
#      writeLines(paste("setdata",ii))
      caddy <- cbind (out$voteshare[[ii]],out$covars[[ii]],out$eligible.voters[[ii]],out$turnout[[ii]],out$seats[[ii]])
      out$fullrow[[ii]] <- as.numeric(missed(caddy))
      if (length(out$fullrow[[ii]])==0) stop ("Trouble in setting data: There appear to be no districts with sufficient data.")
      out$covars[[ii]] <- unique.columns(out$covars[[ii]],keepers=out$fullrow[[ii]])
    }
  }
  return(out)
  writeLines ("Election data has been loaded into specified JudgeIt object.")
}

"JudgeIt" <-
function (...) judgeit(...)

`kernel.plot` <-
function (y, y2=NULL, col=1, ty="l", new=TRUE, grid=TRUE, ...) {
  dt <- density(y,from=0,to=1)
  if (!is.null(y2)) {dt2 <- density(y2,from=0,to=1); maxy <- max(c(dt$y),c(dt2$y))} else
    maxy <- max(dt$y)
  plot (c(0,1),c(0,maxy),ty="n",...)
  lines (dt$x,dt$y,ty=ty,col=col)
  if (!is.null(y2)) lines (dt2$x,dt2$y,col=col+1)
  if (grid) for (ii in 0:10) abline(v=ii/10,col=8,lty=2)
  #lines(rep(ii/10,2),c(0,maxy),col=8,lty=2)
}

`plot.judgeit` <-
function(x,straight.up=FALSE, year=1, ...) {
  
  if (length(x$outputyear)>0)
    yrfull <- x$years[x$outputyear] else
  yrfull <- year

  if (length(x$output)>0) {
    plot(x$output, ...)
  } else {
    year <- x$outputyear
    message (paste("Outputting a kernel plot of districts in year ",
                   yrfull,".",sep=""))
    yy <- x$voteshare[[year]]
    yy <- yy[!is.na(yy)]; kernel.plot(yy, ...)
  }
  
 #   switch (judgeit.object$outputclass,
 #         "seats" = plot(judgeit.object$output),
 #         "prob" = plot(judgeit.object$output),
 #         "distreport" = plot(judgeit.object$output),
 #         "chopcollege" = plot(judgeit.object$output),  )
    
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

`head.judgeit` <- function(x,...) head(x$output,...)
`tail.judgeit` <- function(x,...) tail(x$output,...)



#direct routine access.
`seats` <- function(judgeit.object,...) {
  judgeit.object <- judgeit.preprocess(judgeit.object,...)
  return (sv(judgeit.object))
}

`print.judgeit.seats` <- function(x,...) print(x$output,...)
`head.judgeit.seats` <- function(x,...) head(x$output,...)
`tail.judgeit.seats` <- function(x,...) tail(x$output,...)

`plot.judgeit.seats` <- function(x, main=NULL, xlab=NULL, ylab=NULL, ...) with(x,{
  if (is.null(main)) main <- paste("Seats-Votes Plot for",cal.year)
  if (is.null(xlab)) xlab <- "Vote Proportion"
  if (is.null(ylab)) ylab <- "Seat Proportion"
  
  plot(c(0,1),c(0,1),ty="n",main=main, xlab=xlab, ylab=ylab, ...)
  t.x <- as.numeric(rownames(output))
  t.y <- output[,1]
  line.bottom <- output[,2]
  line.top <- output[,3]
  lines(t.x,t.y)
  lines(t.x,pmin(1,line.top),col=3)
  lines(t.x,pmax(line.bottom,0),col=3)
  abline(h=0.5,col=8)
  abline(v=0.5,col=8)
  points(obsvotes,obsseats,col=4)
  text(obsvotes,obsseats,paste("Vote:",signif(obsvotes,4)),pos=4,...)
  text(obsvotes,obsseats-0.05,paste("Seats:",signif(obsseats,4)),pos=4,...)
})



`voting.power` <- function(judgeit.object,...) {
  judgeit.object <- judgeit.preprocess(judgeit.object,...)
  return (voting.power.raw(judgeit.object))
}

`print.judgeit.voting.power` <- function(x,...) print (x$output,...)
`head.judgeit.voting.power` <- function(x,...) head(x$output,...)
`tail.judgeit.voting.power` <- function(x,...) tail(x$output,...)

`plot.judgeit.voting.power` <- function(x,...) {
#parallel covariate plot -- scaled?
  writeLines ("voting.power does not currently support a plot option.")
}


`prob` <- function(judgeit.object,...) seats.with.prob(judgeit.object,...)

`seats.with.prob` <- function(judgeit.object,...) {
  judgeit.object <- judgeit.preprocess(judgeit.object,...)
  writeLines ("seats.with.prob can take some time. Stand by...")
  return (pv(judgeit.object))
}

`print.judgeit.prob` <- function(x,...) print(x$output,...)
`head.judgeit.prob` <- function(x,...) head(x$output,...)
`tail.judgeit.prob` <- function(x,...) tail(x$output,...)

`plot.judgeit.prob` <- function(x,...) {
  xlib <- ifelse(x$outtype=="mean.votes","Vote Proportion","Vote Proportion (with differences)")
  plot(c(0,1),c(0,1),ty="n",
       main=paste("Seat Proportions with Indicated Probability of Victory, ",x$cal.year),
       xlab=xlib,ylab="Seat Proportion with Selected Probability Range",...)
  t.x <- ifelse(x$outtype=="mean.votes",0,x$meanvote)+as.numeric(rownames(x$output))
  t.y <- x$output[,1]
  line.bottom <- x$output[,2]
  line.top <- x$output[,3]
  lines(t.x,t.y)
  lines(t.x,pmin(1,line.top),col=3)
  lines(t.x,pmax(line.bottom,0),col=3)
  for (ii in 0:10) {abline(h=ii/10,col=8); abline(v=ii/10,col=8)}

}


`results.prob` <- function(judgeit.object,...) winprob(judgeit.object,...)

`winprob` <- function(judgeit.object,...) {
  judgeit.object <- judgeit.preprocess(judgeit.object,...)
  writeLines ("results.prob can take some time. Stand by...")
  return (winprob.raw(judgeit.object))
}

`plot.judgeit.winprob` <- function(x,...) {
  writeLines ("results.prob objects do not currently support a plot option.")
  #writeLines ("Still working on a results.prob plot.")
}

`print.judgeit.winprob` <- function(x,...) {print(x$output)}
`head.judgeit.winprob` <- function(x,...) head(x$output,...)
`tail.judgeit.winprob` <- function(x,...) tail(x$output,...)



`district.report` <- function(judgeit.object,...) {
  judgeit.object <- judgeit.preprocess(judgeit.object,...)
  return (dists(judgeit.object))
}
`distreport` <- function(judgeit.object,...) district.report(judgeit.object,...)


`plot.judgeit.district.report` <- function(x, xlab=NULL, ylab=NULL, main=NULL, ...) {
  if (is.null(xlab)) xlab <- "Vote Share"
  if (is.null(ylab)) ylab <- "Likelihood of District Vote"
  if (is.null(main)) main <- paste("District Vote Likelihood Plot for",x$cal.year)
  
  if (colnames(x$output)[2]!="Std. Dev.") {
    kernel.plot (x$output[,1],x$output[,2],
                 xlab=xlab, ylab=ylab,
                 main=main, ...)
    text(0.9,1.3,"Observed Votes",col=1,...)
    text(0.9,1.4,"Model Prediction",col=2,...)
  } else {
    kernel.plot (x$output[,1],
                 xlab=xlab, ylab=ylab,
                 main=main, ...)
    text(0.9,1.4,"Model Prediction",col=1,...)
  }
}

`print.judgeit.district.report` <- function(x,...) {print(x$output)}
`head.judgeit.district.report` <- function(x,...) head(x$output,...)
`tail.judgeit.district.report` <- function(x,...) tail(x$output,...)



`conditional.seats` <- function(judgeit.object,...) {
  judgeit.object <- judgeit.preprocess(judgeit.object,...)
  return (freq(judgeit.object))
}

`plot.judgeit.conditional.seats` <- function(x,...) {
  #writeLines ("Still working on a conditional seats plot.")
  writeLines ("Conditional seats objects do not currently support a plot option.")
}

`print.judgeit.conditional.seats` <- function(x,...) {print(x$output)}
`head.judgeit.conditional.seats` <- function(x,...) head(x$output,...)
`tail.judgeit.conditional.seats` <- function(x,...) tail(x$output,...)



`votes.for.result` <- function(judgeit.object,...) winvote(judgeit.object,...)

`winvote` <- function(judgeit.object,...) {
  judgeit.object <- judgeit.preprocess(judgeit.object,...)
  writeLines ("votes.for.result can take some time. Stand by...")
  return (winvote.raw(judgeit.object))
}

`plot.judgeit.winvote` <- function(x,...) {
  writeLines ("votes.for.result objects do not currently support a plot option.")
}

`print.judgeit.winvote` <- function(x,...) {print(x$output)}
`head.judgeit.winvote` <- function(x,...) head(x$output,...)
`tail.judgeit.winvote` <- function(x,...) tail(x$output,...)



`bias.resp` <- function(judgeit.object,...) svsum(judgeit.object,...)

`svsum` <- function(judgeit.object,...) {
  judgeit.object <- judgeit.preprocess(judgeit.object,...)
  return (svsum.raw(judgeit.object))
}

`plot.judgeit.svsum` <- function(x,...) {
  writeLines ("Bias-responsiveness summaries do not currently support a plot option.")

  currently.out.of.commission <- function() with(x,{
    plot(c(0.4,0.6),c(0.4,0.6),ty="n",main=paste("Seats-Votes Summary Plot for",cal.year),
         xlab="Vote Proportion",ylab="Seat Proportion",...)
    t.x <- as.numeric(rownames(svplot))
    t.y <- svplot[,1]
    line.bottom <- svplot[,2]
    line.top <- svplot[,3]
    lines(t.x,t.y)
    lines(t.x,pmin(1,line.top),col=3)
    lines(t.x,pmax(line.bottom,0),col=3)
    abline(h=0.5,col=8)
    abline(v=0.5,col=8)
    
    #plot bias, responsiveness bars now.
    b.pts <- svsums[1,c(1,3,5)]/2+0.5
    points (0.5,b.pts[1],col=4)
    lines (rep(0.5,2),b.pts[1]+svsums[1,2]*c(-1,1),col=4,lwd=3)
    lines (rep(0.5,2),b.pts[2:3],col=4)
    
    lines (0.5+c(-0.01,0.01),b.pts[1]+svsums[3,1]*c(-0.01,0.01),col=2)
    lines (0.5+c(-0.01,0.01),b.pts[1]+svsums[3,3]*c(-0.01,0.01),col=2,lty=2)
    lines (0.5+c(-0.01,0.01),b.pts[1]+svsums[3,5]*c(-0.01,0.01),col=2,lty=2)
    
    lines (obsvotes+c(-0.01,0.01),obsseats+svsums[4,1]*c(-0.01,0.01),col=2)
    lines (obsvotes+c(-0.01,0.01),obsseats+svsums[4,3]*c(-0.01,0.01),col=2,lty=2)
    lines (obsvotes+c(-0.01,0.01),obsseats+svsums[4,5]*c(-0.01,0.01),col=2,lty=2)
    points(obsvotes,obsseats,col=4)  
  })
}

`print.judgeit.svsum` <- function(x,...) {print(x$svsums)}
`head.judgeit.svsum` <- function(x,...) head(x$output,...)
`tail.judgeit.svsum` <- function(x,...) tail(x$output,...)

`plot.judgeit.chopcollege` <- function(x, xlab=NULL, ylab=NULL, main=NULL,
                                       show.mean.vote=TRUE, ...) with(x,{

  if (is.null(xlab)) xlab <- "Vote Proportion"
  if (is.null(ylab)) ylab <- "Elector Proportion"
  if (is.null(main)) main <- paste("Electoral College Plot for ",cal.year)

  plot(c(0,1),c(0,1),ty="n", main=main, xlab=xlab, ylab=ylab, ...)
  
  t.x <- as.numeric(rownames(sv))
  t.y <- sv[,1]
  line.bottom <- sv[,2]
  line.top <- sv[,3]
  lines(t.x,t.y)
  lines(t.x,pmin(1,line.top),col=3)
  lines(t.x,pmax(line.bottom,0),col=3)
  abline(h=0.5,col=8)
  abline(v=0.5,col=8)

  if (show.mean.vote) abline(v=mean.vote,lty=2,col=4)

  #put winning-vote measures on there too.
  #partisan bias.
  points(0.5, partisan.bias[1], col=2, pch=19)
  lines(rep(0.5,2), partisan.bias[2:3], col=2, lwd=2)
  #lines(0.5+c(-0.005,0.005),rep(partisan.bias[2],2),col=2)
  #lines(0.5+c(-0.005,0.005),rep(partisan.bias[3],2),col=2)
  
  #winvote.
  points(winvote.summary[2,2], 0.5, col=4, pch=19)
  lines(winvote.summary[2,c(1,3)],rep(0.5,2),col=4, lwd=2)
  #lines(rep(winvote.summary[2,1],2),0.5+c(-0.004,0.004),col=4)
  #lines(rep(winvote.summary[2,3],2),0.5+c(-0.004,0.004),col=4)

})
