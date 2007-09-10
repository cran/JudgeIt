`judgeit` <-
function (model.formula=~1,vote.formula=NULL,same.districts=NULL,data,
                    uncontesteds.method="default",uncontested.low=0.05,uncontested.low.new=0.25,uncontested.high=0.95,uncontested.high.new=0.75,
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
                    ...    #for the model.frame work.
                   )
  { #judgeit
    
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
    zt <- reg(judgeit.object$covars[[ii]][ury,],
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

    
    
    if ((any(dim(judgeit.object$covars[[year]])!=dim(judgeit.object$covarsnew[[year]])))&(!judgeit.object$predict)) {
      writeLines (paste("Warning: in year ",year,", old and new covariates have a different number of observations. Proceeding under prediction mode, with one seat per district, and equal population.",sep=""))
      judgeit.object$predict <- T
      judgeit.object$seats[[year]] <- cbind(rep(1,dim(judgeit.object$covarsnew[[year]])[1]))   #all set to 1.
      judgeit.object$eligible.voters[[year]] <- judgeit.object$turnout[[year]] <- cbind(rep(404,dim(judgeit.object$covarsnew[[year]])[1]))   #all set to 404.
    }

    with (judgeit.object, if (!all(c(length(seats[[year]])==dim(covarsnew[[year]])[1],length(turnout[[year]])==dim(covarsnew[[year]])[1],length(eligible.voters[[year]])==dim(covarsnew[[year]])[1]))) stop (paste("Error in evaluation stage: the number of covariate, seat, eligible and actual voter rows do not match each other.")))

#                       vote.range=NULL, #c(0.45,0.55),

#                    mean.votes=NULL, #seq(min(vote.range),max(vote.range),length.out=round((max(vote.range)-min(vote.range))*100)+1), #options.
 
   
    #this attaches the output from judgeit.core and returns the original object. This means that whatever we add to the object within judgeit.core is temporary.
    try({judgeit.object$output <- judgeit.core(routine=routine,judgeit.object=judgeit.object,
                 year=year,
                 expected.value.only=expected.value.only,
                 distselect=distselect, #subset?
                                               
                 vote.range=vote.range,mean.votes=mean.votes, #options.
                 shift.in.votes=shift.in.votes, winvote=winvote,
                 voting.groups=voting.groups,all.groups=all.groups,probability.range=probability.range)
       judgeit.object$outputclass <- routine
       judgeit.object$outputyear <- year}) #formula based?

    judgeit.object$seats[[year]] <- judgeit.object$seathold
    judgeit.object$turnout[[year]] <- judgeit.object$actvotehold
    judgeit.object$eligible.voters[[year]] <- judgeit.object$elgvotehold

  }

  #it ends here.
  return(judgeit.object)
}

