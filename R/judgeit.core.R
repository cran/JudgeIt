`judgeit.core` <-
function (routine,judgeit.object,year,#predict=F,#covarsnew,
                    expected.value.only,distselect,
                    vote.range,mean.votes,
                    shift.in.votes,winvote,
                    probability.range,voting.groups,all.groups) {

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

  if (!judgeit.object$predict) {
    lamb <- judgeit.object$lam
    qun <- judgeit.object$covarsnew[[year]]-lamb*judgeit.object$covars[[year]]
    mu <- lamb*judgeit.object$voteshare[[year]] + qun%*%judgeit.object$beta[[year]]
    std <- sqrt((1-lamb^2)*judgeit.object$distweights[[year]]*judgeit.object$sig^2*(!judgeit.object$svexpected.value.only) +
                cbind(diag(qun%*%judgeit.object$vc[[year]]%*%t(qun))))
    base <- c(lamb*judgeit.object$voteshare[[year]])
    stoned <- sqrt((1-lamb^2)*judgeit.object$distweights[[year]]*judgeit.object$sig^2*(!judgeit.object$svexpected.value.only))
  } else {
    qun <- judgeit.object$covarsnew[[year]]
    mu <- qun%*%judgeit.object$beta[[year]]
    std <-
      sqrt(judgeit.object$distweights[[year]]*judgeit.object$sig^2*(!judgeit.object$svexpected.value.only)+cbind(diag(qun%*%judgeit.object$vc[[year]]%*%t(qun))))
    base <- c(0*mu)
    stoned <- sqrt(judgeit.object$distweights[[year]]*judgeit.object$sig^2*(!judgeit.object$svexpected.value.only))
  }
  pwin <- 1-pnorm(0.5,mu,std)

  
  tbo <- cbind(mu,std,pwin,judgeit.object$seats[[year]])
  inn <- intersect(missed(tbo),distselect)

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

  #print(c(dim(qun[inn,])))
  
  judgeit.object$mu <- mu
  judgeit.object$std <- std
  judgeit.object$pwin <- pwin
  meanvote <- judgeit.object$meanvote <- weighted.mean(mu,std^2)
  judgeit.object$seatvec <- cbind(judgeit.object$seats[[year]][inn])
  judgeit.object$weightvec <- cbind(judgeit.object$distweights[[year]][inn])
  judgeit.object$turnoutvec <- cbind(judgeit.object$turnout[[year]][inn])  
  judgeit.object$votevec <- cbind(judgeit.object$voteshare[[year]][inn])
  judgeit.object$rows <- rownames(judgeit.object$covars[[year]])[inn]

  if (is.null(vote.range)) vote.range <- c(0.4,0.6)
  if (is.null(mean.votes)) mean.votes <- seq(meanvote-0.15,meanvote+0.15,length.out=31)

  output <- switch (routine,
          "seats" = sv(judgeit.object,year,mean.votes),
          "prob" = {
            if (is.null(mean.votes)&is.null(shift.in.votes)) {
              writeLines ("Neither mean votes nor shifts in the mean vote were specified. Setting the observed votes to a range about the observed outcome.")
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
            
          writeLines ("An invalid routine was entered.")
          )
  
 return(output)
}

