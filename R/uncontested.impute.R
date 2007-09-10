`uncontested.impute` <-
function(data,year) {
  data$voteshare[[year]] <- delunc(data$voteshare[[year]],data$uncL,data$uncU)

  voteshare <- data$voteshare[[year]]
  covars <- data$covars[[year]]
  present <- data$fullrow[[year]]
  
  rstf <- reg(covars[present,],voteshare[present])
  #now, get values.
 
  pv <- rnorm(length(rstf$vshat),rstf$vshat,sqrt(rstf$sig2))
  yt <- rep(NA,length(voteshare)); yt[present] <- pv
  pv <- trunc01(pv) 

  out <- voteshare
  for (ii in 1:length(voteshare)) if (is.na(voteshare[ii])) out[ii] <- pv[ii]

  data$voteshare[[year]] <- voteshare
  return(data)
}

