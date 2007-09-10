`fix.uncontested` <-
function (data,same.districts,uncontesteds.method) {
  dists <- length(data$covars)
  for (ii in 1:dists) {
    data <- switch (uncontesteds.method,
          remove = uncontested.remove(data,ii),
          impute = uncontested.impute(data,ii),
          default = uncontested.default(data,ii),
          nochange = data)
  }
  for (ii in 1:length(data$voteshare)) {
    data$voteshare[[ii]] <- cbind(data$voteshare[[ii]])
    data$covars[[ii]] <- cbind(data$covars[[ii]])
    data$eligible.voters[[ii]] <- cbind(data$eligible.voters[[ii]])
    data$turnout[[ii]] <- cbind(data$turnout[[ii]])
    data$seats[[ii]] <- cbind(data$seats[[ii]])
  }
  return(data)
}

