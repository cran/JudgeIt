`uncontested.remove` <-
function (data,ii) {
  data$voteshare[[ii]] <- delunc(data$voteshare[[ii]],data$uncL,data$uncU)
  to.keep <- which (!is.na(data$voteshare[[ii]]))

  data$voteshare[[ii]] <- rowm(data$voteshare[[ii]],to.keep)
  data$covars[[ii]] <- rowm(data$covars[[ii]],to.keep)
  data$turnout[[ii]] <- rowm(data$turnout[[ii]],to.keep)
  data$eligible.voters[[ii]] <- rowm(data$eligible.voters[[ii]],to.keep)
  data$seats[[ii]] <- rowm(data$seats[[ii]],to.keep)
  
  return(data)
}

