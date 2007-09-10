`uncontested.default` <-
function (data,ii) {
  data$voteshare[[ii]] <- repunc(data$voteshare[[ii]],data$uncL,data$uncU,data$uncLR,data$uncUR)
  return(data)
}

