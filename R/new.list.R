`new.list` <-
function(len) {
  out <- list(NA)
  if (len<1) stop ("Can't make a list of length ",len,".")
  for (ii in 1:len) out[[ii]] <- NA
  out
}

