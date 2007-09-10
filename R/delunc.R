`delunc` <-
function(voteshare,uncL,uncU) {
  f1 <- function (a,b,c) if (is.na(a)) NA else
    if (a<b) NA else if (a>c) NA else a
  sapply (voteshare,f1,uncL,uncU)
}

