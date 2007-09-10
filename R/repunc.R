`repunc` <-
function (voteshare,l,u,lr,ur) {
  f1 <- function (a,b,c,d,e) if (!is.na(a)) {
    if (a<b) a <- d else if (a>c) a <- e else a
  } else NA
  sapply (voteshare,f1,l,u,lr,ur)
}

