`crossadd` <-
function (vert,horiz) {
  vl <- length(vert); hl <- length(horiz)
  vert <- matrix(rep(as.numeric(vert),hl),vl)
  horiz <- matrix(rep(as.numeric(horiz),vl),vl,byrow=T)
  vert+horiz
}

