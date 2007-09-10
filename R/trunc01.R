`trunc01` <-
function(r1) {
  below <- r1<0
  above <- r1>1
  between <- !(below|above)
  0*r1+r1*between+1*above
}

