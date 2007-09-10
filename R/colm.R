`colm` <-
function (matrx,index) {
  out <- cbind(matrx[,index])
  colnames(out) <- index
  out
}

