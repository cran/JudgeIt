`rowm` <-
function (matrx,index) {
#  writeLines ("e1 rowm")
  out <- matrix(matrx[index,],length(index))
  rownames(out) <- index
  out
}

