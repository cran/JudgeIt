`rb` <-
function (dr,bits) if (!is.null(bits)) rbind (dr,as.matrix(replicate(dim(dr)[2],bits))) else dr

