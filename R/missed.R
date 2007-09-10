`missed` <-
function (mat) {one <- function(row) !any(is.na(row)); which(apply(mat,1,one)>0)}

