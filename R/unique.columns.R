`unique.columns` <-
function (x1,x2=NULL,keepers=NULL) {
  copies <- NULL
  jo <- cbind(x1,x2)
  ju <- jo; if (!is.null(keepers)) jo <- cbind(jo[keepers,])
  #cut any completely missing columns
  somedata <- function(col) !all(is.na(col)); jo <- as.matrix(jo[,apply(jo,2,somedata)])
  j <- cropmiss(jo)$covars
  if (dim(j)[2]>1) for (i1 in 1:(dim(j)[2]-1)) for (i2 in (i1+1):dim(j)[2]) {
    cond1 <- any(j[,i2]==0)|any(j[,i1]==0)
    if (is.na(cond1)) cond1 <- F
    if (cond1) remv <- j[,i1]==j[,i2] else remv <- all(j[1,i1]/j[1,i2] == j[,i1]/j[,i2])
    remv <- remv[!is.na(remv)]
    if ((length(remv)>0)&prod(remv)) copies <- c(copies,i2)#; writeLines(paste(length(remv),prod(remv),i1,i2))}
  }  
  if (length(copies)>0) as.matrix(ju[,-unique(copies)]) else as.matrix(ju)
}

