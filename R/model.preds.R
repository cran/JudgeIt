`model.preds` <-
function(frame) {
  checker <- attr(attr(frame,"terms"),"factors")
  preds <- NULL
  if (!is.null(dim(checker)[2])) for (ii in 1:dim(checker)[2]) {
    thinglonger <- as.matrix(frame[,which(checker[,ii]>0)])
    preds <- cbind(preds,apply(thinglonger,1,prod))
  }
  
  if (attr(attr(frame,"terms"),"intercept")) {
    preds <- cbind(rep(1,dim(frame)[1]),preds)
    colnames(preds) <- c("(Intercept)",attr(attr(frame,"terms"),"term.labels"))
  } else colnames(preds) <- attr(attr(frame,"terms"),"term.labels")
  rownames(preds) <- rownames(frame)
  return(preds)
}

