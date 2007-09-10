`winvote` <-
function (judgeit.object,year,pr) {
  work <- function(vs) {
    lo <- 0; hi <- 1;probwin <- 9999;res <- 0.5;ii <- 0;bogus <- F
    patho <- NULL
    if ((pr>=0)&(pr<=1)) while (abs(probwin-pr)>0.005) {
      res <- mean(c(lo,hi))
      vsim <- vs-0.5+res
      probwin <- mean(apply(v2s(rb(vsim,judgeit.object$extra.districts[,1])),2,weighted.mean,c(judgeit.object$seatvec,judgeit.object$extra.districts[,3])))
      if (pr>probwin) lo <- mean(c(lo,res)) else hi <- mean(c(hi,res))
      patho <- rbind(patho,c(pr,probwin,lo,hi))
      ii <- ii+1
      if (ii == 100) {
        writeLines ("Too many repetitions...")
        probwin <- pr
        bogus <- T
      }
      if (ii>10) if (all(patho[ii,2]==patho[(ii-9):(ii-1),2])) {
        res <- -probwin
        probwin <- pr
      }
    } else {writeLines ("Proportion isn't between 0 and 1."); bogus <- T}
    if (!bogus) res else {errcheck <<- patho; writeLines(paste(ii)); NA}
  }

  if (length(pr)>1) stop("winvote routine takes one probability value only.")
  
  vs0 <- hyp.elects(judgeit.object,year,c(NA,0.5),truncit=F)
  mstat <- -1
  mstat <- work(vs0)
  if (mstat>=0) sdstat <- sqrt(mstat*(1-mstat)/judgeit.object$sims)  #It's a probability, after all.

  if (mstat<0) out <- "Problem with the winvote routine." else {
    out <- array(c(mstat,sdstat),c(1,2))
    colnames(out) <- c("Vote Share","SD")
    rownames(out) <- paste("P(win) =",pr)
  }

  return(out)
}

