`summary.judgeit` <-
function (object,year=NA,...) {
  qsum <- function (set) c(mean(set[!is.na(set)]),
                           sd(set[!is.na(set)]),
                           var(set[!is.na(set)]),
                           min(set[!is.na(set)]),
                           max(set[!is.na(set)]),
                           sum(1*!is.na(set)),
                           sum(1*is.na(set)))
  #x,y,wtv,eligible.voters,seats
  out <- NULL
  if (!is.na(year)) {
    out <- rbind(t(apply(object$covars[[year]],2,qsum)),qsum(object$voteshare[[year]]),
               qsum(object$turnout[[year]]),qsum(object$eligible.voters[[year]]),qsum(object$seats[[year]]))

    jj <- dim(out)[1]
    rownames(out)[(jj-3):jj] <- c("Vote Proportion",
                                "Turnout","Eligible Voters",
                                "Seats per District")
    colnames(out) <- c("Mean","SD","Variance","Min","Max","Valid","Missing")
  } else {
    writeLines(paste("This Judgeit object contains data for",length(object$covars),"elections."))
    writeLines (paste("Lambda = ",object$lambda,sep=""))
    writeLines (paste("Sigma = ",object$sigma,sep=""))
  }
  if (!is.null(out)) return(out)
}

