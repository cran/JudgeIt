`judgeit.setdata` <-
function (covars=NULL,voteshare=NULL,turnout=NULL,eligible.voters=NULL,seats=NULL,
  same.districts=NULL,
    use.last.votes=T,uncontesteds.method="impute",uncontested.low=0.05,uncontested.low.new=0.10,
      uncontested.high=0.95,uncontested.high.new=0.90,simulations=102,weight="constant") {

  flag <- F

  out <- list(covars=covars,voteshare=voteshare,turnout=turnout)

  if (is.null(same.districts)) {
    quant <- 0
    if (length(voteshare)>1) for (ii in 2:length(voteshare)) quant <- c(quant,1*(length(voteshare[[ii]])==length(voteshare[[ii-1]])))
    same.districts <- quant
  }
  
  if (is.null(turnout)) {
    writeLines ("Number of actual voters unknown, so let's say 404 per district.")
    turnout <- new.list(length(covars))
    for (ii in 1:length(covars)) turnout[[ii]] <- rep(404,dim(covars[[ii]])[1])
    out$turnout <- turnout
  }

  if (is.null(eligible.voters)) {
    writeLines ("The total number of eligible voters was not given. Eligible voters set to actual voters.")
    out$eligible.voters <- turnout
    eligible.voters <- turnout
  } else out$eligible.voters <- eligible.voters

  
  for (ii in 1:length(covars)) {
    if (is.null(covars[[ii]])|all(is.na(covars[[ii]]))) {
      writeLines (paste("No valid covariates given for year ",ii,". Substituting constant row only.",sep=""))
      covars[[ii]] <- as.matrix(rep(1,length(voteshare[[ii]])))
    }
    nas <- NULL
    for (k in 1:dim(covars[[ii]])[2]) {
      if (all(is.na(covars[[ii]][,k]))) nas <- c(nas,k)
    }
    if (!is.null(nas)) covars[[ii]] <- as.matrix(covars[[ii]][,-nas])
    out$covars <- covars
  }

  if (is.null(voteshare)) stop ("Can't do an analysis without results.")

  for (ii in 1:length(voteshare)) {
    ct <- voteshare[[ii]]
    ct <- ct[!is.na(ct)]
    if (any(trunc01(ct)!=ct)) stop (paste("Some districts have vote proportions outside the [0,1] range, in particular year",ii,"."))
  }

  if (is.null(seats)) {
    writeLines ("The total number of seats per district was not given, and is therefore assumed to be 1 in all elections.")
    onus <- function (vs) 1*(vs>=0)
    seats <- lapply (voteshare,onus)
    out$seats <- seats
  } else out$seats <- seats

  possible.uncs <- c("impute","default","remove","nochange")
  if (!any(uncontesteds.method==possible.uncs)) {
    writeLines ("Your choice for removing uncontesteds does not match an existing option. Assuming no change.")
    uncontesteds.method <- "nochange"
  }
  
  #Now all five variables are in place. Put them in matrix form. Missing values stay for now.
  out$fullrow <- new.list(length(out$covars))
  for (ii in 1:length(out$voteshare)) {
    caddy <- cbind (out$voteshare[[ii]],out$covars[[ii]],out$eligible.voters[[ii]],out$turnout[[ii]],out$seats[[ii]])
    out$fullrow[[ii]] <- as.numeric(missed(caddy))

    out$voteshare[[ii]] <- as.matrix(out$voteshare[[ii]])
    out$covars[[ii]] <- as.matrix(out$covars[[ii]])
    out$eligible.voters[[ii]] <- as.matrix(out$eligible.voters[[ii]])
    out$turnout[[ii]] <- as.matrix(out$turnout[[ii]])
    out$seats[[ii]] <- as.matrix(out$seats[[ii]])
  }

  
  out$uncL <- uncontested.low    #maximum for Repub. uncontested
  out$uncLR <- uncontested.low.new   #new level of Rep. uncontested
  out$uncU <- uncontested.high    #min. for Dem uncontested
  out$uncUR <- uncontested.high.new   #new level of Dem uncontested

  out$svexpected.value.only <- F    #Do we want the expected value (1) or the total variation (0)?
  out$sims <- simulations  #default number of simulations to run within one block.
  out$simd <- simulations  #Number of blocks.
  
  out$covarsnew <- new.list(length(covars))
  out$same.dists <- same.districts
  out$outputclass <- "none"
  out$outputyear <- NULL
  
  #now, diagnose the state of the system.
  if ((!flag)&(length(covars)!=length(voteshare))) {
    stop("Error: Covariates and votes don't have the same number of years.")
    out <- NULL
  } else
  if ((length(voteshare)!=length(turnout))|(length(voteshare)!=length(seats))|(length(voteshare)!=length(eligible.voters))) {
    stop("Error: Votes and weights don't have the same number of years.")
    out <- NULL
  } else for (ii in 1:length(covars)) {
      if (dim(covars[[ii]])[1]!=length(voteshare[[ii]])) {
        stop(paste("Error: Covariates and votes in data set",ii,"do not have the same number of districts. ",dim(covars[[ii]])[1],length(voteshare[[ii]])))
        out <- NULL
      }  
      if ((length(voteshare[[ii]])!=length(turnout[[ii]]))|(length(voteshare[[ii]])!=length(seats[[ii]]))|(length(voteshare[[ii]])!=length(eligible.voters[[ii]]))) {
        stop(paste("Error: Votes and weights in data set",ii,"do not have the same number of districts."))
        out <- NULL
	}
    } 
 
#OK. If all is well, out isn't NULL. So now we look at imputation, replacement or whatever.
#options: impute, remove, default, nochange

  if (flag) {
    out <- NULL
    stop("There was an error loading the data.")
  } else {
    class(out) <- "judgeit"
    writeLines ("Election data has been loaded into specified JudgeIt object.")
  }

  if (is.null(out)) stop() else {
    out <- fix.uncontested (out,same.districts,uncontesteds.method)
    if ((use.last.votes)&(length(out$covars)>1)) for (ii in 2:length(out$covars)) 
      if (same.districts[[ii]]) {out$covars[[ii]] <- unique.columns(cbind(out$covars[[ii]],out$voteshare[[ii-1]])); colnames(out$covars[[ii]])[dim(out$covars[[ii]])[2]] <- "lastvote"}

    for (ii in 1:length(out$voteshare)) {
      caddy <- cbind (out$voteshare[[ii]],out$covars[[ii]],out$eligible.voters[[ii]],out$turnout[[ii]],out$seats[[ii]])
      out$fullrow[[ii]] <- as.numeric(missed(caddy))
      out$covars[[ii]] <- unique.columns(out$covars[[ii]],keepers=out$fullrow[[ii]])
    }
  }
  return(out)
}

