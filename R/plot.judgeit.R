`plot.judgeit` <-
function(x,straight.up=F,year=1,...) {
  judgeit.object <- x
  
  if (length(judgeit.object$outputyear)>0) yrfull <- judgeit.object$years[judgeit.object$outputyear] else
    yrfull <- year

  switch (judgeit.object$outputclass,
          #"none" = {writeLines (paste("Plotting year,",year))
          #          yy <- judgeit.object$voteshare[[year]]
          #          yy <- yy[!is.na(yy)]
          #          kernel.plot(yy)},
          "seats" = {
            plot(c(0,1),c(0,1),ty="n",main=paste("Seats-Votes Plot for",yrfull),
                 xlab="Vote Proportion",ylab="Seat Proportion",...)
            year <- judgeit.object$outputyear
            t.x <- as.numeric(rownames(judgeit.object$output))
            t.y <- judgeit.object$output[,1]
            t.s <- judgeit.object$output[,2]
            lines(t.x,t.y)
            lines(t.x,pmin(t.y+2*t.s,1),col=3)
            lines(t.x,pmax(t.y-2*t.s,0),col=3)
            lines(c(0.5,0.5),c(0,1),col=8)
            lines(c(0,1),c(0.5,0.5),col=8)
            xx <- weighted.mean(c(judgeit.object$voteshare[[year]],judgeit.object$ext[,1]),
                                c(judgeit.object$distweights[[year]],judgeit.object$ext[,4]),na.rm=T)
            yy <- weighted.mean(v2s(c(judgeit.object$voteshare[[year]],judgeit.object$ext[,3])),
                                    c(judgeit.object$seats[[year]],judgeit.object$ext[,4]),na.rm=T)
            points(xx,yy,col=4)
            text(xx,yy,paste("Vote:",signif(xx,4)),pos=4)
            text(xx,yy-0.05,paste("Seats:",signif(yy,4)),pos=4)
          },
          "prob" = {
            plot(c(0,1),c(0,1),ty="n",main=paste("Probability-Votes
Plot for",yrfull),xlab="Vote Proportion",ylab="Seat Proportion with
Indicated Probability",...)
            t.x <- as.numeric(rownames(judgeit.object$output))
            t.y <- judgeit.object$output[,1]
            t.s <- judgeit.object$output[,2]
            lines(t.x,t.y)
            lines(t.x,pmin(t.y+2*t.s,1),col=3)
            lines(t.x,pmax(t.y-2*t.s,0),col=3)
            lines(c(0.5,0.5),c(0,1),col=8)
            lines(c(0,1),c(0.5,0.5),col=8)
          },
          "distreport" = {
            if (colnames(judgeit.object$output)[2]!="Std. Dev.") {
              kernel.plot (judgeit.object$output[,1],judgeit.object$output[,2],
                           xlab="Vote Share",ylab="Likelihood of District Vote",main=paste("District Vote Likelihood Plot for",yrfull),...)
              text(0.9,1.3,"Observed Votes",col=1)
              text(0.9,1.4,"Model Prediction",col=2)
            } else {
              kernel.plot (judgeit.object$output[,1],
                           xlab="Vote Share",ylab="Likelihood of District Vote",main=paste("District Vote Likelihood Plot for",yrfull),...)
              text(0.9,1.4,"Model Prediction",col=1)
            }
          },
          {year <- judgeit.object$outputyear
           writeLines (paste("Outputting a kernel plot of districts in ",yrfull,"."))
           yy <- judgeit.object$voteshare[[year]]; yy <- yy[!is.na(yy)]; kernel.plot(yy)})
}

