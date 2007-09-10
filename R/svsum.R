`svsum` <-
function (judgeit.object,year) {
  judgeit.object$svexpected.value.only <- F; #samebg<-T
  seats <- c(judgeit.object$seatvec,judgeit.object$extra.districts[,3])
  weights <- c(judgeit.object$weightvec,judgeit.object$extra.districts[,4])
  delvbar <- c(NA,0.5)
  #quantities desired in each district: bias and responsiveness for range, center

  intvl <- 45:55/100
  out <- rbind(rep(0,4))
  rsim <- hyp.elects(judgeit.object,year,delvbar,just.mean.sd=T)
  vsim <- rsim$means
#  print(dim(vsim))
  sds <- rsim$sds
#  print(length(sds))
  o1 <- o2 <- o3 <- o4 <- o5 <- NA

  alan <<- vsim
  shore <<- sds

  #New approach: rather than simulate elections, just simulate posterior.
  
  qofi <- sapply(1:dim(vsim)[2],FUN=function(ii) {  #each simulation. 

    #if (ii %% 50 == 0) print(ii)
    tt <- crossadd(vsim[,ii],intvl-0.5)
    tt <- 1-pnorm(0.5,tt,sds)
    tt <-
      rbind(tt,array(1*(judgeit.object$extra.districts[,1]>0.5),c(length(judgeit.object$extra.districts[,1]),11) ) )
        
    sts <- apply(tt,2,weighted.mean,seats)
                
#    print(c(dim(tt),length(seats)))
      
    #midpoint bias
    o1 <- 2*sts[6]-1
          
    #spread bias
    o2 <- mean(sts[11:7]+sts[1:5]-1)
          
    #responsiveness
    o3 <- (sts[11]-sts[1])/(intvl[11]-intvl[1])

    mv <- weighted.mean(judgeit.object$mu,judgeit.object$weightvec)
    intvl2 <- mv+c(-0.01,0.01)    
    
    tt <- crossadd(vsim[,ii],intvl2-0.5)
    tt <- 1-pnorm(0.5,tt,sds)
    tt <-
      rbind(tt,array(1*(judgeit.object$extra.districts[,1]>0.5),c(length(judgeit.object$extra.districts[,1]),2) ) )
        
    sts <- apply(tt,2,weighted.mean,seats)

    o4 <- (sts[2]-sts[1])/(intvl2[2]-intvl2[1])
    
    return(c(o1,o2,o3,o4))

  })

#  print(summary(qofi))
#  print(dim(qofi))
#  piper <<- qofi
  vs <- apply(qofi,1,var)
#  covie <- apply(qofi[1:4,],1,cov,qofi[5,])
#  cond.vs <- vs[1:4]-covie^2/vs[5]
  out <- cbind(apply(qofi,1,mean)[1:4],sqrt(vs))
 # print(dim(out))
  
  colnames(out) <- c("Mean","SD")
  rownames(out) <- c("Partisan Bias (0.5)","Partisan Bias (0.45-0.55)",
                    "Responsiveness (0.45-0.55)","Responsiveness (observed)")
  return(signif(out,4))
}

