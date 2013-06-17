data(house6311)

unc <- function(inp) -1*(inp<0.05)+1*(inp>0.95)

#even though it's hard-coded into house6311, here it is again.
elecyears <- as.numeric(names(house6311))
same.d <- 1*(elecyears %% 10 != 2)

#load the object.
j.ob <- judgeit(model=VOTE~unc(VOTE)+INC, vote.form=TURNOUT~1,
                    data=house6311, use.last.votes=TRUE, subset=DELSOUTH==0,
                    same.d=same.d)

#routines.
SV <- svsum(j.ob,year=26)  #year code, or...
SV <- bias.resp(j.ob,year=1932)  #calendar year
SV
plot(SV)

act.sat <- seats(j.ob,year=1948)
head(act.sat)
plot(act.sat)
plot(act.sat,xlim=c(0.4,0.6),ylim=c(0.4,0.6))

biggie <- winvote(j.ob,prob.win=1:19/20,year=1992)
head(biggie)
plot(biggie)
plot(seats(j.ob,year=1992)) #for comparison...
biggie.2 <- votes.for.result(j.ob,prob.win=1:19/20,year=1982)
biggie.2

smack <- conditional.seats(j.ob,vote.range=c(0.5,1),year=46)
head(smack)
plot(smack)

disty <- district.report(j.ob,year=1982)
head(disty)
plot(disty)

mack <- results.prob(j.ob,year=1972)
head(mack)
plot(mack)
head(winprob(j.ob,year=1974))

close.ones <- seats.with.prob(j.ob,prob.range=c(0.4,0.6),year=1992)
head(close.ones)
plot(close.ones)

#cook up some fake groups for testing.
last.year <- which(j.ob$years==1992)
ll <- length(j.ob$voteshare[[last.year]])
pop.groups <- array(rpois(3*ll,10000),c(ll,3)); colnames(pop.groups) <- c("aa","bb","cc")
man.power <- voting.power(j.ob,new.pop.groups=pop.groups,year=1992)
man.power


