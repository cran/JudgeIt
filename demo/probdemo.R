#library(Judgeit)
data(house6311)

#define uncontested for its purpose as a covariate.
unc <- function(inp) -1*(inp<0.05)+1*(inp>0.95)

#even though it's hard-coded into house6311, here it is again.
elecyears <- as.numeric(names(house6311))
same.d <- 1*(elecyears %% 10 != 2)

#load the object.
j.ob <- judgeit(model=VOTE~unc(VOTE)+INC, vote.form=TURNOUT~1,
                    data=house6311, use.last.votes=TRUE, subset=DELSOUTH==0,
                    same.d=same.d)

#run some "prob" routines.
#What fraction of seats were favored to go Democratic?
j.ob.1 <- seats.with.prob(j.ob,prob.range=c(0.5,1),mean.votes=45:55/100,year=1946)
plot(j.ob.1)

#What fraction of seats were tossups?
j.ob.2 <- seats.with.prob(j.ob,prob.range=c(0.4,0.6),mean.votes=45:55/100,year=1946)
plot(j.ob.2)
