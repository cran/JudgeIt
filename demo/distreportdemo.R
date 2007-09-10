#library(Judgeit)
data(house6311)

#define uncontested for its purpose as a covariate.
unc <- function(inp) -1*(inp<0.05)+1*(inp>0.95)

#even though it's hard-coded into house6311, here it is again.
elecyears <- as.numeric(names(house6311))
same.d <- 1*(elecyears %% 10 != 2)

#load the object.
j.ob <- judgeit(model=VOTE~unc(VOTE)+INC,vote.form=TURNOUT~1,
                    data=house6311,use.last.votes=T,subset=DELSOUTH==0,
                    same.d=same.d)

#what happens if...
plot(judgeit(judgeit.object=j.ob,routine="distreport",year=which(elecyears==1984)))
judgeit(judgeit.object=j.ob,routine="winvote",winvote=0.52,year=which(elecyears==1984))

#...everyone had quit Congress in 1984?
plot(judgeit(judgeit.object=j.ob,routine="distreport",new.covariates=list("INC",0),year=which(elecyears==1984)))
judgeit(judgeit.object=j.ob,routine="winvote",winvote=0.52,new.covariates=list("INC",0),year=which(elecyears==1984))


