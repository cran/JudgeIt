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

#what happens if...
reports <- district.report(j.ob,year=1984)
plot(reports)

winning <- votes.for.result(j.ob,prob.win=0.52,year=1984)
head(winning)

#...everyone had quit Congress in 1984?
reports.2 <- district.report(j.ob,year=1984,new.covariates=list("INC",0))
head(reports.2)

winning.2 <- winvote(j.ob,prob.win=0.52,year=1984,new.covariates=list("INC",0))
head(winning.2)


