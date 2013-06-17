#library(Judgeit)
data(house6311)

#define uncontested for its purpose as a covariate.
unc <- function(inp) -1*(inp<0.05)+1*(inp>0.95)

#even though it's hard-coded into house6311, here it is again.
elecyears <- as.numeric(names(house6311))

#load the object.
j.ob <- judgeit(model.form=VOTE~unc(VOTE)+INC, vote.form=TURNOUT~1,
                    data=house6311, same.districts=(elecyears%%10!=2),
                    use.last.votes=TRUE, subset=DELSOUTH==0)

#get some seats-votes summaries.
j.ob.1 <- bias.resp(j.ob,year=1944)
j.ob.1

bias.resp(j.ob,year=1990)
bias.resp(j.ob,year=1992)

