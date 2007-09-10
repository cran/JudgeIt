#library(Judgeit)
data(house6311)

#define uncontested for its purpose as a covariate.
unc <- function(inp) -1*(inp<0.05)+1*(inp>0.95)
#zero <- function(a) 0*a

#even though it's hard-coded into house6311, here it is again.
elecyears <- as.numeric(names(house6311))

#load the object.
j.ob <- judgeit(model.form=VOTE~unc(VOTE)+INC,vote.form=TURNOUT~1,
                    data=house6311,same.districts=(elecyears%%10!=2),
                    use.last.votes=T,subset=DELSOUTH==0)

#run some seats routines.
j.ob.1 <- judgeit(judgeit.object=j.ob,routine="seats",mean.votes=seq(0.3,0.7,by=0.01),year=which(elecyears==1904))
plot(j.ob.1)
#plot(j.ob.1,filename="1904house")
