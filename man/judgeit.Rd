\name{judgeit}
\alias{judgeit}
\alias{JudgeIt}
%- Also NEED an '\alias' for EACH other topic documented here.
%This one is more up to date by coding, etc.
\title{judgeit}
\description{
Two-party election modelling and simulation for R.
}
\usage{

judgeit (model.formula=~1,vote.formula=NULL,same.districts=NULL,data,
                    uncontesteds.method="default",uncontested.low=0.05,uncontested.low.new=0.25,uncontested.high=0.95,uncontested.high.new=0.75,
                    use.last.votes=T,
                    simulations=1000, 

                    weight="constant", 
                    
                    routine=NULL,year=NULL,judgeit.object=NULL,
                    predict=FALSE,
                    new.covariates=NULL,  
                    new.covariate.matrix=NULL,  
                    new.seats=NULL,
                    new.actual.voters=NULL,
                    new.eligible.voters=NULL,
          
                    extra.districts=NULL, 
                    expected.value.only=F,
                    distselect=NULL,
                    vote.range=NULL, 

                    mean.votes=NULL,
                    shift.in.votes=NULL,
                    winvote=0.5,
          
                    probability.range=c(0.5,1),
                    voting.groups=NULL,
                    all.groups=NULL, state.group = NULL,
                    disaggregate = NULL,
                    ...    #for the model.frame work.
                   )

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model.formula}{The model formula; left side contains the vote
    outcome as one party's share of the total vote, right side contains predictors.}
  \item{vote.formula}{The vote distribution formula. Left side contains one
    or two columns with actual and/or eligible turnout, right side
    contains the number of seats per district. ~~Describe \code{vote.formula} here~~ }
  \item{same.districts}{A vector noting whether an election had the same
  district map as the previous one.}
  \item{data}{A list containing all the elections to be modelled in
    the system. Each item of the list should be an election variable;
    each election variable should be a data frame containing the
    information for each district.}
  \item{uncontesteds.method}{A choice between "nochange","default","impute"
    and "delete" for the way to deal with uncontested districts.}
  \item{uncontested.low}{The value below which a district should be
    considered uncontested by party 1, so that party 2 wins the
    district's seats.}
  \item{uncontested.low.new}{If method is "default", districts uncontested by
    party 1 will be considered to have this amount of the vote share for the
    purposes of the analysis.}
  \item{uncontested.high}{The value above which a district should be
    considered uncontested by party 2.}
  \item{uncontested.high.new}{If method is "default", districts uncontested by
    party 2 will be considered to have this amount of the vote share for the
    purposes of the analysis.}
  \item{use.last.votes}{T/F whether a previous election's votes should
    be automatically included as a current election's predictor. This is
    overridden if the district maps are not identical due to the
    information in 'same.districts', or an unequal number of districts between two elections.}
  \item{simulations}{The number of elections to be simulated for
    analysis.}
  \item{weight}{A choice between "constant","elgvotes","actvotes" and
    "seats" for the type of weights used in the linear model of each election.}
  \item{routine}{The routine to run in order to obtain desired
    quantities of interest.}
  \item{year}{The election for which the routine will be run.}
  \item{judgeit.object}{A pre-existing object from which judgeit() will
    obtain the model.}
  \item{predict}{T/F whether prediction, rather than analysis, should be
  executed.}
  \item{new.covariates}{A list of variable names and their replacement
    values for counterfactual analysis or prediction.}
  \item{new.covariate.matrix}{A new matrix of covariates for prediction
    or counterfactual analysis. This matrix need not have the same
    number of rows as the old covariate matrix, but must have the
    identical column setup.}
  \item{new.seats, new.actual.voters, new.eligible.voters}{If a new
    covariate matrix is used, these are vectors of the new seats per
    district, turnout, and eligible voters respectively. (Default: one
    seat per district and identical populations.)}
  \item{extra.districts}{Extra districts to be added to the system after analysis
  but before reporting, typically uncontested districts whose results
  are unlikely to change. Should be three column matrix, with columns
  representing vote share, turnout/eligible voters and seats per
  district respectively.}
  \item{expected.value.only}{T/F whether judgeit() should report errors in expected values of
    quantities rather than in the quantities themselves.}
  \item{distselect}{If a subset of districts is desired for one
    analysis, this is a numerical vector identifying these districts.}
  \item{vote.range,probability.range}{Two-element vectors specifying a
    mean vote range or desired probability range. This depends on the
    routine called.}
  \item{mean.votes}{A vector of mean votes to be used as the independent
  variable in analyses.}
  \item{shift.in.votes}{A vector of shifts in the mean vote from the
    observed value, defining the independent variables in an analysis.}
  \item{winvote}{The desired fraction of seats Party 1 ought to win the
    election, to be used in the winvote routine to determine the
    required mean vote.}
  \item{voting.groups}{In the voting.power routine, this is a matrix of
    turnout data for several groups in each district.}
  \item{all.groups}{In the voting.power routine, this is a matrix of
    eligible voter data for several groups in each district.}
  \item{state.group}{For the ``chopcollege'' routine, indicates the
    membership of each electoral district in a state.}
  \item{disaggregate}{Which state's electors are chosen by congressional
  district in the ``chopcollege'' routine.}
  \item{\dots}{Extra commands to be used with model.frame(), a routine
    used in assembling the linear model for each year.}
}
\value{
  judgeit() always returns an object of class judgeit.

  If no pre-existing object is given (i.e. judgeit.object=NULL) judgeit()
  will attempt to use the model formula and data provided to assemble
  a judgeit object.

  Otherwise, judgeit() will perform the routine requested in option
  "routine", and the function will return a judgeit object with
  judgeit.object\$output containing the output from the chosen routine.
}
\references{See JudgeIt website (http://gking.harvard.edu/judgeit) for more information.}
\author{Andrew Gelman, Gary King & Andrew C. Thomas}
\examples{

#Demo files are available through the following commands:
demo(seatsdemo)
demo(probdemo)
demo(svsumdemo)
demo(distreportdemo)

data(house6311)
#columns: STATE,DIST,INC,VOTE,TURNOUT,DELSOUTH

#operators:
unc <- function(inp) -1*(inp<0.05)+1*(inp>0.95)

years <- seq (1896,1992,by=2)
same.dists <- rep(1,49); same.dists[seq(4,49,by=5)] <- 0

j.ob <- judgeit(model.formula=VOTE~unc(VOTE)+INC,vote.formula=TURNOUT~1,
               data=house6311,
               use.last.votes=T,subset=DELSOUTH==0,same.d=same.dists)

summary(j.ob)
summary(j.ob,which(years==1942))

j.ob <- judgeit(routine="distreport",judgeit.object=j.ob,year=which(years==1962),new.covariates=list("INC",0),vote.range=c(0.1,0.9))
j.ob

#seats-votes curve
j.ob <- judgeit(routine="seats",jud=j.ob,year=which(years==1986),vote.range=c(0.2,0.8))
plot(j.ob)


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{models}
