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

judgeit (model.formula=~1,vote.formula=NULL,same.districts=NULL,
          data,pop.groups=NULL,

          uncontesteds.method="default",uncontested.low=0.05,
          uncontested.low.new=0.25,uncontested.high=0.95,
          uncontested.high.new=0.75,

          use.last.votes=TRUE,
          simulations=201, 
          
          weight="constant", 
          years=NULL,
          
          routine=NULL, year=NULL, judgeit.object=NULL,
          verbose=FALSE, ... )

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
  \item{pop.groups}{A formula indicating groups of subpopulations to be
    considered in the voting.power routine.}
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
  \item{simulations}{The number of simulations of election parameters
    for each analysis.}
  \item{weight}{A choice between "constant","elgvotes","actvotes" and
    "seats" for the type of weights used in the linear model of each election.}
  \item{years}{A vector indicating the years considered in the
    analysis. This entry is redundant if the names of the entries in the
    data list are given as the years.}

  \item{routine}{If desired, the routine to run in order to obtain desired
    quantities of interest.}
  \item{year}{If desired, the election for which the above routine will be run.}
  \item{judgeit.object}{If desired, a pre-existing object from which judgeit() will
    obtain the model.}
  \item{verbose}{If TRUE, extra messages will be displayed during execution.}
  
  \item{\dots}{Extra commands to be used with model.frame(), a routine
    used in assembling the linear model for each year.}
}
\value{
  judgeit() loads several years of electoral history into a judgeit
    object for further analysis.

  Alternatively, judgeit() will perform the routine requested in option
  "routine", and the function will return a judgeit object with judgeit.object\$output containing the output from the chosen routine.
}
\references{See JudgeIt website (http://gking.harvard.edu/judgeit) for more information.}
\author{Andrew Gelman, Gary King & Andrew C. Thomas}
\examples{

#Demo files are available through the following commands:
\dontrun{
demo(judgeit.primer)
demo(seatsdemo)
demo(probdemo)
demo(svsumdemo)
demo(distreportdemo)
}

data(house6311)
#columns: STATE,DIST,INC,VOTE,TURNOUT,DELSOUTH

#operators:
unc <- function(inp) -1*(inp<0.05)+1*(inp>0.95)

years <- seq (1896,1992,by=2)
same.dists <- rep(1,49); same.dists[seq(4,49,by=5)] <- 0

j.ob <- judgeit(model.formula=VOTE~unc(VOTE)+INC,vote.formula=TURNOUT~1,
               data=house6311,
               use.last.votes=TRUE, subset=DELSOUTH==0,same.d=same.dists)

summary(j.ob)
summary(j.ob,which(years==1942))

d.rep <- district.report(j.ob,year=1962,new.covariates=list("INC",0),vote.range=c(0.1,0.9))
d.rep

#seats-votes curve
seating <- seats(j.ob,year=1986,vote.range=c(0.2,0.8))
plot(seating)


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{models}
