\name{seats}
\alias{seats}
\alias{voting.power}
\alias{prob}
\alias{seats.with.prob}
\alias{results.prob}
\alias{winprob}
\alias{distreport}
\alias{district.report}
\alias{conditional.seats}
\alias{votes.for.result}
\alias{winvote}
\alias{bias.resp}
\alias{svsum}
\alias{chopcollege}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{JudgeIt analysis routines}
\description{
  seats: Given an average vote, determine the distribution of seats
  received in a two-party election system.

  voting.power: Determine the average probability of changing an
  election result given membership in a particular population subgroup.

  seats.with.prob (or prob): Given a probability range, determine the
  fraction of seats whose probability of victory lies within that range.

  results.prob (or winprob): Given a seats range, determine the probability
  that the election outcome lies in that range.

  district.report (or distreport): Output information about the system
  at the district level.

  conditional.seats: Given a vote range, output the fraction of seats
  whose outcomes lie within this range.

  votes.for.result (or winvote): Determine the mean vote required to
  get a given probability of victory/majority for Party 1.

  bias.resp (or svsum): Output the partisan bias and responsiveness of
  the electoral system under consideration.

  chopcollege: Determine the seats-votes curve for the Electoral
  College, conditional on the method for dividing electoral votes
}
\usage{
  seats(judgeit.object,...) #mean.votes=NULL,
  voting.power(judgeit.object,...) #new.pop.groups=NULL,
  seats.with.prob(judgeit.object,...) #prob.range=c(0.5,1),
  results.prob(judgeit.object,...) #seat.range=c(0.5,1),
  district.report(judgeit.object,...)
  conditional.seats(judgeit.object,...) #vote.range=c(0.4,0.6),
  votes.for.result(judgeit.object,...) #prob.win=c(0.1,0.5,0.9),
  bias.resp(judgeit.object, ...)
  chopcollege(judgeit.object, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{judgeit.object}{An election object produced by judgeit().}
  \item{...}{Additional options, including:}
  
  \item{}{year: The year to be analyzed. Defaults to the last year in the system.}
  \item{}{predict: T/F whether prediction, rather than analysis, should be
    executed.}
  \item{}{new.covariates: A list of variable names and their replacement
    values for counterfactual analysis or prediction.}
  \item{}{new.covariate.matrix: A new matrix of covariates for prediction
    or counterfactual analysis. This matrix need not have the same
    number of rows as the old covariate matrix, but must have the
    identical column setup.}
  \item{}{new.seats, new.actual.voters, new.eligible.voters: If a new
    covariate matrix is used, these are vectors of the new seats per
    district, turnout, and eligible voters respectively. (Default: one
    seat per district and identical populations.)}
  \item{}{extra.districts: Extra districts to be added to the system after analysis
    but before reporting, typically uncontested districts whose results
    are unlikely to change. Should be three column matrix, with columns
    representing vote share, turnout/eligible voters and seats per
    district respectively.}
  \item{}{district.select: If a subset of districts is desired for one
    analysis, this is a numerical vector identifying these districts.}
  \item{}{vote.range,prob.range,seat.range: Two-element vectors specifying a
    mean vote range, desired probability range, or fraction of
    seats. Their use depends on the routine called.}
  \item{}{mean.votes: A vector of mean votes to be used as the independent
    variable in analyses.}
  \item{}{shift.in.votes: A vector of shifts in the mean vote from the
    observed value, defining the independent variables in an analysis.}
  \item{}{prob.win: The desired probability/ies of victory for Party 1.}
  \item{}{new.pop.groups: Population groups to be used in the voting.power
    routine. These will override groups specified in the JudgeIt object.}
}
\value{
  An object of a judgeit sub-class.
}
\author{Andrew C. Thomas}
\references{Thomas, Gelman, King, Katz (2012) ``Estimating Partisan Bias
  of the Electoral College Under Proposed Changes in Elector
  Apportionment'', Statistics, Politics and Policy.}
\keyword{array}
