\name{StateLeg}
\docType{data}
\alias{StateLeg}
\alias{StateSummary}
\title{Elections of Legislators in Selected States, 1998-2006}
\description{
This data set contains the biennial elections of state legislators for each
election between 1998 and 2006 for states with single-member districts.}
\usage{StateLeg}
\format{
  StateLeg: A list that contains 25 lists corresponding to each state
  in the data set; each sub-list contains five matrices that list the names of the 
  Republican and Democratic candidates, their total votes in the election, and
  an indicator for the incumbency status of the candidates (1 for Democratic incumbent,
  -1 for Republican, and 0 where neither candidate has an incumbency advantage.)

  StateSummary: A matrix of summary values for each state under consideration, including 
  JudgeIt-derived quantities.
  \enumerate{
    \item full.name, abbrev: The name of the state.
    \item commish: Whether the 2001 redistricting was performed by a commission (as opposed to the legislature.)
    \item X2000pop, X1990pop, growth: State populations in 1990 and 2000, and the corresponding growth rate.
    \item seats: The number of seats in the legislative body.
    \item legsal: The natural logarithm of the annual salary for a state's legislator.
    \item meansal: The natural logarithm of the mean annual salary for a resident of the state.
    \item consper: The natural logarithm of the number of constituents per legislator.
    \item house, sen, gov: Political control of the state legislature/assembly, senate and governorship in 2001: -1 for Republican, 0 for a split/independent, 1 for Democrat.
    \item sumcontrol, unified: The sum of the house-sen-gov indicators, and whether or not all were controlled by the same party.
    \item pctwhite: Fraction of the state whose race is white/Caucasian.
    \item secfive: Whether preclearance of an electoral plan was required under Section 5 of the Voting Rights Act.
    \item Tlever: Whether the members of the legislature were ever subject to term limits during the period in question.
    \item avg00vote: The average share of the district vote won by the Democratic party in the 2000 election.
    \item pbe, pba, res5, reso: Refers to the exact partisan bias, averaged partisan bias, split-vote-responsiveness and observed responsiveness for the desired year, according to JudgeIt analysis.
    \item m, s: The mean or standard deviation of the estimate for
    these quantities.
  }

  
	
}
\references{
	Andrew C. Thomas (2011). ``Avoiding an Electoral Lost Decade: What Lessons Will We Learn in the 2011 Redistricting Cycle?''. http://papers.ssrn.com/sol3/papers.cfm?abstract_id=1757313
}
\keyword{datasets}
