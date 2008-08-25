\name{v2s}
\alias{v2s}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{v2s - votes to seats}
\description{
Indicates whether a vote share variable is greater than 0.5.}
\usage{
v2s(inp)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{inp}{The vote share variable.}
}
\value{
  1*(inp>0.5)
}
\author{Andrew C. Thomas}
\note{For aid in JudgeIt routines.}
\keyword{array}
