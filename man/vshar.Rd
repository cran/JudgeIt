\name{vshar}
\alias{vshar}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{vshar - vote share given two vote totals}
\description{
  Given two vote totals (or fractions), outputs the share of the vote
  received by the first party.
}
\usage{
vshar(r1, r2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{r1,r2}{vectors of total votes or fractions.}
}
\value{
  r1/(r1+r2)
}
\author{Andrew C. Thomas}
\keyword{array}
