\name{cohen.ES}
\alias{cohen.ES}
\title{Conventional effects size}
\description{
Give the conventional effect size (small, medium, large)
for the tests available in this package
}
\usage{
cohen.ES(test = c("p", "t", "r", "anov", "chisq", "f2"), size = c("small", "medium", "large"))
}
\arguments{
  \item{test}{The statistical test of interest}
  \item{size}{The ES : small, medium of large? }
}
\value{
The corresponding effect size
}
\references{Cohen, J. (1988). Statistical power analysis for the
	behavioral sciences (2nd ed.). Hillsdale,NJ: Lawrence Erlbaum.}
\author{Stephane CHAMPELY}

\examples{
## medium effect size for the correlation test
cohen.ES(test="r",size="medium")

## sample size corresponding to a medium size effect in the two-sided correlation test
## using the conventional power of 0.80
pwr.r.test(r=cohen.ES(test="r",size="medium")$effect.size,power=0.80,sig.level=0.05,alternative="two.sided")
}
\keyword{htest}