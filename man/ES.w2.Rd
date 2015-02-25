\name{ES.w2}
\alias{ES.w2}
\title{Effect size calculation in the chi-squared test 
for association}
\description{
Compute effect size w for a two-way probability table corresponding to
the alternative hypothesis in the chi-squared test of association in 
two-way contingency tables
}
\usage{
ES.w2(P)
}
\arguments{
  \item{P}{A two-way probability table (alternative hypothesis)}
}

\value{
The corresponding effect size w
}
\references{Cohen, J. (1988). Statistical power analysis for the
	behavioral sciences (2nd ed.). Hillsdale,NJ: Lawrence Erlbaum.}
\author{Stephane CHAMPELY}


\seealso{pwr.chisq.test}

\examples{
prob<-matrix(c(0.225,0.125,0.125,0.125,0.16,0.16,0.04,0.04),nrow=2,byrow=TRUE)
prob
ES.w2(prob)
pwr.chisq.test(w=ES.w2(prob),df=(2-1)*(4-1),N=200)
}
\keyword{htest}
