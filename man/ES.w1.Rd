\name{ES.w1}
\alias{ES.w1}
\title{Effect size calculation in the chi-squared test for goodness of fit}
\description{
Compute effect size w for two sets of k probabilities
P0 (null hypothesis) and P1 (alternative hypothesis)
}
\usage{
ES.w1(P0, P1)
}
\arguments{
  \item{P0}{First set of k probabilities (null hypothesis)}
  \item{P1}{Second set of k probabilities (alternative hypothesis)}
}
\value{
The corresponding effect size w
}
\references{Cohen, J. (1988). Statistical power analysis for the
	behavioral sciences (2nd ed.). Hillsdale,NJ: Lawrence Erlbaum.}
\author{Stephane CHAMPELY}


\seealso{pwr.chisq.test}

\examples{
## Exercise 7.1 p. 249 from Cohen 
P0<-rep(1/4,4)
P1<-c(0.375,rep((1-0.375)/3,3))
ES.w1(P0,P1)
pwr.chisq.test(w=ES.w1(P0,P1),N=100,df=(4-1))
}
\keyword{htest}
