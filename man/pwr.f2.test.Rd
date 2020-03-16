\name{pwr.f2.test}
\alias{pwr.f2.test}
\title{Power calculations for the general linear model}
\description{
 Compute power of test or determine parameters to obtain target
     power (same as power.anova.test).}
\usage{
pwr.f2.test(u = NULL, v = NULL, f2 = NULL, sig.level = 0.05, power = NULL)
}
\arguments{
  \item{u}{degrees of freedom for numerator}
  \item{v}{degrees of freedom for denominator}
  \item{f2}{effect size}
  \item{sig.level}{Significance level (Type I error probability)}
  \item{power}{Power of test (1 minus Type II error probability)}
}
\details{
 Exactly one of the parameters 'u','v','f2','power' and
     'sig.level' must be passed as NULL, and that parameter is
     determined from the others. Notice that the last one has non-NULL
     default so NULL must be explicitly passed if you want to compute
     it.
}
\value{
   Object of class '"power.htest"', a list of the arguments
     (including the computed one) augmented with 'method' and 'note'
     elements.
}
\references{Cohen, J. (1988). Statistical power analysis for the
	behavioral sciences (2nd ed.). Hillsdale,NJ: Lawrence Erlbaum.}
\author{Stephane Champely <champely@univ-lyon1.fr> but this is a mere copy of Peter Dalgaard work (power.t.test)}
\note{
'uniroot' is used to solve power equation for unknowns, so you may
     see errors from it, notably about inability to bracket the root
     when invalid arguments are given.
}
\examples{

## Exercise 9.1 P. 424 from Cohen (1988)
pwr.f2.test(u=5,v=89,f2=0.1/(1-0.1),sig.level=0.05)
}
\keyword{htest}
