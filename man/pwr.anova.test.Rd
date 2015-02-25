\name{pwr.anova.test}
\alias{pwr.anova.test}

\title{Power calculations for balanced one-way analysis of variance tests}
\description{
 Compute power of test or determine parameters to obtain target
     power (same as power.anova.test).}
\usage{
pwr.anova.test(k = NULL, n = NULL, f = NULL, sig.level = 0.05, power = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{k}{Number of groups}
  \item{n}{Number of observations (per group)}
  \item{f}{Effect size}
  \item{sig.level}{Significance level (Type I error probability)}
  \item{power}{Power of test (1 minus Type II error probability)}
}
\details{
 Exactly one of the parameters 'k','n','h','power' and
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
\seealso{power.anova.test}
\examples{
## Exercise 8.1 P. 357 from Cohen (1988) 
pwr.anova.test(f=0.28,k=4,n=20,sig.level=0.05)

## Exercise 8.10 p. 391
pwr.anova.test(f=0.28,k=4,power=0.80,sig.level=0.05)
}
\keyword{htest}