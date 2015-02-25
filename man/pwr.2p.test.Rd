\name{pwr.2p.test}
\alias{pwr.2p.test}

\title{Power calculation for two proportions (same sample sizes)}
\description{
  Compute power of test, or determine parameters to obtain target
     power (similar to power.prop.test).
}
\usage{
pwr.2p.test(h = NULL, n = NULL, sig.level = 0.05, power = NULL, 
    alternative = c("two.sided","less","greater"))
}


\arguments{
  \item{h}{Effect size}
  \item{n}{Number of observations (per sample)}
  \item{sig.level}{Significance level (Type I error probability)}
  \item{power}{Power of test (1 minus Type II error probability)}
  \item{alternative}{a character string specifying the alternative hypothesis,
          must be one of "two.sided" (default), "greater" or
          "less"}
}
\details{
 Exactly one of the parameters 'h','n', 'power' and
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
\seealso{ES.h, pwr.2p2n.test, power.prop.test}
\examples{
## Exercise 6.1 p. 198 from Cohen (1988)
pwr.2p.test(h=0.3,n=80,sig.level=0.05,alternative="greater")
}
\keyword{htest}
