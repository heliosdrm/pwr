\name{pwr.t2n.test}
\alias{pwr.t2n.test}
\title{Power calculations for two samples (different sizes) t-tests of means }
\description{
 Compute power of tests or determine parameters to obtain target
     power (similar to as power.t.test).}
\usage{
pwr.t2n.test(n1 = NULL, n2= NULL, d = NULL, sig.level = 0.05, power = NULL,
 alternative = c("two.sided", 
        "less","greater"))}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n1}{Number of observations in the first sample}
  \item{n2}{Number of observations in the second sample}
  \item{d}{Effect size}
  \item{sig.level}{Significance level (Type I error probability)}
  \item{power}{Power of test (1 minus Type II error probability)}
  \item{alternative}{a character string specifying the alternative hypothesis,
          must be one of "two.sided" (default), "greater" or
          "less"}
}
\details{
 Exactly one of the parameters 'd','n1','n2','power' and
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

## Exercise 2.3 p. 437 from Cohen (1988)
pwr.t2n.test(d=0.6,n1=90,n2=60,alternative="greater")
}
\keyword{htest}