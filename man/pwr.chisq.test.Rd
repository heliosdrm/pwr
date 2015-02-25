\name{pwr.chisq.test}
\alias{pwr.chisq.test}

\title{power calculations for chi-squared tests}
\description{
Compute power of test or determine parameters to obtain target
     power (same as power.anova.test).}
\usage{
pwr.chisq.test(w = NULL, N = NULL, df = NULL, sig.level = 0.05, power = NULL)
}

\arguments{
  \item{w}{Effect size}
  \item{N}{Total number of observations}
  \item{df}{degree of freedom (depends on the chosen test)}
  \item{sig.level}{Significance level (Type I error probability)}
  \item{power}{Power of test (1 minus Type II error probability)}
}
\details{
 Exactly one of the parameters 'w','N','power' and
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
\seealso{ES.w1,ES.w2}
\examples{

## Exercise 7.1 P. 249 from Cohen (1988) 
pwr.chisq.test(w=0.289,df=(4-1),N=100,sig.level=0.05)

## Exercise 7.3 p. 251
pwr.chisq.test(w=0.346,df=(2-1)*(3-1),N=140,sig.level=0.01)

## Exercise 7.8 p. 270
pwr.chisq.test(w=0.1,df=(5-1)*(6-1),power=0.80,sig.level=0.05)
}
\keyword{htest}