\name{pwr.p.test}
\alias{pwr.p.test}
\title{Power calculations for proportion tests (one sample)}
\description{
 Compute power of test or determine parameters to obtain target
     power (same as power.anova.test).}
\usage{
pwr.p.test(h = NULL, n = NULL, sig.level = 0.05, power = NULL,alternative = c("two.sided","less","greater"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{h}{Effect size}
  \item{n}{Number of observations}
  \item{sig.level}{Significance level (Type I error probability)}
  \item{power}{Power of test (1 minus Type II error probability)}
  \item{alternative}{a character string specifying the alternative hypothesis,
          must be one of "two.sided" (default), "greater" or
          "less"}}
\details{
These calculations use arcsine transformation of the proportion (see Cohen (1988))

 Exactly one of the parameters 'h','n','power' and
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
\seealso{ES.h}
\examples{

## Exercise 6.5 p. 203 from Cohen 
h<-ES.h(0.5,0.4)
h
pwr.p.test(h=h,n=60,sig.level=0.05,alternative="two.sided")

## Exercise 6.8 p. 208
pwr.p.test(h=0.2,power=0.95,sig.level=0.05,alternative="two.sided")

}
\keyword{htest}