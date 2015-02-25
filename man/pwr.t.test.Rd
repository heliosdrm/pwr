\name{pwr.t.test}
\alias{pwr.t.test}
\title{Power calculations for t-tests of means (one sample, two samples and paired samples)}
\description{
 Compute power of tests or determine parameters to obtain target
     power (similar to power.t.test).}
\usage{
pwr.t.test(n = NULL, d = NULL, sig.level = 0.05, power = NULL, 
    type = c("two.sample", "one.sample", "paired"), alternative = c("two.sided", 
        "less","greater"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{Number of observations (per sample)}
  \item{d}{Effect size}
  \item{sig.level}{Significance level (Type I error probability)}
  \item{power}{Power of test (1 minus Type II error probability)}
  \item{type}{Type of t test : one- two- or paired-samples}
  \item{alternative}{a character string specifying the alternative hypothesis,
          must be one of "two.sided" (default), "greater" or
          "less"}
}
\details{
 Exactly one of the parameters 'd','n','power' and
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
\seealso{power.prop.test}
\examples{

## One sample (power)
## Exercise 2.5 p. 47 from Cohen (1988)
pwr.t.test(d=0.2,n=60,sig.level=0.10,type="one.sample",alternative="two.sided")

## Paired samples (power)
## Exercise p. 50 from Cohen (1988)
d<-8/(16*sqrt(2*(1-0.6)))
pwr.t.test(d=d,n=40,sig.level=0.05,type="paired",alternative="two.sided")

## Two independent samples (power)
## Exercise 2.1 p. 40 from Cohen (1988)
d<-2/2.8
pwr.t.test(d=d,n=30,sig.level=0.05,type="two.sample",alternative="two.sided")

## Two independent samples (sample size)
## Exercise 2.10 p. 59
pwr.t.test(d=0.3,power=0.75,sig.level=0.05,type="two.sample",alternative="greater")

}
\keyword{htest}