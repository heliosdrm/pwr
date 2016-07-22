\name{plot.power.htest}
\alias{plot.power.htest}
\title{Plot diagram of sample size vs. test power}
\description{Plot a diagram to illustrate the relationship of sample size and test power for a given set of parameters.}
\usage{
## S3 method for class 'power.htest'
plot.power.htest(pwr)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pwr}{object of class power.htest usually created by one of the power calculation functions, e.g., pwr.t.test()}
}
\details{
Power calculations for the following tests are supported: t-test (pwr.t.test(), pwr.t2n.test()), chi squared test (pwr.chisq.test()), one-way ANOVA (pwr.anova.test(), standardnormal distribution (pwr.norm.test()), pearson correlation (pwr.r.test()), proportions (pwr.p.test(), pwr.2p.test(), pwr.2p2n.test()))
}
\value{
These functions are invoked for their side effect of drawing on the active graphics device.
}
\author{Stephan Weibelzahl <weibelzahl@pfh.de>}
}
\seealso{pwr.t.test, pwr.p.test, pwr.2p.test, pwr.2p2n.test, pwr.r.test, pwr.r.test, pwr.chisq.test, pwr.anova.test}
\examples{

## Two-sample t-test
p.t.two <- pwr.t.test(d=0.3, power = 0.8, type= "two.sample", alternative = "two.sided")
plot(p.t.two)

}
\keyword{htest}
