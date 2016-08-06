\name{plot.power.htest}
\alias{plot.power.htest}
\title{Plot diagram of sample size vs. test power}
\description{Plot a diagram to illustrate the relationship of sample size and test power for a given set of parameters.}
\usage{
\method{plot}{power.htest}(x, \dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{object of class power.htest usually created by one of the power calculation functions, e.g., pwr.t.test()}
  \item{\dots}{Arguments to be passed to \code{ggplot} including xlab and ylab}
}
\details{
Power calculations for the following tests are supported: t-test (pwr.t.test(), pwr.t2n.test()), chi squared test (pwr.chisq.test()), one-way ANOVA (pwr.anova.test(), standardnormal distribution (pwr.norm.test()), pearson correlation (pwr.r.test()), proportions (pwr.p.test(), pwr.2p.test(), pwr.2p2n.test()))

}
\value{
These functions are invoked for their side effect of drawing on the active graphics device.
}
\note{
By default it attempts to use the plotting tools of \href{https://cran.r-project.org/package=ggplot2/}{ggplot2} and \href{https://cran.r-project.org/package=scales}{scales}.  If they are not installed, it will use the basic R plotting tools.
}
\author{Stephan Weibelzahl <weibelzahl@pfh.de>}
\seealso{
  \code{\link{pwr.t.test}}
  \code{\link{pwr.p.test}}
  \code{\link{pwr.2p.test}}
  \code{\link{pwr.2p2n.test}}
  \code{\link{pwr.r.test}}
  \code{\link{pwr.chisq.test}}
  \code{\link{pwr.anova.test}}
  \code{\link{pwr.t2n.test}}
}
\examples{

## Two-sample t-test
p.t.two <- pwr.t.test(d=0.3, power = 0.8, type= "two.sample", alternative = "two.sided")
plot(p.t.two)
plot(p.t.two, xlab="sample size per group")

}
\keyword{htest}
