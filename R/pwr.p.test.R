"pwr.p.test" <-
function (h = NULL, n = NULL, sig.level = 0.05, power = NULL,
    alternative = c("two.sided","less","greater"))
{
    if (sum(sapply(list(h, n, power, sig.level), is.null)) !=
        1)
        stop("exactly one of h, n, power, and sig.level must be NULL")
    if (!is.null(n) && any(n < 1))
        stop("number of observations in each group must be at least 1")
    if (!is.null(sig.level) && !is.numeric(sig.level) || any(0 >
        sig.level | sig.level > 1))
        stop(sQuote("sig.level"), " must be numeric in [0, 1]")
    if (!is.null(power) && !is.numeric(power) || any(0 > power |
        power > 1))
        stop(sQuote("power"), " must be numeric in [0, 1]")
    alternative <- match.arg(alternative)
    tside <- switch(alternative, less = 1, two.sided = 2, greater=3)
    if (tside == 2 && !is.null(h)) 
        h <- abs(h)
    if (tside == 2) {
        p.body <- quote({
            pnorm(qnorm(sig.level/2, lower = FALSE) - h * sqrt(n),
                lower = FALSE) + pnorm(qnorm(sig.level/2, lower = TRUE) -
                h * sqrt(n), lower = TRUE)
        })
    }
    if (tside == 3) {
        p.body <- quote({
            pnorm(qnorm(sig.level, lower = FALSE) - h * sqrt(n),
                lower = FALSE)
        })
    }

if (tside == 1) {
        p.body <- quote({
            pnorm(qnorm(sig.level, lower = TRUE) - h * sqrt(n),
                lower = TRUE)
        })
    }

    if (is.null(power))
        power <- eval(p.body)
    else if (is.null(h)){
 	if(tside==2){       h <- uniroot(function(h) eval(p.body) - power,
					c(1e-10,10))$root}
	if(tside==1){       h <- uniroot(function(h) eval(p.body) - power,
					c(-10,5))$root}
	if(tside==3){       h <- uniroot(function(h) eval(p.body) - power,
					c(-5,10))$root}
}
    else if (is.null(n))
        n <- uniroot(function(n) eval(p.body) - power, c(2 +
            1e-10, 1e+09))$root
    else if (is.null(sig.level))
        sig.level <- uniroot(function(sig.level) eval(p.body) -
            power, c(1e-10, 1 - 1e-10))$root
    else stop("internal error")
    METHOD <- "proportion power calculation for binomial distribution (arcsine transformation)"
    structure(list(h = h, n = n, sig.level = sig.level, power = power,
        alternative = alternative, method = METHOD), class = "power.htest")
}
