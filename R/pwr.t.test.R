"pwr.t.test" <-
function (n = NULL, d = NULL, sig.level = 0.05, power = NULL,
    type = c("two.sample", "one.sample", "paired"), alternative = c("two.sided",
        "less","greater"))
{
    if (sum(sapply(list(n, d, power, sig.level), is.null)) !=
        1)
        stop("exactly one of n, d, power, and sig.level must be NULL")
    if (!is.null(d) && is.character(d))
        d <- cohen.ES(test="t",size=d)$effect.size
    if (!is.null(sig.level) && !is.numeric(sig.level) || any(0 >
        sig.level | sig.level > 1))
        stop(sQuote("sig.level"), " must be numeric in [0, 1]")
    if (!is.null(power) && !is.numeric(power) || any(0 > power |
        power > 1))
        stop(sQuote("power"), " must be numeric in [0, 1]")
    type <- match.arg(type)
    alternative <- match.arg(alternative)
    tsample <- switch(type, one.sample = 1, two.sample = 2, paired = 1)
ttside<-switch(alternative, less = 1, two.sided = 2, greater=3)

    tside <- switch(alternative, less = 1, two.sided = 2, greater =1)
    if (tside == 2 && !is.null(d))
        d <- abs(d)
    if (ttside == 1) {
        p.body <- quote({
            nu <- (n - 1) * tsample
            pt(qt(sig.level/tside, nu, lower = TRUE), nu, ncp = sqrt(n/tsample) *
                d, lower = TRUE)
        })
    }
    if (ttside == 2)  {
        p.body <- quote({
            nu <- (n - 1) * tsample
            qu <- qt(sig.level/tside, nu, lower = FALSE)
            pt(qu, nu, ncp = sqrt(n/tsample) * d, lower = FALSE) +
                pt(-qu, nu, ncp = sqrt(n/tsample) * d, lower = TRUE)
        })
    }
	if (ttside == 3) {
        p.body <- quote({
            nu <- (n - 1) * tsample
            pt(qt(sig.level/tside, nu, lower = FALSE), nu, ncp = sqrt(n/tsample) *
                d, lower = FALSE)
        })
    }

    if (is.null(power))
        power <- eval(p.body)
    else if (is.null(n))
        n <- uniroot(function(n) eval(p.body) - power, c(2 +
            1e-10, 1e+09))$root
    else if (is.null(d)) {
 	if(ttside==2){       d <- uniroot(function(d) eval(p.body) - power, c(1e-07,
            10))$root}
if(ttside==1){       d <- uniroot(function(d) eval(p.body) - power, c(-10,
            5))$root}
if(ttside==3){       d <- uniroot(function(d) eval(p.body) - power, c(-5,
            10))$root}

}
    else if (is.null(sig.level))
        sig.level <- uniroot(function(sig.level) eval(p.body) -
            power, c(1e-10, 1 - 1e-10))$root
    else stop("internal error")
    NOTE <- switch(type, paired = "n is number of *pairs*", two.sample = "n is number in *each* group",
        NULL)
    METHOD <- paste(switch(type, one.sample = "One-sample", two.sample = "Two-sample",
        paired = "Paired"), "t test power calculation")
    structure(list(n = n, d = d, sig.level = sig.level, power = power,
        alternative = alternative, note = NOTE, method = METHOD),
        class = "power.htest")
}
