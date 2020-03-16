"pwr.f2.test" <-
function (u = NULL, v = NULL, f2 = NULL, sig.level = 0.05, power = NULL)
{
    if (sum(sapply(list(u,v, f2, power, sig.level), is.null)) !=
        1)
        stop("exactly one of u, v, f2, power, and sig.level must be NULL")
    if (!is.null(f2)){
        if (is.character(f2))
            f2 <- cohen.ES(test="f2",size=f2)$effect.size
        if (any(f2 < 0))
            stop("f2 must be positive")
    }
    if (!is.null(u) && any(u < 1))
        stop("degree of freedom u for numerator must be at least 1")
    if (!is.null(v) && any(v < 1))
        stop("degree of freedom v for denominator must be at least 1")
    if (!is.null(sig.level) && !is.numeric(sig.level) || any(0 >
        sig.level | sig.level > 1))
        stop(sQuote("sig.level"), " must be numeric in [0, 1]")
    if (!is.null(power) && !is.numeric(power) || any(0 > power |
        power > 1))
        stop(sQuote("power"), " must be numeric in [0, 1]")
    p.body <- quote({
        lambda <- f2*(u+v+1)
        pf(qf(sig.level, u, v, lower = FALSE),
            u, v, lambda, lower = FALSE)
    })
    if (is.null(power))
        power <- eval(p.body)
    else if (is.null(u))
        u <- uniroot(function(u) eval(p.body) - power, c(1 + 1e-10, 100))$root
    else if (is.null(v))
        v <- uniroot(function(v) eval(p.body) - power, c(1 + 1e-10, 1e+09))$root
    else if (is.null(f2))
        f2 <- uniroot(function(f2) eval(p.body) - power, c(1e-07, 1e+07))$root
    else if (is.null(sig.level))
        sig.level <- uniroot(function(sig.level) eval(p.body) - power,
            c(1e-10, 1 - 1e-10))$root
    else stop("internal error")
    METHOD <- "Multiple regression power calculation"
    structure(list(u = u, v = v, f2 = f2, sig.level = sig.level,
        power = power, method = METHOD), class = "power.htest")
}
