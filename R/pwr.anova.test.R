"pwr.anova.test" <-
function (k = NULL, n = NULL, f = NULL, sig.level = 0.05, power = NULL) 
{
    if (sum(sapply(list(k, n, f, power, sig.level), is.null)) != 
        1) 
        stop("exactly one of k, n, f, power, and sig.level must be NULL")
    if (!is.null(f) && f < 0) 
        stop("f must be positive")
    if (!is.null(k) && k < 2) 
        stop("number of groups must be at least 2")
    if (!is.null(n) && n < 2) 
        stop("number of observations in each group must be at least 2")
    if (!is.null(sig.level) && !is.numeric(sig.level) || any(0 > 
        sig.level | sig.level > 1)) 
        stop(sQuote("sig.level"), " must be numeric in [0, 1]")
    if (!is.null(power) && !is.numeric(power) || any(0 > power | 
        power > 1)) 
        stop(sQuote("power"), " must be numeric in [0, 1]")
    p.body <- quote({
        lambda <- k * n * f^2
        pf(qf(sig.level, k - 1, (n - 1) * k, lower = FALSE), 
            k - 1, (n - 1) * k, lambda, lower = FALSE)
    })
    if (is.null(power)) 
        power <- eval(p.body)
    else if (is.null(k)) 
        k <- uniroot(function(k) eval(p.body) - power, c(2 + 
            1e-10, 100))$root
    else if (is.null(n)) 
        n <- uniroot(function(n) eval(p.body) - power, c(2 + 
            1e-10, 1e+00))$root
    else if (is.null(f)) 
        f <- uniroot(function(f) eval(p.body) - power, c(1e-07, 
            1e+07))$root
    else if (is.null(sig.level)) 
        sig.level <- uniroot(function(sig.level) eval(p.body) - 
            power, c(1e-10, 1 - 1e-10))$root
    else stop("internal error")
    NOTE <- "n is number in each group"
    METHOD <- "Balanced one-way analysis of variance power calculation"
    structure(list(k = k, n = n, f = f, sig.level = sig.level, 
        power = power, note = NOTE, method = METHOD), class = "power.htest")
}

