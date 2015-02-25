"pwr.chisq.test" <-
function (w = NULL, N = NULL, df = NULL, sig.level = 0.05, power = NULL) 
{
    if (sum(sapply(list(w, N, df, power, sig.level), is.null)) != 
        1) 
        stop("exactly one of w, N, df, power, and sig.level must be NULL")
    if (!is.null(w) && w < 0) 
        stop("w must be positive")
    if (!is.null(N) && N < 1) 
        stop("number of observations must be at least 1")
    if (!is.null(sig.level) && !is.numeric(sig.level) || any(0 > 
        sig.level | sig.level > 1)) 
        stop(sQuote("sig.level"), " must be numeric in [0, 1]")
    if (!is.null(power) && !is.numeric(power) || any(0 > power | 
        power > 1)) 
        stop(sQuote("power"), " must be numeric in [0, 1]")
    p.body <- quote({
        k <- qchisq(sig.level, df = df, lower = FALSE)
        pchisq(k, df = df, ncp = N * w^2, lower = FALSE)
    })
    if (is.null(power)) 
        power <- eval(p.body)
    else if (is.null(w)) 
        w <- uniroot(function(w) eval(p.body) - power, c(1e-10, 
            1e+05))$root
    else if (is.null(N)) 
        N <- uniroot(function(N) eval(p.body) - power, c(1 + 
            1e-10, 1e+05))$root
    else if (is.null(sig.level)) 
        sig.level <- uniroot(function(sig.level) eval(p.body) - 
            power, c(1e-10, 1 - 1e-10))$root
    else stop("internal error")
    METHOD <- "Chi squared power calculation"
    NOTE <- "N is the number of observations"
    structure(list(w = w, N = N, df = df, sig.level = sig.level, 
        power = power, method = METHOD, note = NOTE), class = "power.htest")
}

