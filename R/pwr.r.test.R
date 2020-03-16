"pwr.r.test" <-
function (n = NULL, r = NULL, sig.level = 0.05, power = NULL,
    alternative = c("two.sided", "less","greater"))
{
    if (sum(sapply(list(n, r, power, sig.level), is.null)) !=
        1)
        stop("exactly one of n, r, power, and sig.level must be NULL")
    if (!is.null(r) && is.character(r))
        r <- cohen.ES(test="r",size=r)$effect.size
    if (!is.null(sig.level) && !is.numeric(sig.level) || any(0 >
        sig.level | sig.level > 1))
        stop(sQuote("sig.level"), " must be numeric in [0, 1]")
    if (!is.null(power) && !is.numeric(power) || any(0 > power |
        power > 1))
        stop(sQuote("power"), " must be numeric in [0, 1]")
    if (!is.null(n) && any(n < 4))
        stop("number of observations must be at least 4")
    alternative <- match.arg(alternative)
    tside <- switch(alternative, less = 1, two.sided = 2,greater=3)
    if (tside == 2 && !is.null(r))
        r <- abs(r)
    if (tside == 3) {
        p.body <- quote({
            ttt <- qt(sig.level, df = n - 2, lower = FALSE)
            rc <- sqrt(ttt^2/(ttt^2 + n - 2))
            zr <- atanh(r) + r/(2 * (n - 1))
            zrc <- atanh(rc) # + rc/(2 * (n - 1))
            pnorm((zr - zrc) * sqrt(n - 3))
        })
    }
    if (tside == 1) {
        p.body <- quote({
            r<--r
            ttt <- qt(sig.level, df = n - 2, lower = FALSE)
            rc <- sqrt(ttt^2/(ttt^2 + n - 2))
            zr <- atanh(r) + r/(2 * (n - 1))
            zrc <- atanh(rc) # + rc/(2 * (n - 1))
            pnorm((zr - zrc) * sqrt(n - 3))
        })
    }

    if (tside == 2) {
        p.body <- quote({
            ttt <- qt(sig.level/2, df = n - 2, lower = FALSE)
            rc <- sqrt(ttt^2/(ttt^2 + n - 2))
            zr <- atanh(r) + r/(2 * (n - 1))
            zrc <- atanh(rc) # + rc/(2 * (n - 1))
            pnorm((zr - zrc) * sqrt(n - 3)) + pnorm((-zr - zrc) *
                sqrt(n - 3))
        })
    }
    if (is.null(power))
        power <- eval(p.body)
    else if (is.null(n))
        n <- uniroot(function(n) eval(p.body) - power, c(4 +
            1e-10, 1e+09))$root
    else if (is.null(r)){
        if(tside==2)
            r <- uniroot(function(r) eval(p.body) - power,
                c(1e-10,1 - 1e-10))$root
        else
            r <- uniroot(function(r) eval(p.body) - power,
                c(-1+1e-10, 1 - 1e-10))$root
    }
    else if (is.null(sig.level))
        sig.level <- uniroot(function(sig.level) eval(p.body) - power,
            c(1e-10, 1 - 1e-10))$root
    else stop("internal error")
    METHOD <- "approximate correlation power calculation (arctangh transformation)"
    structure(list(n = n, r = r, sig.level = sig.level, power = power,
        alternative = alternative, method = METHOD), class = "power.htest")
}
