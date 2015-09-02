"pwr.t2n.test" <-
function (n1 = NULL, n2= NULL, d = NULL, sig.level = 0.05, power = NULL,
 alternative = c("two.sided", 
        "less","greater")) 
{
    if (sum(sapply(list(n1,n2, d, power, sig.level), is.null)) != 
        1) 
        stop("exactly one of n1, n2, d, power, and sig.level must be NULL")
    if (!is.null(sig.level) && !is.numeric(sig.level) || any(0 > 
        sig.level | sig.level > 1)) 
        stop(sQuote("sig.level"), " must be numeric in [0, 1]")
    if (!is.null(power) && !is.numeric(power) || any(0 > power | 
        power > 1)) 
        stop(sQuote("power"), " must be numeric in [0, 1]")
   if (!is.null(n1) && n1 < 2) 
        stop("number of observations in the first group must be at least 2")
     if (!is.null(n2) && n2 < 2) 
        stop("number of observations in the second group must be at least 2")
  
    alternative <- match.arg(alternative)
    tsample <-2
ttside<-switch(alternative, less = 1, two.sided = 2, greater=3)

    tside <- switch(alternative, less = 1, two.sided = 2, greater =1)
    if (tside==2 &&  !is.null(d)) 
        d <- abs(d)
    if (ttside == 1) {
        p.body <- quote({
            nu <- n1+n2-2
            pt(qt(sig.level/tside, nu, lower = TRUE), nu, 
ncp = d*(1/sqrt(1/n1+1/n2)),
 lower = TRUE)
        })
    }
    if (ttside == 2)  {
        p.body <- quote({
           nu <- n1+n2-2
            qu <- qt(sig.level/tside, nu, lower = FALSE)
            pt(qu, nu, ncp = d*(1/sqrt(1/n1+1/n2)), lower = FALSE) + 
                pt(-qu, nu,ncp = d*(1/sqrt(1/n1+1/n2)), lower = TRUE)
        })
    }
	if (ttside == 3) {
        p.body <- quote({
           nu <- n1+n2-2
            pt(qt(sig.level/tside, nu, lower = FALSE), nu, 
ncp = d*(1/sqrt(1/n1+1/n2)), lower = FALSE)
        })
    }

    if (is.null(power)) 
        power <- eval(p.body)
    else if (is.null(n1)) 
        n1 <- uniroot(function(n1) eval(p.body) - power, c(2 + 
            1e-10, 1e+07))$root
  else if (is.null(n2)) 
        n2 <- uniroot(function(n2) eval(p.body) - power, c(2 + 
            1e-10, 1e+07))$root
    else if (is.null(d)) {
 	if(ttside==2){       d <- uniroot(function(d) eval(p.body) - power, 
c(1e-07, 10))$root}
if(ttside==1){       d <- uniroot(function(d) eval(p.body) - power, 
c(-10, 5))$root}
if(ttside==3){       d <- uniroot(function(d) eval(p.body) - power, 
c(-5, 10))$root}

}
    else if (is.null(sig.level)) 
        sig.level <- uniroot(function(sig.level) eval(p.body) - 
            power, c(1e-10, 1 - 1e-10))$root
    else stop("internal error")
      METHOD <- c("t test power calculation")
    structure(list(n1 = n1,n2=n2, d = d, sig.level = sig.level, power = power, 
        alternative = alternative,method = METHOD), 
        class = "power.htest")
}

