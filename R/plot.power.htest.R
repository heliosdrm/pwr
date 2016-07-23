"plot.power.htest" <- function (x, ...){
  # initial checks
  if (class(x) != 
      "power.htest") 
    stop("argument must be of class power.htest")
  pwr.methods <- c("One-sample t test power calculation", "Two-sample t test power calculation", "Paired t test power calculation", "t test power calculation", "Difference of proportion power calculation for binomial distribution (arcsine transformation)", "difference of proportion power calculation for binomial distribution (arcsine transformation)", "Balanced one-way analysis of variance power calculation", "Chi squared power calculation", "Mean power calculation for normal distribution with known variance", "proportion power calculation for binomial distribution (arcsine transformation)", "approximate correlation power calculation (arctangh transformation)")
  if(!(x$method %in% pwr.methods))
    stop(paste("the method ", x$method, " is not supported. Supported methods include:", paste(pwr.methods, collapse = "; ")))
  
  # settings
  breaks <- 20
  
  # case: One-sample, Two-sample or Paired t test
  if(x$method == "One-sample t test power calculation" || x$method == "Two-sample t test power calculation" || x$method == "Paired t test power calculation")
  {
    n <- x$n
    n_upper <- max(n*1.5, n+30) # upper at least 30 above n
    
    # generate data
    sample_sizes <- seq.int(from=10, to=n_upper, by=(n_upper - 10)/breaks)
    data <- data.frame(sample_sizes)
    data$power <- sapply(sample_sizes, FUN = function(ss) {
      return(pwr.t.test(n=ss, d=x$d, sig.level = x$sig.level, type=x$type, alternative = x$alternative)$power)
    }, simplify = TRUE)
    
    # create labels
    title_string <- x$method
    legend_string <- paste("tails =", x$alternative, "\neffect size d =", x$d, "\nalpha =", x$sig.level
    )
    xlab_string <- "sample size"
    ylab_string <- expression(paste("test power = 1 - ", beta))
    optimal_string <- paste("optimal sample size \nn = ", ceiling(n), "\n", x$note, sep = "")
  }
  
  # case: Two-sample t test with n1 and n2
  else if(x$method == "t test power calculation")
  {
    n <- x$n1 + x$n2
    n_upper <- max(n*1.5, n+30) # upper at least 30 above n
    n_rel <- x$n1 / n # relative sample size; will be kept constant in claculations
    
    # generate data
    sample_sizes <- seq.int(from=10, to=n_upper, by=(n_upper - 10)/breaks)
    data <- data.frame(sample_sizes)
    data$power <- sapply(sample_sizes, FUN = function(ss) {
      n1 <- ceiling(ss*n_rel)
      n2 <- ss - n1
      if(n1 <2 || n2 <2){
        return(NA)
      }else{
        return(pwr.t2n.test(n1=n1, n2=n2, d=x$d, sig.level = x$sig.level, alternative = x$alternative)$power)
      }
    }, simplify = TRUE)
    
    # create labels
    title_string <- x$method
    legend_string <- paste("tails =", x$alternative, "\neffect size d =", x$d, "\nalpha =", x$sig.level, "\nn1/n2 = ", round(n_rel, 2))
    xlab_string <- "sample size"
    ylab_string <- expression(paste("test power = 1 - ", beta))
    optimal_string <- paste("optimal sample size \nn = ", x$n1, " + ", x$n2, " = ", n, sep = "")
  }
  
  # case: Difference of proportion (same sample size)
  else if(x$method == "Difference of proportion power calculation for binomial distribution (arcsine transformation)")
  {
    n <- x$n
    n_upper <- max(n*1.5, n+30) # upper at least 30 above n
    
    # generate data
    sample_sizes <- seq.int(from=10, to=n_upper, by=(n_upper - 10)/breaks)
    data <- data.frame(sample_sizes)
    data$power <- sapply(sample_sizes, FUN = function(ss) {
      return(pwr.2p.test(n=ss, h=x$h, sig.level = x$sig.level, alternative = x$alternative)$power)
    }, simplify = TRUE)
    
    # create labels
    title_string <- "Difference of proportion power calculation\nfor binomial distribution (arcsine transformation)"  
    
    legend_string <- paste("tails =", x$alternative, "\neffect size h =", x$h, "\nalpha =", x$sig.level
    )
    xlab_string <- "sample size"
    ylab_string <- expression(paste("test power = 1 - ", beta))
    optimal_string <- paste("optimal sample size \nn = ", ceiling(n), "\n", x$note, sep = "")
  }
  
  # case: difference of proportion (different sample size)
  else if(x$method == "difference of proportion power calculation for binomial distribution (arcsine transformation)")
  {
    n <- x$n1 + x$n2
    n_upper <- max(n*1.5, n+30) # upper at least 30 above n
    n_rel <- x$n1 / n # relative sample size; will be kept constant in claculations
    
    # generate data
    sample_sizes <- seq.int(from=10, to=n_upper, by=(n_upper - 10)/breaks)
    data <- data.frame(sample_sizes)
    data$power <- sapply(sample_sizes, FUN = function(ss) {
      n1 <- ceiling(ss*n_rel)
      n2 <- ss - n1
      if(n1 <2 || n2 <2){
        return(NA)
      }else{
        return(pwr.2p2n.test(n1=n1, n2=n2, h=x$h, sig.level = x$sig.level, alternative = x$alternative)$power)
      }
    }, simplify = TRUE)
    
    # create labels
    title_string <- "Difference of proportion power calculation\nfor binomial distribution (arcsine transformation)"
    legend_string <- paste("tails =", x$alternative, "\neffect size h =", x$h, "\nalpha =", x$sig.level, "\nn1/n2 = ", round(n_rel, 2))
    xlab_string <- "sample size"
    ylab_string <- expression(paste("test power = 1 - ", beta))
    optimal_string <- paste("optimal sample size \nn = ", x$n1, " + ", x$n2, " = ", n, sep = "")
    
  }
  
  
  # case: ANOVA
  else if(x$method == "Balanced one-way analysis of variance power calculation")
  {
    n <- x$n
    n_upper <- max(n*1.5, n+30) # upper at least 30 above n
    
    # generate data
    sample_sizes <- seq.int(from=10, to=n_upper, by=(n_upper - 10)/breaks)
    data <- data.frame(sample_sizes)
    data$power <- sapply(sample_sizes, FUN = function(ss) {
      return(pwr.anova.test(n=ss, k=x$k, f=x$f, sig.level = x$sig.level)$power)
    }, simplify = TRUE)
    
    # create labels
    title_string <- "Balanced one-way analysis of variance \npower calculation"
    legend_string <- paste("groups k =", x$k, "\neffect size f =", x$f, "\nalpha =", x$sig.level
    )
    xlab_string <- "sample size"
    ylab_string <- expression(paste("test power = 1 - ", beta))
    optimal_string <- paste("optimal sample size \nn = ", ceiling(n), "\n", x$note, sep = "")
    
  }
  
  # case: Chi Squared
  else if(x$method == "Chi squared power calculation")
  {
    n <- x$N
    n_upper <- max(n*1.5, n+30) # upper at least 30 above n
    
    # generate data
    sample_sizes <- seq.int(from=10, to=n_upper, by=(n_upper - 10)/breaks)
    data <- data.frame(sample_sizes)
    data$power <- sapply(sample_sizes, FUN = function(ss) {
      return(pwr.chisq.test(N=ss, w=x$w, sig.level = x$sig.level, df=x$df)$power)
    }, simplify = TRUE)
    
    # create labels
    title_string <- x$method
    legend_string <- paste("effect size w =", x$w, "\ndf =", x$df, "\nalpha =", x$sig.level)
    xlab_string <- "sample size"
    ylab_string <- expression(paste("test power = 1 - ", beta))
    optimal_string <- paste("optimal sample size \nN = ", ceiling(n), "\n", x$note, sep = "")
    
  }
  
  
  # case: Normal distribution
  else if(x$method == "Mean power calculation for normal distribution with known variance")
  {
    n <- x$n
    n_upper <- max(n*1.5, n+30) # upper at least 30 above n
    
    # generate data
    sample_sizes <- seq.int(from=10, to=n_upper, by=(n_upper - 10)/breaks)
    data <- data.frame(sample_sizes)
    data$power <- sapply(sample_sizes, FUN = function(ss) {
      return(pwr.norm.test(n=ss, d=x$d, sig.level = x$sig.level, alternative = x$alternative)$power)
    }, simplify = TRUE)
    
    # create labels
    title_string <- "Mean power calculation for normal distribution\nwith known variance"
    legend_string <- paste("tails =", x$alternative, "\neffect size d =", x$d, "\nalpha =", x$sig.level
    )
    xlab_string <- "sample size"
    ylab_string <- expression(paste("test power = 1 - ", beta))
    optimal_string <- paste("optimal sample size \nn = ", ceiling(n), "\n", x$note, sep = "")
    
  }
  
  
  
  # case: proportion
  else if(x$method == "proportion power calculation for binomial distribution (arcsine transformation)")
  {
    n <- x$n
    n_upper <- max(n*1.5, n+30) # upper at least 30 above n
    
    # generate data
    sample_sizes <- seq.int(from=10, to=n_upper, by=(n_upper - 10)/breaks)
    data <- data.frame(sample_sizes)
    data$power <- sapply(sample_sizes, FUN = function(ss) {
      return(pwr.p.test(n=ss, h=x$h, sig.level = x$sig.level, alternative = x$alternative)$power)
    }, simplify = TRUE)
    
    # create labels
    title_string <- "proportion power calculation\nfor binomial distribution (arcsine transformation)"  
    
    legend_string <- paste("tails =", x$alternative, "\neffect size h =", x$h, "\nalpha =", x$sig.level
    )
    xlab_string <- "sample size"
    ylab_string <- expression(paste("test power = 1 - ", beta))
    optimal_string <- paste("optimal sample size \nn = ", ceiling(n), "\n", x$note, sep = "")
    
  }
  
  
  # case: correlation
  else if(x$method == "approximate correlation power calculation (arctangh transformation)")
  {
    n <- x$n
    n_upper <- max(n*1.5, n+30) # upper at least 30 above n
    
    # generate data
    sample_sizes <- seq.int(from=10, to=n_upper, by=(n_upper - 10)/breaks)
    data <- data.frame(sample_sizes)
    data$power <- sapply(sample_sizes, FUN = function(ss) {
      return(pwr.r.test(n=ss, r=x$r, sig.level = x$sig.level, alternative = x$alternative)$power)
    }, simplify = TRUE)
    
    # create labels
    title_string <- "approximate correlation power calculation\n(arctangh transformation)"  
    
    legend_string <- paste("tails =", x$alternative, "\nr =", x$r, "\nalpha =", x$sig.level
    )
    xlab_string <- "sample size"
    ylab_string <- expression(paste("test power = 1 - ", beta))
    optimal_string <- paste("optimal sample size \nn = ", ceiling(n), sep = "")
    
  }
  
  # pass arguments if required
  if(length(dots <- list(...)) && !is.null(dots$xlab)){
    xlab_string <- dots$xlab
  }
  if(length(dots <- list(...)) && !is.null(dots$ylab)){
    ylab_string <- dots$ylab
  }
  if(length(dots <- list(...)) && !is.null(dots$main)){
    title_string <- dots$main
}

  # position of text in plot
  if(x$power < 0.5){
    text_anchor <- 1
    text_vjust <- 1
  }else{
    text_anchor <- 0
    text_vjust <- 0
  }
  if(min(data$power, na.rm = TRUE) < 0.6){
    legend_anchor <- 1
    legend_vjust <- 1
  }else{
    legend_anchor <- 0
    legend_vjust <- 0
  }

  
  # plot
  ggplot(data = data, aes(x=sample_sizes, y=power)) +
    geom_line(colour="red", size=0.1, na.rm = TRUE) +
    geom_point(na.rm = TRUE) +
    geom_vline(xintercept = ceiling(n), linetype=3, size=0.8, colour="darkblue") +
    xlab(xlab_string) +
    ylab(ylab_string) +
    ggtitle(title_string) +
    scale_y_continuous(labels=percent,limits = c(0,1)) +
    annotate("text", 10, legend_anchor, label=legend_string, hjust=0, vjust=legend_vjust, size=3.5) +
    annotate("text", n + ((n_upper-10)/breaks), text_anchor, label=optimal_string, hjust=0, vjust=text_vjust, colour="darkblue", size=3.5)
  
}
