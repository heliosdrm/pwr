"plot.power.htest" <- function (pwr){
  # initial checks
  if (class(pwr) != 
      "power.htest") 
    stop("argument must be of class power.htest")
  pwr.methods <- c("One-sample t test power calculation", "Two-sample t test power calculation", "Paired t test power calculation", "t test power calculation", "Difference of proportion power calculation for binomial distribution (arcsine transformation)", "difference of proportion power calculation for binomial distribution (arcsine transformation)", "Balanced one-way analysis of variance power calculation", "Chi squared power calculation", "Mean power calculation for normal distribution with known variance", "proportion power calculation for binomial distribution (arcsine transformation)", "approximate correlation power calculation (arctangh transformation)")
  if(!(pwr$method %in% pwr.methods))
    stop(paste("the method ", pwr$method, " is not supported. Supported methods include:", paste(pwr.methods, collapse = "; ")))
  
  # settings
  breaks <- 20
  
  # case: One-sample, Two-sample or Paired t test
  if(pwr$method == "One-sample t test power calculation" || pwr$method == "Two-sample t test power calculation" || pwr$method == "Paired t test power calculation")
  {
    n <- pwr$n
    n_upper <- max(n*1.5, n+30) # upper at least 30 above n
    
    # generate data
    sample_sizes <- seq.int(from=10, to=n_upper, by=(n_upper - 10)/breaks)
    data <- data.frame(sample_sizes)
    data$power <- sapply(sample_sizes, FUN = function(x) {
      return(pwr.t.test(n=x, d=pwr$d, sig.level = pwr$sig.level, type=pwr$type, alternative = pwr$alternative)$power)
    }, simplify = TRUE)
    
    # create labels
    title_string <- pwr$method
    legend_string <- paste("tails =", pwr$alternative, "\neffect size d =", pwr$d, "\nalpha =", pwr$sig.level
    )
    xlab_string <- "sample size"
    ylab_string <- expression(paste("test power = 1 - ", beta))
    optimal_string <- paste("optimal sample size \nn = ", ceiling(n), "\n", pwr$note, sep = "")
  }
  
  # case: Two-sample t test with n1 and n2
  else if(pwr$method == "t test power calculation")
  {
    n <- pwr$n1 + pwr$n2
    n_upper <- max(n*1.5, n+30) # upper at least 30 above n
    n_rel <- pwr$n1 / n # relative sample size; will be kept constant in claculations
    
    # generate data
    sample_sizes <- seq.int(from=10, to=n_upper, by=(n_upper - 10)/breaks)
    data <- data.frame(sample_sizes)
    data$power <- sapply(sample_sizes, FUN = function(x) {
      n1 <- ceiling(x*n_rel)
      n2 <- x - n1
      if(n1 <2 || n2 <2){
        return(NA)
      }else{
        return(pwr.t2n.test(n1=n1, n2=n2, d=pwr$d, sig.level = pwr$sig.level, alternative = pwr$alternative)$power)
      }
    }, simplify = TRUE)
    
    # create labels
    title_string <- pwr$method
    legend_string <- paste("tails =", pwr$alternative, "\neffect size d =", pwr$d, "\nalpha =", pwr$sig.level, "\nn1/n2 = ", round(n_rel, 2))
    xlab_string <- "sample size"
    ylab_string <- expression(paste("test power = 1 - ", beta))
    optimal_string <- paste("optimal sample size \nn = ", pwr$n1, " + ", pwr$n2, " = ", n, sep = "")
  }
  
  # case: Difference of proportion (same sample size)
  else if(pwr$method == "Difference of proportion power calculation for binomial distribution (arcsine transformation)")
  {
    n <- pwr$n
    n_upper <- max(n*1.5, n+30) # upper at least 30 above n
    
    # generate data
    sample_sizes <- seq.int(from=10, to=n_upper, by=(n_upper - 10)/breaks)
    data <- data.frame(sample_sizes)
    data$power <- sapply(sample_sizes, FUN = function(x) {
      return(pwr.2p.test(n=x, h=pwr$h, sig.level = pwr$sig.level, alternative = pwr$alternative)$power)
    }, simplify = TRUE)
    
    # create labels
    title_string <- "Difference of proportion power calculation\nfor binomial distribution (arcsine transformation)"  
    
    legend_string <- paste("tails =", pwr$alternative, "\neffect size h =", pwr$h, "\nalpha =", pwr$sig.level
    )
    xlab_string <- "sample size"
    ylab_string <- expression(paste("test power = 1 - ", beta))
    optimal_string <- paste("optimal sample size \nn = ", ceiling(n), "\n", pwr$note, sep = "")
  }
  
  # case: difference of proportion (different sample size)
  else if(pwr$method == "difference of proportion power calculation for binomial distribution (arcsine transformation)")
  {
    n <- pwr$n1 + pwr$n2
    n_upper <- max(n*1.5, n+30) # upper at least 30 above n
    n_rel <- pwr$n1 / n # relative sample size; will be kept constant in claculations
    
    # generate data
    sample_sizes <- seq.int(from=10, to=n_upper, by=(n_upper - 10)/breaks)
    data <- data.frame(sample_sizes)
    data$power <- sapply(sample_sizes, FUN = function(x) {
      n1 <- ceiling(x*n_rel)
      n2 <- x - n1
      if(n1 <2 || n2 <2){
        return(NA)
      }else{
        return(pwr.2p2n.test(n1=n1, n2=n2, h=pwr$h, sig.level = pwr$sig.level, alternative = pwr$alternative)$power)
      }
    }, simplify = TRUE)
    
    # create labels
    title_string <- "Difference of proportion power calculation\nfor binomial distribution (arcsine transformation)"
    legend_string <- paste("tails =", pwr$alternative, "\neffect size h =", pwr$h, "\nalpha =", pwr$sig.level, "\nn1/n2 = ", round(n_rel, 2))
    xlab_string <- "sample size"
    ylab_string <- expression(paste("test power = 1 - ", beta))
    optimal_string <- paste("optimal sample size \nn = ", pwr$n1, " + ", pwr$n2, " = ", n, sep = "")
    
  }
  
  
  # case: ANOVA
  else if(pwr$method == "Balanced one-way analysis of variance power calculation")
  {
    n <- pwr$n
    n_upper <- max(n*1.5, n+30) # upper at least 30 above n
    
    # generate data
    sample_sizes <- seq.int(from=10, to=n_upper, by=(n_upper - 10)/breaks)
    data <- data.frame(sample_sizes)
    data$power <- sapply(sample_sizes, FUN = function(x) {
      return(pwr.anova.test(n=x, k=pwr$k, f=pwr$f, sig.level = pwr$sig.level)$power)
    }, simplify = TRUE)
    
    # create labels
    title_string <- "Balanced one-way analysis of variance \npower calculation"
    legend_string <- paste("groups k =", pwr$k, "\neffect size f =", pwr$f, "\nalpha =", pwr$sig.level
    )
    xlab_string <- "sample size"
    ylab_string <- expression(paste("test power = 1 - ", beta))
    optimal_string <- paste("optimal sample size \nn = ", ceiling(n), "\n", pwr$note, sep = "")
    
  }
  
  # case: Chi Squared
  else if(pwr$method == "Chi squared power calculation")
  {
    n <- pwr$N
    n_upper <- max(n*1.5, n+30) # upper at least 30 above n
    
    # generate data
    sample_sizes <- seq.int(from=10, to=n_upper, by=(n_upper - 10)/breaks)
    data <- data.frame(sample_sizes)
    data$power <- sapply(sample_sizes, FUN = function(x) {
      return(pwr.chisq.test(N=x, w=pwr$w, sig.level = pwr$sig.level, df=pwr$df)$power)
    }, simplify = TRUE)
    
    # create labels
    title_string <- pwr$method
    legend_string <- paste("effect size w =", pwr$w, "\ndf =", pwr$df, "\nalpha =", pwr$sig.level)
    xlab_string <- "sample size"
    ylab_string <- expression(paste("test power = 1 - ", beta))
    optimal_string <- paste("optimal sample size \nN = ", ceiling(n), "\n", pwr$note, sep = "")
    
  }
  
  
  # case: Normal distribution
  else if(pwr$method == "Mean power calculation for normal distribution with known variance")
  {
    n <- pwr$n
    n_upper <- max(n*1.5, n+30) # upper at least 30 above n
    
    # generate data
    sample_sizes <- seq.int(from=10, to=n_upper, by=(n_upper - 10)/breaks)
    data <- data.frame(sample_sizes)
    data$power <- sapply(sample_sizes, FUN = function(x) {
      return(pwr.norm.test(n=x, d=pwr$d, sig.level = pwr$sig.level, alternative = pwr$alternative)$power)
    }, simplify = TRUE)
    
    # create labels
    title_string <- "Mean power calculation for normal distribution\nwith known variance"
    legend_string <- paste("tails =", pwr$alternative, "\neffect size d =", pwr$d, "\nalpha =", pwr$sig.level
    )
    xlab_string <- "sample size"
    ylab_string <- expression(paste("test power = 1 - ", beta))
    optimal_string <- paste("optimal sample size \nn = ", ceiling(n), "\n", pwr$note, sep = "")
    
  }
  
  
  
  # case: proportion
  else if(pwr$method == "proportion power calculation for binomial distribution (arcsine transformation)")
  {
    n <- pwr$n
    n_upper <- max(n*1.5, n+30) # upper at least 30 above n
    
    # generate data
    sample_sizes <- seq.int(from=10, to=n_upper, by=(n_upper - 10)/breaks)
    data <- data.frame(sample_sizes)
    data$power <- sapply(sample_sizes, FUN = function(x) {
      return(pwr.p.test(n=x, h=pwr$h, sig.level = pwr$sig.level, alternative = pwr$alternative)$power)
    }, simplify = TRUE)
    
    # create labels
    title_string <- "proportion power calculation\nfor binomial distribution (arcsine transformation)"  
    
    legend_string <- paste("tails =", pwr$alternative, "\neffect size h =", pwr$h, "\nalpha =", pwr$sig.level
    )
    xlab_string <- "sample size"
    ylab_string <- expression(paste("test power = 1 - ", beta))
    optimal_string <- paste("optimal sample size \nn = ", ceiling(n), "\n", pwr$note, sep = "")
    
  }
  
  
  # case: correlation
  else if(pwr$method == "approximate correlation power calculation (arctangh transformation)")
  {
    n <- pwr$n
    n_upper <- max(n*1.5, n+30) # upper at least 30 above n
    
    # generate data
    sample_sizes <- seq.int(from=10, to=n_upper, by=(n_upper - 10)/breaks)
    data <- data.frame(sample_sizes)
    data$power <- sapply(sample_sizes, FUN = function(x) {
      return(pwr.r.test(n=x, r=pwr$r, sig.level = pwr$sig.level, alternative = pwr$alternative)$power)
    }, simplify = TRUE)
    
    # create labels
    title_string <- "approximate correlation power calculation\n(arctangh transformation)"  
    
    legend_string <- paste("tails =", pwr$alternative, "\nr =", pwr$r, "\nalpha =", pwr$sig.level
    )
    xlab_string <- "sample size"
    ylab_string <- expression(paste("test power = 1 - ", beta))
    optimal_string <- paste("optimal sample size \nn = ", ceiling(n), sep = "")
    
  }
  
  
  # position of text in plot
  if(pwr$power < 0.5){
    text_high <- TRUE
  }else{
    text_high <- FALSE
  }
  if(text_high == TRUE){
    text_anchor <- 1
    text_vjust <- 1
  }else{
    text_anchor <- 0
    text_vjust <- 0
  }
  if(min(data$power, na.rm = TRUE) < 0.5){
    legend_high <- TRUE
  }else{
    legend_high <- FALSE
  }
  if(legend_high == TRUE){
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
