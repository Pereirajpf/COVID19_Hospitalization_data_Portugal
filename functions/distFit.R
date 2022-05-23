# Function that fits 5 statistical distributions to data, which are: 
#gamma, lognormal, Weibull, gamma adl and lognormal adl

distFit <- function(data, var = "Time.stay", plot_colour = "tomato1", plot_title = "Distribution fitting", ylim = 52){
  #Check if fitdistrplus and ggplot2 is in use
  if (!require(fitdistrplus)) {library(fitdistrplus)}
  if (!require(ggplot2)) {library(ggplot2)}
  
  #Distributions fitting
  fg   <- fitdist(data[[var]], distr = "gamma", method="mle",lower = c(0, 0))
  fln  <- fitdist(data[[var]], distr = "lnorm", method="mle")
  fw   <- fitdist(data[[var]], distr = "weibull", method="mle")
  
  fg_adl  <- fitdist(data[[var]], distr = "gamma", method="mge",gof="ADL")
  fln_adl <- fitdist(data[[var]], distr = "lnorm", method="mge",gof="ADL")
  
  #Distribution parameters
  dist_params <- list(fg_params=fg$estimate, fln_params=fln$estimate, fw_params=fw$estimate, fg_adl_params=fg_adl$estimate, fln_adl_params=fln_adl$estimate)
  # Goodness-of-fit statistics table
  GoF_table <- gofstat(list(fg, fln, fw, fg_adl, fln_adl))
  
  #Plot
  plot <- ggplot() +
    theme_bw() + 
    geom_histogram(data = data, aes(x = .data[[var]], y = ..density..), binwidth = 1,fill=plot_colour,alpha=0.4) +
    #
    stat_function(fun = dgamma, n = 1000, args = list(shape = dist_params$fg_params[[1]], rate = dist_params$fg_params[[2]]), aes(colour="1"),alpha=0.8,size = 0.8) +
    stat_function(fun = dlnorm, n = 1000, args = list(meanlog = dist_params$fln_params[[1]], sdlog = dist_params$fln_params[[2]]), aes(colour="2"),alpha=0.8,size = 0.8) +
    stat_function(fun = dweibull, n = 1000, args = list(shape = dist_params$fw_params[[1]], scale = dist_params$fw_params[[2]]), aes(colour="3"),alpha=0.8,size = 0.8) +
    #
    stat_function(fun = dgamma, n = 1000, args = list(shape = dist_params$fg_adl_params[[1]], rate = dist_params$fg_adl_params[[2]]), aes(colour="4"),alpha=0.8,size = 0.8) +
    stat_function(fun = dlnorm, n = 1000, args = list(meanlog = dist_params$fln_adl_params[[1]], sdlog = dist_params$fln_adl_params[[2]]), aes(colour="6"),alpha=0.8,size = 0.8) +
    #
    labs(x="Length of stay",y="Density",colour = "Distributions",title = plot_title) +
    scale_colour_discrete(labels = c("gamma", "lnormal", "weibull", "gamma adl", "lnormal adl")) +
    #xlim(0,100)
    scale_x_continuous(expand = c(0,0), limits = c(0, ylim)) +
    theme(legend.position = c(0.9, 0.72),
          #text = element_text(size = 15),
          plot.background = element_rect(color = "white"),
          legend.background = element_rect(fill = "transparent"))
  
  # List that containes the fitting results and a table with Goodness-of-fit statistics and criteria
  fit_table <- list(fitdist = list(gamma = fg, lognormal = fln, weibull= fw, gamma_adl = fg_adl, lognormal_adl = fln_adl),
                    dist_params = dist_params,
                    GoF_table = GoF_table,
                    plot = plot)
  
  fit_table
}
