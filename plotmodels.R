plot_models <- function(modelnames, coefs = NULL, coeflabels, modellabels) {
  
  plot_data_wide <- rbindlist(lapply(models, function(m) {
    
<<<<<<< HEAD
    mname <- deparse(substitute(m))
    
<<<<<<< HEAD
    return(rbind(data.table(stat = "coef",
                            model = mname,
                            t(coefficients(m))),
                 data.table(stat = "sd",
                            model = mname,
                            t(sqrt(diag(vcov(m)))))))
  }), fill = T, use.names = T)
=======
   return(rbind(data.table(stat = "coef",
                     model = m,
                     t(get(m)$coefficients)),
          data.table(stat = "sd",
                     model = m,
                     t(sqrt(diag(vcov(get(m)))))), fill = T, use.names = T))
    }), fill = T, use.names = T)
>>>>>>> parent of 76ec628... Update plotmodels.R
=======
   return(rbind(data.table(stat = "coef",
                     model = mname,
                     t(m$coefficients)),
          data.table(stat = "sd",
                     model = mname,
                     t(sqrt(diag(vcov(m))))), fill = T, use.names = T))
    }), fill = T, use.names = T)
>>>>>>> parent of 659bdf8... Update plotmodels.R
  
  plot_data_long <- dcast(melt(plot_data_wide, id.vars = c("model", "stat")), model + variable ~ stat)
  
  if (is.null(coefs)) coefs <- plot_data_long$coef
  
  # keep only data to be plotted
  plot_data <- plot_data_long[ variable %in% coefs]
  
  # make sure 0 is always included in the plot
  plot_limits <- copy(plot_data)
  plot_limits[ , `:=`(limit_max = max(coef, na.rm = T), limit_min = min(coef, na.rm = T))]
  limit_max <- plot_limits[ limit_max == coef, coef + 1.96*sd]
  limit_min <- plot_limits[ limit_min == coef, coef - 1.96*sd]
  plot_limits <- ifelse(c(limit_min < 0, limit_max < 0), c(limit_min, 0),
                        ifelse(c(limit_min > 0, limit_max > 0), c(0, limit_max),
                               c(limit_min, limit_max)))

  # dodge the posotion of the points to be plotted
  dodge <- position_dodge(width=0.5)
  
  # the actual plot
  ggplot(plot_data, aes(y = coef, x = variable, color = model)) +
    geom_point(position = dodge, size = 3) +
    geom_errorbar(aes(ymin = coef - 1.96*sd, ymax = coef + 1.96*sd), width = 0, position = dodge, size = 1.5) +
    geom_hline(aes(yintercept = 0), color = "darkgrey", size = 1.5) +
    coord_flip(ylim = plot_limits) +
    theme(legend.position = "top") +
    scale_color_viridis_d(breaks = models, labels = modellabels) +
    labs(x = NULL, color = NULL, y = "estimate") +
    scale_x_discrete(breaks = coefs, labels = coeflabels)
    
}
