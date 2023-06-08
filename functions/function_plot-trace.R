#' Function to plot cohort trace per strategy
#' @param n.t is a scalar for time horizon, i.e. number of cycles
#' @param v.n is a string vector containing labels for the model states
#' @param m.TRr is the cohort trace matrix
#' @returns a ggplot of cohort distribution across states over cycle

plot_trace <- function(n.t, v.n, m.TRr) {
  df_M      <- data.frame(Cycle = 0:n.t, m.TRr)
  df_M_long <- tidyr::gather(df_M, key = `Health State`, value, 2:ncol(df_M))
  df_M_long$`Health State` <- factor(df_M_long$`Health State`, levels = v.n)
  gg_trace <- ggplot(df_M_long, aes(x = Cycle, y = value, 
                                    color = `Health State`, linetype = `Health State`)) +
    geom_line(linewidth = 1) +
    xlab("Cycle") +
    ylab("Proportion of the cohort") +
    ylim(0,1) +
    scale_x_continuous() + 
    theme_bw(base_size = 14) +
    theme(legend.position  = "bottom", 
          legend.background = element_rect(fill = NA)) 
  
  return(gg_trace) 
}