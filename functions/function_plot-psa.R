#' Plot the psa object
#'
#' Source: DARTH (https://github.com/DARTH-git/cohort-modeling-tutorial-intro/)
#' @param x the psa object
#' @param center plot the mean cost and effectiveness for each strategy. defaults to TRUE
#' @param ellipse plot an ellipse around each strategy. defaults to TRUE
#' @param alpha opacity of the scatterplot points.
#' 0 is completely transparent, 1 is completely opaque
#' @inheritParams add_common_aes
#'
#' @importFrom ellipse ellipse
#' @import dplyr
#' @import ggplot2
#' @importFrom scales dollar_format
#' @return A \code{ggplot2} plot of the PSA, showing the distribution of each PSA sample and strategy
#' on the cost-effectiveness plane.
#' @importFrom tidyr pivot_longer
#' @export
plot.psa <- function(x,
                     center = TRUE, ellipse = TRUE,
                     alpha = 0.2, txtsize = 12, col = c("full", "bw"),
                     n_x_ticks = 6, n_y_ticks = 6,
                     xbreaks = NULL,
                     ybreaks = NULL,
                     xlim = NULL,
                     ylim = NULL,
                     ...) {
  
  effectiveness <- x$effectiveness
  cost <- x$cost
  strategies <- x$strategies
  currency <- x$currency
  
  # expect that effectiveness and costs have strategy column names
  # removes confusing 'No id variables; using all as measure variables'
  df_cost <- suppressMessages(
    pivot_longer(cost,
                 everything(),
                 names_to = "Strategy",
                 values_to = "Cost")
  )
  df_effect <- suppressMessages(
    pivot_longer(effectiveness,
                 cols = everything(),
                 names_to = "Strategy",
                 values_to = "Effectiveness")
  )
  ce_df <- data.frame("Strategy" = df_cost$Strategy,
                      "Cost" = df_cost$Cost,
                      "Effectiveness" = df_effect$Effectiveness)
  
  # make strategies in psa object into ordered factors
  ce_df$Strategy <- factor(ce_df$Strategy, levels = strategies, ordered = TRUE)
  
  psa_plot <- ggplot(ce_df, aes_string(x = "Effectiveness", y = "Cost", color = "Strategy")) +
    geom_point(size = 0.7, alpha = alpha, shape = 21) +
    ylab(paste("Cost (", currency, ")", sep = ""))
  
  # define strategy-specific means for the center of the ellipse
  if (center) {
    strat_means <- ce_df %>%
      group_by(.data$Strategy) %>%
      summarize(Cost.mean = mean(.data$Cost),
                Eff.mean = mean(.data$Effectiveness))
    # make strategies in psa object into ordered factors
    strat_means$Strategy <- factor(strat_means$Strategy, levels = strategies, ordered = TRUE)
    psa_plot <- psa_plot +
      geom_point(data = strat_means,
                 aes_string(x = "Eff.mean", y = "Cost.mean", fill = "Strategy"),
                 size = 8, shape = 21, color = "black")
  }
  
  if (ellipse) {
    # make points for ellipse plotting
    df_list_ell <- lapply(strategies, function(s) {
      strat_specific_df <- ce_df[ce_df$Strategy == s, ]
      els <-  with(strat_specific_df,
                   ellipse::ellipse(cor(Effectiveness, Cost),
                                    scale = c(sd(Effectiveness), sd(Cost)),
                                    centre = c(mean(Effectiveness), mean(Cost))))
      data.frame(els, group = s, stringsAsFactors = FALSE)
    })
    df_ell <- bind_rows(df_list_ell)
    # draw ellipse lines
    psa_plot <- psa_plot + geom_path(data = df_ell,
                                     aes_string(x = "x", y = "y", colour = "group"),
                                     size = 1, linetype = 2, alpha = 1)
  }
  
  # # add common theme
  # col <- match.arg(col)
  # add_common_aes(psa_plot, txtsize, col = col, col_aes = c("color", "fill"),
  #                continuous = c("x", "y"),
  #                n_x_ticks = n_x_ticks, n_y_ticks = n_y_ticks,
  #                xbreaks = xbreaks, ybreaks = ybreaks,
  #                xlim = xlim, ylim = ylim)
}