#' summarize a psa object across all simulations
#' Source: DARTH (https://github.com/DARTH-git/cohort-modeling-tutorial-intro/)
#'
#' @param object the psa object
#' @param calc_sds whether or not to calculate the standard deviations. Defaults to FALSE
#' @param ... further arguments to summary (not used)
#'
#' @importFrom stats sd
#' @return a \code{data.frame} containing the mean cost and effectiveness for each strategy and, if requested,
#' the standard deviations of the cost and effectiveness for each strategy.
#' @export
summary.psa <- function(object, calc_sds = FALSE, ...) {
  
  mean_cost <- colMeans(object$cost)
  mean_effect <- colMeans(object$effectiveness)
  strat <- object$strategies
  sum_psa <- data.frame("Strategy" = strat,
                        "meanCost" = mean_cost,
                        "meanEffect" = mean_effect,
                        stringsAsFactors = FALSE)
  if (calc_sds) {
    sd_cost <- apply(object$cost, 2, sd)
    sd_effect <- apply(object$effectiveness, 2, sd)
    sum_psa[, "sdCost"] <- sd_cost
    sum_psa[, "sdEffect"] <- sd_effect
  }
  rownames(sum_psa) <- seq_len(nrow(sum_psa))
  
  ## ICER ##
  delta.c <- sum_psa$meanCost[2] - sum_psa$meanCost[1]            # calculate incremental costs between rivaroxaban and warfarin 
  delta.e <- sum_psa$meanEffect[2] - sum_psa$meanEffect[1]            # calculate incremental QALYs between rivaroxaban and warfarin
  ICER <- delta.c / delta.e             # calculate the ICER
  results <- c(delta.c, delta.e, ICER)  # store the values in a new variable
  
  # Create full incremental cost-effectiveness analysis table
  table_markov <- data.frame(
    sum_psa$Strategy,
    round(sum_psa$meanCost, 0),              # costs per arm
    round(sum_psa$meanEffect, 4),              # health outcomes per arm
    c("", round(delta.c, 0)),   # incremental costs
    c("", round(delta.e, 4)),   # incremental QALYs
    c("", round(ICER, 0))       # ICER
  )
  rownames(sum_psa) <- seq_len(nrow(sum_psa))  # name the rows
  colnames(table_markov) = c("Strategies", "Mean Costs", "Mean QALYs","Incremental Costs", "QALYs Gained", "ICER") # name the columns
  table_markov                    # print the table 
}