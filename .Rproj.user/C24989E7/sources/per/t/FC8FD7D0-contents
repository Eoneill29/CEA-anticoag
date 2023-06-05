#' Function to simulate a Markov cohort model, based on anticoag 5-state diagram
#' @param n.t is a scalar for time horizon, i.e. number of cycles
#' @param d.c is a scalar for the cycle discount rate for costs
#' @param d.e is a scalar for the cycle discount rate for utility / QALYs
#' @param v.n is a string vector containing labels for the model states
#' @param v.cost is a numeric vector containing cost values for each state
#' @param v.util is a numeric vector containing utility values for each state
#' @param v.M_1 is a numeric vector containing initial population distribution
#' @param l.probr is a list containing parameter values for state transition probabilities
#' @returns a list (r.results) of a trace matrix (m.TRr), total discounted costs (tc_hatr) and utility (te_hatr)

Markov <- function(n.t, d.c, d.e, v.n, v.cost, v.util, v.M_1, l.probr){  
  ##Extract parameter values for state transition probabilities
  pr.ne_ne  <- l.probr$pr.ne_ne
  pr.ne_vr  <- l.probr$pr.ne_vr
  pr.ne_mb  <- l.probr$pr.ne_mb
  pr.ne_nmb <- l.probr$pr.ne_nmb
  pr.mb_ot  <- l.probr$pr.mb_ot
  pr.mb_mb  <- 1 - l.probr$pr.mb_ot
  pr.nmb_ot  <- l.probr$pr.nmb_ot
  pr.nmb_nmb  <- 1 - l.probr$pr.nmb_ot
  
  ##Create transition probability matrix  
  t.probr <- matrix(c(pr.ne_ne, pr.ne_vr, pr.ne_mb, pr.ne_nmb, 0, 
                      0, 1, 0, 0, 0, 
                      0, 0, pr.mb_mb, 0, pr.mb_ot, 
                      0, 0, 0, pr.nmb_nmb, pr.nmb_ot, 
                      0, 0, 0, 0, 1), 
                    nrow = length(v.n), ncol = length(v.n), byrow = T, 
                    dimnames = list (v.n, v.n)) 
  print(t.probr)
  
  ##Create the transition trace matrix 
  m.TRr <- matrix(0, nrow = n.t + 1, ncol = length(v.n), 
                  dimnames = list(paste("cycle", 0:n.t, sep = ""), v.n)) 
  
  ##Indicating the initial health state  
  m.TRr[1, ] <- v.M_1
  
  ##Utility and Cost Vectors 
  v.cost <- v.cost
  v.util <- v.util
  
  ##Simulation run 
  for (i in 2:(n.t +1)){ 
    m.TRr[i,] <-t(m.TRr[i - 1, ]) %*% t.probr} 
  
  tcr <- m.TRr %*% v.cost #cost calculated 
  ter <- m.TRr %*% v.util #QALY calculated 
  
  ##Discounting
  v.dwc <- 1/(1 + d.c) ^ (0:n.t)   
  # calculate the cost discount weight based on the discount rate d.c  
  v.dwe <- 1/(1 + d.e) ^ (0:n.t)    
  # calculate the QALY discount weight based on the discount rate d.e
  tc_hatr <- t(tcr) %*% v.dwc #totaled discounted cost 
  te_hatr <- t(ter) %*% v.dwe #total discounted QALY 
  
  r.results <- list(m.TRr = m.TRr, tc_hatr = tc_hatr, te_hatr = te_hatr) #store results 
  return(r.results) 
}