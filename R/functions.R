# R/functions.R
check_convergence <- function(fit) {
  rhat <- summary(fit)$fixed$Rhat
  neff <- summary(fit)$fixed$n_eff
  div <- sum(nuts_params(fit)$Value[nuts_params(fit)$Parameter == "divergent__"])
  
  passed <- all(rhat < 1.1) && 
    all(neff > 400) && 
    div == 0
  
  list(
    passed = passed,
    max_rhat = max(rhat),
    min_neff = min(neff),
    n_divergent = div
  )
}