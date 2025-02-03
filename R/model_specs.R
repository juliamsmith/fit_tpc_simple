# R/model_specs.R

# Stan functions (your existing stan_funs code here)
stan_funs <- "..." # Your existing Stan functions

get_model_formula <- function(model_type) {
  formulas <- list(
    lrf = bf(rate ~ LRF(temp, Tmin, Topt + Above, Topt, Ropt),
             Tmin ~ 1,
             Topt ~ 1,
             Above ~ 1,
             Ropt ~ 1,
             nl = TRUE),
    deutsch = bf(rate ~ deutsch(temp, rmax, topt, ctmax, a),
                 rmax ~ 1,
                 topt ~ 1,
                 ctmax ~ 1,
                 a ~ 1,
                 nl = TRUE)
    # Add other model formulas here
  )
  return(formulas[[model_type]])
}

get_model_prior <- function(model_type) {
  priors <- list(
    lrf = prior(normal(12, 3), nlpar = "Tmin", lb = 0, ub = 20) +
      prior(normal(40, 3), nlpar = "Topt", lb = 30, ub = 50) +
      prior(normal(50, 3), nlpar = "Tmax", lb = 40, ub = 50) +
      prior(normal(7, 3), nlpar = "Ropt", lb = 0),
    deutsch = prior(normal(7, 3), nlpar = "rmax", lb = 0) +
      prior(normal(40, 3), nlpar = "topt", lb = 30, ub = 50) +
      prior(normal(50, 3), nlpar = "ctmax", lb = 40, ub = 50) +
      prior(normal(7, 3), nlpar = "a", lb = 0),
    ss <- prior(normal(0.74, 0.4), nlpar = "rtref", lb = 0) +  
      prior(normal(0.68, 0.4), nlpar = "e", lb = 0) +  
      prior(normal(1.32, 0.6), nlpar = "el", lb = 0) + 
      prior(normal(19, 6), nlpar = "tl", lb = 0) +  
      prior(normal(3.8, 1), nlpar = "eh", lb = 0) + 
      prior(normal(42, 3), nlpar = "th", lb = 30),
    lactin2 <- prior(normal(0.188, 0.1), nlpar = "a", lb = 0) +  
      prior(normal(0.167, 0.1), nlpar = "b", lb = 0) +  
      prior(normal(45, 3), nlpar = "tmax", lb = 40, ub = 50) +  
      prior(normal(5.31, 2), nlpar = "deltat", lb = 0),  
    rezende <- prior(normal(4, 2), nlpar = "q10", lb = 0) + 
      prior(normal(0.0706, 0.05), nlpar = "a", lb = 0) +  
      prior(normal(2, 1), nlpar = "b") +  
      prior(normal(0.000517, 0.0002), nlpar = "c", lb = 0),
    beta <- prior(normal(4.82, 2), nlpar = "a", lb = 0) +  
      prior(normal(40, 3), nlpar = "b", lb = 30, ub = 50) + 
      prior(normal(117, 30), nlpar = "c", lb = 0) + 
      prior(normal(19.2, 5), nlpar = "d", lb = 1) + 
      prior(normal(2.05, 1), nlpar = "e", lb = 1),  
    gaussian <- prior(normal(7, 3), nlpar = "rmax", lb = 0) + 
      prior(normal(40, 3), nlpar = "topt", lb = 30, ub = 50) +  
      prior(normal(7, 3), nlpar = "a", lb = 0),  
    weibull <- prior(normal(7, 3), nlpar = "a", lb = 0) +  
      prior(normal(40, 3), nlpar = "topt", lb = 30, ub = 50) +  
      prior(normal(357675, 100000), nlpar = "b", lb = 0) +  
      prior(normal(57040, 20000), nlpar = "c", lb = 0)
  )
  return(priors[[model_type]])
}

get_model_settings <- function(model_type) {
  list(
    control = list(adapt_delta = 0.99, max_treedepth = 15),
    iter = 4000,
    warmup = 2000,
    chains = 4,
    cores = 4,
    seed = 123
  )
}