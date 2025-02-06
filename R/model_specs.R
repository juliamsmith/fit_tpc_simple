# R/model_specs.R

# Stan functions
stan_funs <- "
// flexTPC function
real flexTPC(real temp, real Tmin, real Tmax, real rmax, real alpha, real beta) {
  real result;
  real s;
  
  // Bound parameters to ensure stability
  real Tmin_safe = fmin(fmax(Tmin, -10), 20);
  real Tmax_safe = fmin(fmax(Tmax, 30), 60);
  real rmax_safe = fmax(rmax, 1e-6);
  real alpha_safe = fmin(fmax(alpha, 1e-6), 1 - 1e-6);
  real beta_safe = fmax(beta, 1e-6);
  
  s = alpha_safe * (1 - alpha_safe) / square(beta_safe);
  
  if(temp >= Tmax_safe || temp <= Tmin_safe) {
    return 1e-6;
  }
  
  result = rmax_safe * exp(s * (alpha_safe * log(fmax(temp - Tmin_safe, 1e-6) / alpha_safe) + 
                               (1 - alpha_safe) * log(fmax(Tmax_safe - temp, 1e-6) / (1 - alpha_safe)) - 
                               log(Tmax_safe - Tmin_safe)));
  
  return fmax(result, 1e-6);
}

// LRF function
real LRF(real temp, real Tmin, real Tmax, real Topt, real Ropt) {
  real devrate;
  if(temp > Tmax || temp < Tmin || Topt - Tmin < Tmax - Topt || Tmin >= Topt || Tmax <= Topt) {
    devrate = 1e-10;
  }
  else {
    devrate = ((Ropt * ((Tmax + 273.15) - (temp + 273.15)) * ((temp + 273.15) - (Tmin + 273.15)) ^ 2) / 
              (((Tmin + 273.15) - (Topt + 273.15)) * ( - (temp + 273.15) * (Tmin + 273.15) + 
              3 * (temp + 273.15) * (Topt + 273.15) - 2 * (Topt + 273.15) ^ 2 + 
              (Tmax + 273.15) * ( - 2 * (temp + 273.15) + (Tmin + 273.15) + (Topt + 273.15)))));
  }
  return devrate;
}

// Deutsch function
real deutsch(real temp, real rmax, real topt, real ctmax, real a) {
  real est;
  if (rmax <= 0 || a <= 0 || topt >= ctmax || temp > ctmax) {
    return 1e-10;
  }
  
  if (temp < topt) {
    est = rmax * exp(-((temp - topt)/(2 * a))^2);
  } else {
    est = rmax * (1 - ((temp - topt)/(topt - ctmax))^2);
  }
  
  if (est <= 0) {
    return 1e-10;
  }
  return est;
}

// Sharpe-Schoolfield function
real sharpe_schoolfield(real temp, real rtref, real e, real el, real tl, real eh, real th) {
  real k;
  real tref;
  real boltzmann_term;
  real inactivation_term;
  
  if (rtref <= 0 || e <= 0 || el <= 0 || eh <= 0 || tl <= 0 || th <= 0 || th <= tl) {
    return 1e-10;
  }
  
  k = 8.62e-5;
  tref = 273.15 + 20;
  
  boltzmann_term = rtref * exp(e/k * (1/tref - 1/(temp + 273.15)));
  inactivation_term = 1/(1 + exp(-el/k * (1/(tl + 273.15) - 1/(temp + 273.15))) 
                          + exp(eh/k * (1/(th + 273.15) - 1/(temp + 273.15))));
  
  real result = boltzmann_term * inactivation_term;
  if (result <= 0) {
    return 1e-10;
  }
  return result;
}

// Lactin2 function
real lactin2(real temp, real a, real b, real tmax, real deltat) {
  real est;
  
  if (a <= 0 || b <= 0 || tmax <= temp || deltat <= 0) {
    return 1e-10;
  }
  
  est = exp(a * temp) - exp(a * tmax - (tmax - temp) / deltat) + b;
  
  if (est <= 0) {
    return 1e-10;
  }
  return est;
}

// Rezende function
real rezende(real temp, real q10, real a, real b, real c) {
  real est;
  real tref;
  
  if (q10 <= 0 || temp <= 0) {
    return 1e-10;
  }
  
  tref = 20.0;  // Reference temperature
  est = (a * pow(q10, (temp - tref)/10.0)) / (1 + exp(b - c * temp));
  
  if (est <= 0) {
    return 1e-10;
  }
  return est;
}
// Beta TPC function
real beta_tpc(real temp, real a, real b, real c, real d, real e) {
  real part1;
  real part2;
  real part3;
  real part4;
  real result;
  real norm;
  
  // Return small positive value if parameters invalid
  if (a <= 0 || c <= 0 || d <= 1 || e <= 1) {
    return 1e-6;
  }
  
  part1 = a * pow(fmax(0.0, (temp - b + ((c * (d - 1))/(d + e - 2)))/c), d - 1);
  part2 = pow(fmax(0.0, 1 - ((temp - b + ((c * (d - 1))/(d + e - 2)))/c)), e - 1);
  part3 = pow((d - 1)/(d + e - 2), d - 1);
  part4 = pow((e - 1)/(d + e - 2), e - 1);
  
  result = part1 * part2;
  norm = part3 * part4;
  
  // Return small positive value if result invalid
  if (result <= 0 || norm <= 0 || is_nan(result) || is_nan(norm)) {
    return 1e-6;
  }
  
  return fmax(1e-6, result/norm);
}

// Weibull TPC function
real weibull_tpc(real temp, real a, real topt, real b, real c) {
  real term1;
  real term2;
  real term3;
  real inner;
  
  // Return small positive value if parameters invalid
  if (a <= 0 || b <= 0 || c <= 0) {
    return 1e-6;
  }
  
  term1 = a * pow(fmax(0.0, ((c - 1)/c)), ((1 - c)/c));
  inner = ((temp - topt)/b) + pow(fmax(0.0, ((c - 1)/c)), (1.0/c));
  term2 = pow(fabs(inner), c - 1);  // Use fabs instead of abs
  term3 = exp(-pow(fabs(inner), c) + ((c - 1)/c));
  
  real result = term1 * term2 * term3;
  
  // Return small positive value if result invalid
  if (result <= 0 || is_nan(result)) {
    return 1e-6;
  }
  
  return fmax(1e-6, result);
}


// Gaussian TPC function
real gaussian_tpc(real temp, real rmax, real topt, real a) {
  real est;
  
  if (rmax <= 0 || a <= 0) {
    return 1e-10;
  }
  
  est = rmax * exp(-0.5 * pow(fabs(temp - topt)/a, 2));
  
  if (est <= 0) {
    return 1e-10;
  }
  
  return est;
}

"



get_model_formula <- function(model_type) {
  formulas <- list(
    flex = bf(
      rate ~ flexTPC(temp_std * attr(data, "temp_sd") + attr(data, "temp_mean"), Tmin, Tmax, rmax, alpha, beta),
      Tmin ~ 1,
      Tmax ~ 1,
      rmax ~ 1,
      alpha ~ 1,
      beta ~ 1,
      nl = TRUE
    ),
    lrf = bf(
      rate ~ LRF(temp_std * attr(data, "temp_sd") + attr(data, "temp_mean"), Tmin, Topt + Above, Topt, Ropt),
      Tmin ~ 1,
      Topt ~ 1,
      Above ~ 1,
      Ropt ~ 1,
      nl = TRUE
    ),
    deutsch = bf(
      rate ~ deutsch(temp_std * attr(data, "temp_sd") + attr(data, "temp_mean"), rmax, topt, ctmax, a),
      rmax ~ 1,
      topt ~ 1,
      ctmax ~ 1,
      a ~ 1,
      nl = TRUE
    ),
    ss = bf(
      rate ~ sharpe_schoolfield(temp_std * attr(data, "temp_sd") + attr(data, "temp_mean"), rtref, e, el, tl, eh, th),
      rtref ~ 1,
      e ~ 1,
      el ~ 1,
      tl ~ 1,
      eh ~ 1,
      th ~ 1,
      nl = TRUE
    ),
    lactin2 = bf(
      rate ~ lactin2(temp_std * attr(data, "temp_sd") + attr(data, "temp_mean"), a, b, tmax, deltat),
      a ~ 1,
      b ~ 1,
      tmax ~ 1,
      deltat ~ 1,
      nl = TRUE
    ),
    rezende = bf(rate ~ rezende(temp_std * attr(data, "temp_sd") + attr(data, "temp_mean"), q10, a, b, c), q10 ~ 1, a ~ 1, b ~ 1, c ~ 1, nl = TRUE),
    beta = bf(rate ~ beta_tpc(temp_std * attr(data, "temp_sd") + attr(data, "temp_mean"), a, b, c, d, e), a ~ 1, b ~ 1, c ~ 1, d ~ 1, e ~ 1, nl = TRUE),
    gaussian = bf(
      rate ~ gaussian_tpc(temp_std * attr(data, "temp_sd") + attr(data, "temp_mean"), rmax, topt, a),
      rmax ~ 1,
      topt ~ 1,
      a ~ 1,
      nl = TRUE
    ),
    weibull = bf(rate ~ weibull_tpc(temp_std * attr(data, "temp_sd") + attr(data, "temp_mean"), a, topt, b, c), a ~ 1, topt ~ 1, b ~ 1, c ~ 1, nl = TRUE)
  )
  return(formulas[[model_type]])
}

get_model_prior <- function(model_type) {
  priors <- list(
    flex = prior(normal(10, 5), nlpar = "Tmin", lb = 0) +
      prior(normal(45, 5), nlpar = "Tmax", lb = 35) +
      prior(normal(10, 5), nlpar = "rmax", lb = 0) +
      prior(beta(5, 5), nlpar = "alpha") +
      prior(gamma(4, 20), nlpar = "beta", lb = 0),
    lrf = prior(normal(12, 3), nlpar = "Tmin", lb = 0, ub = 20) +
      prior(normal(40, 3), nlpar = "Topt", lb = 30, ub = 50) +
      prior(normal(10, 3), nlpar = "Above", lb = 5, ub = 15) +
      prior(normal(7, 3), nlpar = "Ropt", lb = 0),
    deutsch = prior(normal(7, 3), nlpar = "rmax", lb = 0) +
      prior(normal(40, 3), nlpar = "topt", lb = 30, ub = 50) +
      prior(normal(50, 3), nlpar = "ctmax", lb = 40, ub = 60) +
      prior(normal(7, 3), nlpar = "a", lb = 0),
    ss = prior(normal(0.74, 0.4), nlpar = "rtref", lb = 0) +  
      prior(normal(0.68, 0.4), nlpar = "e", lb = 0) +  
      prior(normal(1.32, 0.6), nlpar = "el", lb = 0) + 
      prior(normal(19, 6), nlpar = "tl", lb = 0) +  
      prior(normal(3.8, 1), nlpar = "eh", lb = 0) + 
      prior(normal(42, 3), nlpar = "th", lb = 30),
    lactin2 = prior(normal(0.188, 0.1), nlpar = "a", lb = 0) +  
      prior(normal(0.167, 0.1), nlpar = "b", lb = 0) +  
      prior(normal(50, 3), nlpar = "tmax", lb = 40, ub = 60) +  
      prior(normal(5.31, 2), nlpar = "deltat", lb = 0),  
    rezende = prior(normal(4, 2), nlpar = "q10", lb = 0) + 
      prior(normal(0.0706, 0.05), nlpar = "a", lb = 0) +  
      prior(normal(2, 1), nlpar = "b") +  
      prior(normal(0.000517, 0.0002), nlpar = "c", lb = 0),
    beta = prior(normal(4.82, 2), nlpar = "a", lb = 0) +  
      prior(normal(40, 3), nlpar = "b", lb = 30, ub = 50) + 
      prior(normal(117, 30), nlpar = "c", lb = 0) + 
      prior(normal(19.2, 5), nlpar = "d", lb = 1) + 
      prior(normal(2.05, 1), nlpar = "e", lb = 1),  
    gaussian = prior(normal(7, 3), nlpar = "rmax", lb = 0) + 
      prior(normal(40, 3), nlpar = "topt", lb = 30, ub = 50) +  
      prior(normal(7, 3), nlpar = "a", lb = 0),  
    weibull = prior(normal(7, 3), nlpar = "a", lb = 0) +  
      prior(normal(40, 3), nlpar = "topt", lb = 30, ub = 50) +  
      prior(normal(357675, 100000), nlpar = "b", lb = 0) +  
      prior(normal(57040, 20000), nlpar = "c", lb = 0)
  )
  existing_prior <- priors[[model_type]]
  # Add shape prior
  full_prior <- existing_prior + 
    prior(gamma(3, 1), class = "shape")  # weakly informative prior
  return(full_prior)
}

get_model_settings <- function(model_type) {
  list(
    control = list(adapt_delta = 0.9999, max_treedepth = 15),
    iter = 2000, #shortened for testing
    warmup = 1000,
    chains = 4,
    cores = 4,
    seed = 123
  )
}
