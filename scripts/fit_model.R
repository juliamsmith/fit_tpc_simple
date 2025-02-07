# scripts/fit_model.R
#!/usr/bin/env Rscript
source("R/setup.R")
setup_packages()

args <- commandArgs(trailingOnly = TRUE)
model_type <- args[1]  # e.g., "lrf"
species <- args[2]     # e.g., "MS"

source("R/functions.R")
source("R/model_specs.R")
source("R/prepare_data.R")

# Get data and model specs
data <- prepare_data(species)
formula <- get_model_formula(model_type)
prior <- get_model_prior(model_type)
settings <- get_model_settings(model_type)

# Add some logging
cat(sprintf("Fitting %s model for species %s\n", model_type, species))

# Fit model
fit <- brm(
  formula = formula,
  data = data,
  prior = prior,
  family = Gamma(link = "identity"),  # Added explicit family specification
  stanvars = stanvar(scode = stan_funs, block = "functions"),
  control = settings$control,
  iter = settings$iter,
  warmup = settings$warmup,
  chains = settings$chains,
  cores = settings$cores,
  seed = settings$seed
)

# Check convergence
conv_check <- check_convergence(fit)
if(!conv_check$passed) {
  warning(sprintf("Model convergence issues detected:\n  Rhat max: %.3f\n  Neff min: %.0f\n  Divergences: %d", 
                  conv_check$max_rhat, conv_check$min_neff, conv_check$n_divergent))
}

# Save output
outfile <- paste0("output/fits/", species, "_", model_type, ".rds")
saveRDS(fit, file = outfile)
cat(sprintf("Model saved to %s\n", outfile))
