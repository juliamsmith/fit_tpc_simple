# R/prepare_data.R

prepare_data <- function(species) {
  # Read data
  ad <- read.csv("data/ad.csv")
  
  # Filter for species and add small constant for gamma distribution
  model_data <- ad %>%
    filter(spp == species) 
  # Add small constant to handle zeros
  min_nonzero <- min(model_data$rate[model_data$rate > 0], na.rm = TRUE)
  epsilon <- min_nonzero/100
  model_data$rate <- model_data$rate + epsilon
  # Store the original rate too in case we need it
  model_data$rate_orig <- model_data$rate - epsilon
  
  return(model_data)
}
