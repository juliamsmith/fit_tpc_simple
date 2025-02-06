# R/prepare_data.R

prepare_data <- function(species) {
  # Read data
  ad <- read.csv("data/ad.csv")
  
  # Filter for species and add small constant for gamma distribution
  model_data <- ad %>%
    filter(spp == species) %>%
    mutate(
      # Add small constant to handle zeros
      min_nonzero <- min(rate[rate > 0], na.rm = TRUE),
      rate = rate + min_nonzero/100,
      # Add standardized temperature
      temp_std = (temp - mean(temp)) / sd(temp)
    )
  
  # Store temperature transformation values for later back-transformation
  attr(model_data, "temp_mean") <- mean(ad$temp)
  attr(model_data, "temp_sd") <- sd(ad$temp)
  
  return(model_data)
}