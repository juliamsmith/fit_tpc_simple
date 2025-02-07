# R/prepare_data.R

prepare_data <- function(species) {
  # Read data
  ad <- read.csv("data/ad.csv")
  
  # Filter for species and add small constant for gamma distribution
  model_data <- ad %>%
    filter(spp == species) %>%
    mutate(
      .min_nonzero = min(rate[rate > 0], na.rm = TRUE),
      rate = rate + .min_nonzero/100,
      temp_mean = mean(temp),
      temp_sd = sd(temp),
      temp_std = (temp - temp_mean) / temp_sd
    ) %>%
    select(-`.min_nonzero`)
  return(model_data)
}


