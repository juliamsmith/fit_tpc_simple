library(tidyverse)
library(bayesplot)
library(patchwork)

plot_parameter_estimates <- function(model_list) {
  # Extract posterior means and CIs for all models
  param_estimates <- map_dfr(names(model_list), function(name) {
    model <- model_list[[name]]
    post_draws <- as.data.frame(model)
    params <- names(post_draws)[startsWith(names(post_draws), "b_")]
    
    map_dfr(params, function(param) {
      tibble(
        model = name,
        parameter = param,
        mean = mean(post_draws[[param]]),
        lower = quantile(post_draws[[param]], 0.025),
        upper = quantile(post_draws[[param]], 0.975)
      )
    })
  })
  
  # Create forest plot
  ggplot(param_estimates, aes(y = interaction(model, parameter), x = mean)) +
    geom_point() +
    geom_errorbarh(aes(xmin = lower, xmax = upper)) +
    labs(title = "Parameter Estimates Across Models",
         x = "Estimate",
         y = "") +
    theme_minimal() +
    theme(axis.text.y = element_text(hjust = 0))
}

plot_model_predictions <- function(model_list, data, temp_range = c(15, 45)) {
  # Generate temperature sequence
  temps <- seq(temp_range[1], temp_range[2], length.out = 100)
  
  # Get predictions for each model
  predictions <- map_dfr(names(model_list), function(name) {
    model <- model_list[[name]]
    post_draws <- as.data.frame(model)
    
    # Get predictions for each temperature
    pred_matrix <- matrix(NA, nrow = length(temps), ncol = nrow(post_draws))
    for(i in seq_along(temps)) {
      # Get predictions (need to implement prediction function for each model type)
      pred_matrix[i,] <- predict(model, newdata = data.frame(temp = temps[i]))
    }
    
    # Summarize predictions
    tibble(
      model = name,
      temperature = temps,
      mean = rowMeans(pred_matrix),
      lower = apply(pred_matrix, 1, quantile, 0.025),
      upper = apply(pred_matrix, 1, quantile, 0.975)
    )
  })
  
  # Plot predictions
  ggplot() +
    geom_point(data = data, aes(x = temp, y = rate), alpha = 0.5) +
    geom_ribbon(data = predictions, 
                aes(x = temperature, ymin = lower, ymax = upper, fill = model),
                alpha = 0.2) +
    geom_line(data = predictions,
              aes(x = temperature, y = mean, color = model)) +
    labs(title = "Model Predictions",
         x = "Temperature",
         y = "Rate") +
    theme_minimal()
}

compare_residuals <- function(model_list, data) {
  # Get residuals for each model
  residuals_df <- map_dfr(names(model_list), function(name) {
    model <- model_list[[name]]
    fitted_vals <- fitted(model)[,1]
    resid <- residuals(model)[,1]
    
    tibble(
      model = name,
      fitted = fitted_vals,
      residual = resid
    )
  })
  
  # Create residual plots
  ggplot(residuals_df, aes(x = fitted, y = residual)) +
    geom_point(alpha = 0.5) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    facet_wrap(~model) +
    labs(title = "Residual Plots by Model",
         x = "Fitted Values",
         y = "Residuals") +
    theme_minimal()
}

assess_model_fit <- function(model_list, data) {
  # Calculate fit statistics for each model
  fit_stats <- map_dfr(names(model_list), function(name) {
    model <- model_list[[name]]
    
    # Get LOO and WAIC
    loo_result <- loo(model)
    waic_result <- waic(model)
    
    tibble(
      model = name,
      loo_elpd = loo_result$estimates["elpd_loo", "Estimate"],
      loo_se = loo_result$estimates["elpd_loo", "SE"],
      waic_elpd = waic_result$estimates["elpd_waic", "Estimate"],
      waic_se = waic_result$estimates["elpd_waic", "SE"]
    )
  })
  
  return(fit_stats)
}

  
# Example usage:
param_plot <- plot_parameter_estimates(model_list)
pred_plot <- plot_model_predictions(model_list, data)
resid_plot <- compare_residuals(model_list)
fit_stats <- assess_model_fit(model_list, data)