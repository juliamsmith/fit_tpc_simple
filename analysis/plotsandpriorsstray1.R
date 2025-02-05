library(tidyverse)
library(gt)
library(scales)

plot_parameter_estimates <- function(model_list) {
  # Extract estimates
  param_estimates <- map_dfr(names(model_list), function(name) {
    post_draws <- as_draws_df(model_list[[name]])
    params <- names(post_draws)[startsWith(names(post_draws), "b_")]
    
    map_dfr(params, function(param) {
      vals <- post_draws[[param]]
      tibble(
        model = name,
        parameter = param,
        mean = mean(vals),
        lower = quantile(vals, 0.025),
        upper = quantile(vals, 0.975),
        sd = sd(vals)
      )
    })
  })
  
  # Split by model type and create table
  param_estimates <- param_estimates %>%
    mutate(
      model_type = str_extract(model, "^[^_]+"),
      equation = str_extract(model, "(?<=_).+"),
      estimate_str = sprintf("%.3g [%.3g, %.3g]", mean, lower, upper)
    )
  
  # Create table
  param_table <- param_estimates %>%
    select(model_type, equation, parameter, estimate_str) %>%
    arrange(model_type, equation, parameter) %>%
    gt() %>%
    tab_header(title = "Parameter Estimates by Model")
  
  # Split into regular and extreme values for plotting
  param_estimates <- param_estimates %>%
    mutate(
      extreme = abs(mean) > 100 | sd > 100,
      plot_mean = ifelse(extreme, sign(mean) * log10(abs(mean)), mean),
      plot_lower = ifelse(extreme, sign(lower) * log10(abs(lower)), lower),
      plot_upper = ifelse(extreme, sign(upper) * log10(abs(upper)), upper)
    )
  
  # Create plots for each model type
  plot_list <- param_estimates %>%
    group_by(model_type) %>%
    group_map(~{
      # Regular scale plot
      regular_params <- filter(.x, !extreme)
      p1 <- if(nrow(regular_params) > 0) {
        ggplot(regular_params, 
               aes(y = interaction(equation, parameter), 
                   x = mean, color = equation)) +
          geom_point() +
          geom_errorbarh(aes(xmin = lower, xmax = upper)) +
          labs(title = paste(.y$model_type, "Parameters (Regular Scale)"),
               x = "Estimate", y = "") +
          theme_minimal()
      }
      
      # Log-transformed plot
      extreme_params <- filter(.x, extreme)
      p2 <- if(nrow(extreme_params) > 0) {
        ggplot(extreme_params, 
               aes(y = interaction(equation, parameter), 
                   x = plot_mean, color = equation)) +
          geom_point() +
          geom_errorbarh(aes(xmin = plot_lower, xmax = plot_upper)) +
          labs(title = paste(.y$model_type, "Parameters (Log10 Transform)"),
               x = "log10(|Estimate|) × sign(Estimate)", y = "") +
          theme_minimal()
      }
      
      list(regular = p1, log = p2, data = .x)
    })
  
  return(list(plots = plot_list, table = param_table))
}

plot_model_predictions <- function(model_list, data_df, temp_range = c(15, 45)) {
  temps <- seq(temp_range[1], temp_range[2], length.out = 100)
  
  predictions <- map_dfr(names(model_list), function(name) {
    model <- model_list[[name]]
    newdata <- data.frame(temp = temps)
    
    pred_matrix <- posterior_predict(model, newdata = newdata)
    
    tibble(
      model = name,
      model_type = str_extract(name, "^[^_]+"),
      equation = str_extract(name, "(?<=_).+"),
      temperature = temps,
      mean = colMeans(pred_matrix),
      lower = apply(pred_matrix, 2, quantile, 0.025),
      upper = apply(pred_matrix, 2, quantile, 0.975)
    )
  })
  
  # Create separate plots for MB and MS
  plots <- predictions %>%
    group_by(model_type) %>%
    group_map(~{
      model_data <- data_df %>% 
        filter(spp == .y$model_type)
      
      ggplot() +
        geom_point(data = model_data, 
                   aes(x = temp, y = rate), 
                   alpha = 0.5) +
        geom_ribbon(data = .x, 
                    aes(x = temperature, ymin = lower, ymax = upper, 
                        fill = equation),
                    alpha = 0.2) +
        geom_line(data = .x,
                  aes(x = temperature, y = mean, color = equation)) +
        labs(title = paste(.y$model_type, "Model Predictions"),
             x = "Temperature (°C)",
             y = "Rate") +
        theme_minimal()
    })
  
  return(plots)
}

results <- plot_parameter_estimates(model_list)
print(results$plots[[1]]$regular)  # Regular scale MB
print(results$plots[[1]]$log)      # Log scale MB
print(results$table)

pred_plots <- plot_model_predictions(model_list, data_df)
print(pred_plots[[1]])  # MB
print(pred_plots[[2]])  # MS



#OLD

library(tidyverse)
library(gt)
library(scales)

plot_parameter_estimates <- function(model_list) {
  # Extract estimates
  param_estimates <- map_dfr(names(model_list), function(name) {
    post_draws <- as_draws_df(model_list[[name]])
    params <- names(post_draws)[startsWith(names(post_draws), "b_")]
    
    map_dfr(params, function(param) {
      tibble(
        model = name,
        parameter = param,
        mean = mean(post_draws[[param]]),
        lower = quantile(post_draws[[param]], 0.025),
        upper = quantile(post_draws[[param]], 0.975),
        sd = sd(post_draws[[param]])
      )
    })
  })
  
  # Split by model type
  param_estimates <- param_estimates %>%
    mutate(
      model_type = str_extract(model, "^[^_]+"),
      equation = str_extract(model, "(?<=_).+"),
      log_scale = abs(mean) > 1000 | sd > 1000
    )
  
  # Create plots for each model type (MB/MS)
  plot_list <- param_estimates %>%
    group_by(model_type) %>%
    group_map(~{
      # Split into regular and log scale parameters
      regular_params <- filter(.x, !log_scale)
      log_params <- filter(.x, log_scale)
      
      # Regular scale plot
      p1 <- ggplot(regular_params, 
                   aes(y = interaction(equation, parameter), x = mean, color = equation)) +
        geom_point() +
        geom_errorbarh(aes(xmin = lower, xmax = upper)) +
        labs(title = paste(.y$model_type, "Parameters (Regular Scale)"),
             x = "Estimate", y = "") +
        theme_minimal()
      
      # Log scale plot (if needed)
      if(nrow(log_params) > 0) {
        p2 <- ggplot(log_params, 
                     aes(y = interaction(equation, parameter), x = mean, color = equation)) +
          geom_point() +
          geom_errorbarh(aes(xmin = lower, xmax = upper)) +
          scale_x_log10(labels = scientific) +
          labs(title = paste(.y$model_type, "Parameters (Log Scale)"),
               x = "Estimate (log scale)", y = "") +
          theme_minimal()
        
        list(regular = p1, log = p2)
      } else {
        list(regular = p1)
      }
    })
  
  return(plot_list)
}

plot_model_predictions <- function(model_list, data, temp_range = c(15, 45)) {
  temps <- seq(temp_range[1], temp_range[2], length.out = 100)
  
  predictions <- map_dfr(names(model_list), function(name) {
    model <- model_list[[name]]
    newdata <- data.frame(temp = temps)
    
    pred_matrix <- posterior_predict(model, newdata = newdata)
    
    tibble(
      model = name,
      model_type = str_extract(name, "^[^_]+"),
      equation = str_extract(name, "(?<=_).+"),
      temperature = temps,
      mean = colMeans(pred_matrix),
      lower = apply(pred_matrix, 2, quantile, 0.025),
      upper = apply(pred_matrix, 2, quantile, 0.975)
    )
  })
  
  # Create separate plots for MB and MS
  plots <- predictions %>%
    group_by(model_type) %>%
    group_map(~{
      ggplot() +
        # Add data points
        geom_point(data = filter(data, species == .y$model_type), 
                   aes(x = temp, y = rate), 
                   alpha = 0.5) +
        # Add model predictions
        geom_ribbon(data = .x, 
                    aes(x = temperature, ymin = lower, ymax = upper, 
                        fill = equation),
                    alpha = 0.2) +
        geom_line(data = .x,
                  aes(x = temperature, y = mean, color = equation)) +
        labs(title = paste(.y$model_type, "Model Predictions"),
             x = "Temperature (°C)",
             y = "Rate") +
        theme_minimal()
    })
  
  return(plots)
}


# For parameter plots
param_plots <- plot_parameter_estimates(model_list)
# Access MB plots
print(param_plots[[1]]$regular)  # Regular scale
print(param_plots[[1]]$log)      # Log scale if exists
# Access MS plots similarly with param_plots[[2]]

# For prediction plots
pred_plots <- plot_model_predictions(model_list, data)
# Access MB and MS plots
print(pred_plots[[1]])  # MB
print(pred_plots[[2]])  # MS











#OLD


library(tidyverse)
library(gt)

plot_parameter_estimates <- function(model_list) {
  # Extract estimates
  param_estimates <- map_dfr(names(model_list), function(name) {
    post_draws <- as_draws_df(model_list[[name]])
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
  
  # Categorize parameters by scale
  param_estimates <- param_estimates %>%
    mutate(
      scale = case_when(
        str_detect(parameter, "topt|tmin|tmax|ctmax|th|tl") ~ "temperature",
        str_detect(parameter, "rmax|ropt|rtref") ~ "rate",
        TRUE ~ "shape"
      )
    )
  
  # Create separate plots for each scale
  plot_list <- param_estimates %>%
    group_by(scale) %>%
    group_map(~{
      ggplot(.x, aes(y = interaction(model, parameter), x = mean)) +
        geom_point() +
        geom_errorbarh(aes(xmin = lower, xmax = upper)) +
        labs(title = paste0(str_to_title(.y$scale), " Parameters"),
             x = "Estimate",
             y = "") +
        theme_minimal() +
        theme(axis.text.y = element_text(hjust = 0))
    })
  
  # Create table
  param_table <- param_estimates %>%
    arrange(scale, model, parameter) %>%
    mutate(
      estimate = sprintf("%.2f [%.2f, %.2f]", mean, lower, upper)
    ) %>%
    select(scale, model, parameter, estimate) %>%
    gt() %>%
    tab_header(title = "Parameter Estimates by Model") %>%
    fmt_markdown(columns = estimate)
  
  return(list(plots = plot_list, table = param_table))
}

plot_model_predictions <- function(model_list, temp_range = c(15, 45)) {
  temps <- seq(temp_range[1], temp_range[2], length.out = 100)
  predictions <- map_dfr(names(model_list), function(name) {
    model <- model_list[[name]]
    newdata <- data.frame(temp = temps)
    
    pred_matrix <- posterior_predict(model, newdata = newdata)
    
    tibble(
      model = name,
      temperature = temps,
      mean = colMeans(pred_matrix),
      lower = apply(pred_matrix, 2, quantile, 0.025),
      upper = apply(pred_matrix, 2, quantile, 0.975)
    )
  })
  
  ggplot(predictions) +
    geom_ribbon(aes(x = temperature, ymin = lower, ymax = upper, fill = model),
                alpha = 0.2) +
    geom_line(aes(x = temperature, y = mean, color = model)) +
    labs(title = "Model Predictions",
         x = "Temperature (°C)",
         y = "Rate") +
    theme_minimal()
}

# Usage:
param_results <- plot_parameter_estimates(model_list)
print(param_results$plots[[1]]) # Temperature parameters
print(param_results$plots[[2]]) # Rate parameters
print(param_results$plots[[3]]) # Shape parameters
print(param_results$table)
# 
pred_plot <- plot_model_predictions(model_list)