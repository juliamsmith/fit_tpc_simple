#NEWEREST
plot_model_predictions <- function(model_list, ad_data) {
  temps <- seq(15, 45, length.out = 100)
  
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
  
  plots <- predictions %>%
    group_by(model_type) %>%
    group_map(~{
      model_data <- ad_data %>%
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

ad <- read.csv("data/ad.csv")

pred_plots <- plot_model_predictions(model_list, ad)


#just deutsch and lrf
#pred_plots <- plot_model_predictions(model_list[c(2,5,10,13)], ad)
print(pred_plots[[1]])  # MB
print(pred_plots[[2]])  # MS

#older
library(tidyverse)
library(patchwork)


plot_model_predictions <- function(model_list) {
  temps <- seq(15, 45, length.out = 100)
  
  # Get data from a model
  example_model <- model_list[[1]]
  data <- example_model$data
  
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
      model_data <- data %>%
        filter(str_detect(spp, .y$model_type))
      
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

# Example usage:
pred_plots <- plot_model_predictions(model_list)




#NEWEST

library(tidyverse)
library(brms)

analyze_priors_posteriors <- function(model_list) {
  get_summaries <- function(model, model_name) {
    tryCatch({
      # Get posterior draws
      post_draws <- as_draws_df(model)
      
      # Get parameters
      params <- names(post_draws)[startsWith(names(post_draws), "b_")]
      
      # Get prior information
      prior_info <- prior_summary(model)
      
      # Calculate summaries for each parameter
      map_dfr(params, function(param) {
        clean_param <- sub("b_", "", param)
        prior_spec <- if(!is.null(prior_info$prior)) {
          prior_row <- prior_info$prior[prior_info$prior$class == "b" & 
                                          (prior_info$prior$coef == clean_param | 
                                             is.na(prior_info$prior$coef)), ]
          ifelse(nrow(prior_row) > 0, prior_row$prior[1], "Default")
        } else {
          "Not available"
        }
        
        tibble(
          model = model_name,
          parameter = param,
          prior = prior_spec,
          post_mean = mean(post_draws[[param]]),
          post_sd = sd(post_draws[[param]]),
          post_q2.5 = quantile(post_draws[[param]], 0.025),
          post_q97.5 = quantile(post_draws[[param]], 0.975)
        )
      })
    }, error = function(e) {
      message("Error processing ", model_name, ": ", e$message)
      return(NULL)
    })
  }
  
  # Process all models
  bind_rows(lapply(names(model_list), function(name) {
    get_summaries(model_list[[name]], name)
  }))
}

plot_model_predictions <- function(model_list, temperature_range = c(15, 45)) {
  temps <- seq(temperature_range[1], temperature_range[2], length.out = 100)
  
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
  
  # Extract original data from one of the models
  model_data <- model_list[[1]]$data
  
  # Create separate plots for MB and MS
  plots <- predictions %>%
    group_by(model_type) %>%
    group_map(~{
      curr_data <- model_data %>%
        filter(spp == .y$model_type)
      
      ggplot() +
        geom_point(data = curr_data, 
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

plot_prior_posterior_densities <- function(model_list) {
  map(names(model_list), function(name) {
    model <- model_list[[name]]
    post_draws <- as_draws_df(model)
    params <- names(post_draws)[startsWith(names(post_draws), "b_")]
    
    # Create density plots for each parameter
    param_plots <- map(params, function(param) {
      # Get posterior values
      posterior_vals <- post_draws[[param]]
      
      # Determine if we need log scale
      needs_log <- abs(mean(posterior_vals)) > 100 || sd(posterior_vals) > 100
      
      # Handle extreme values
      if(needs_log) {
        posterior_vals <- sign(posterior_vals) * log10(abs(posterior_vals))
        x_lab <- "log10(|Value|) × sign(Value)"
      } else {
        x_lab <- "Value"
      }
      
      # Create plot
      ggplot() +
        geom_density(data = data.frame(value = posterior_vals),
                     aes(x = value),
                     fill = "blue", alpha = 0.5) +
        labs(title = param,
             x = x_lab,
             y = "Density") +
        theme_minimal()
    })
    
    wrap_plots(param_plots) +
      plot_annotation(title = paste(name, "Model"))
  })
}


summaries <- analyze_priors_posteriors(model_list)
pred_plots <- plot_model_predictions(model_list)
density_plots <- plot_prior_posterior_densities(model_list)

#THIS ONE
#also we want to see all the priors vs posteriors
plot_parameter_densities <- function(model, name, n_prior = 10000) {
  # Get posterior draws
  post_draws <- as_draws_df(model)
  params <- names(post_draws)[startsWith(names(post_draws), "b_")]
  
  # Get prior information
  prior_info <- model$prior
  
  # Generate prior samples
  prior_samples <- map(params, function(param) {
    # Extract prior for this parameter
    clean_param <- sub("b_", "", param)
    clean_param <- sub("_Intercept", "", clean_param)
    
    prior_row <- prior_info[prior_info$nlpar == clean_param & 
                              prior_info$class == "b", ]
    
    if(nrow(prior_row) > 0 && !is.na(prior_row$prior[1]) && prior_row$prior[1] != "") {
      # Parse prior string
      prior_str <- prior_row$prior[1]
      if(grepl("normal", prior_str)) {
        params <- as.numeric(str_extract_all(prior_str, "-?\\d+\\.?\\d*")[[1]])
        return(rnorm(n_prior, params[1], params[2]))
      }
    }
    return(NULL)
  })
  names(prior_samples) <- params
  
  # Create plots
  param_plots <- imap(params, function(param, i) {
    posterior_vals <- post_draws[[param]]
    prior_vals <- prior_samples[[param]]
    
    use_log <- abs(mean(posterior_vals)) > 1000 || sd(posterior_vals) > 1000
    
    if(use_log) {
      posterior_vals <- sign(posterior_vals) * log10(abs(posterior_vals))
      if(!is.null(prior_vals)) {
        prior_vals <- sign(prior_vals) * log10(abs(prior_vals))
      }
      x_lab <- "log10(|Value|) × sign(Value)"
    } else {
      x_lab <- "Value"
    }
    
    p <- ggplot()
    
    if(!is.null(prior_vals)) {
      p <- p + geom_density(data = data.frame(value = prior_vals),
                            aes(x = value, color = "Prior"),
                            alpha = 0.5)
    }
    
    p + geom_density(data = data.frame(value = posterior_vals),
                     aes(x = value, color = "Posterior"),
                     alpha = 0.5) +
      scale_color_manual(values = c("Prior" = "red", "Posterior" = "blue")) +
      labs(title = param,
           x = x_lab,
           y = "Density",
           color = "") +
      theme_minimal()
  })
  
  wrap_plots(param_plots) +
    plot_annotation(title = paste(name, "Prior vs Posterior"))
}




#No this one!
plot_parameter_densities <- function(model, name, n_prior = 10000) {
  post_draws <- as_draws_df(model)
  params <- names(post_draws)[startsWith(names(post_draws), "b_")]
  prior_info <- model$prior
  
  # Generate prior samples (unchanged)
  prior_samples <- map(params, function(param) {
    clean_param <- sub("b_", "", param)
    clean_param <- sub("_Intercept", "", clean_param)
    
    prior_row <- prior_info[prior_info$nlpar == clean_param & 
                              prior_info$class == "b", ]
    
    if(nrow(prior_row) > 0 && !is.na(prior_row$prior[1]) && prior_row$prior[1] != "") {
      prior_str <- prior_row$prior[1]
      if(grepl("normal", prior_str)) {
        params <- as.numeric(str_extract_all(prior_str, "-?\\d+\\.?\\d*")[[1]])
        return(rnorm(n_prior, params[1], params[2]))
      }
    }
    return(NULL)
  })
  names(prior_samples) <- params
  
  # Create plots with improved scaling
  param_plots <- imap(params, function(param, i) {
    posterior_vals <- post_draws[[param]]
    prior_vals <- prior_samples[[param]]
    
    # Determine if we need log scale
    use_log <- abs(mean(posterior_vals)) > 1000 || sd(posterior_vals) > 1000
    
    if(use_log) {
      # Improved log transformation handling
      posterior_vals <- ifelse(posterior_vals == 0, 0,
                               sign(posterior_vals) * log10(abs(posterior_vals) + 1))
      if(!is.null(prior_vals)) {
        prior_vals <- ifelse(prior_vals == 0, 0,
                             sign(prior_vals) * log10(abs(prior_vals) + 1))
      }
      x_lab <- "sign(Value) × log10(|Value| + 1)"
    } else {
      x_lab <- "Value"
    }
    
    p <- ggplot()
    
    if(!is.null(prior_vals)) {
      p <- p + geom_density(data = data.frame(value = prior_vals),
                            aes(x = value, color = "Prior"),
                            alpha = 0.5)
    }
    
    p + geom_density(data = data.frame(value = posterior_vals),
                     aes(x = value, color = "Posterior"),
                     alpha = 0.5) +
      scale_color_manual(values = c("Prior" = "red", "Posterior" = "blue")) +
      labs(title = param,
           x = x_lab,
           y = "Density",
           color = "") +
      theme_minimal()
  })
  
  wrap_plots(param_plots) +
    plot_annotation(title = paste(name, "Prior vs Posterior"))
}
for(name in names(model_list[9:16])){
  print(plot_parameter_densities(model_list[[name]], name))
}


