library(tidyverse)
library(patchwork)

plot_parameter_densities <- function(model, n_prior = 10000) {
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
    plot_annotation(title = paste(class(model)[1], "Prior vs Posterior"))
}

# Example usage:
# For one model:
plot_parameter_densities(model_list$MB_lrf)



#OLD-- in part at least

get_summaries_debug <- function(model) {
  post_draws <- as_draws_df(model)
  print("Got draws")
  params <- names(post_draws)[startsWith(names(post_draws), "b_")]
  print("Got params:")
  print(params)
  prior_info <- prior_summary(model)
  print("Got prior info:")
  print(str(prior_info))
  return(prior_info)
}

# Try with one model
print("Testing first model:")
test_result <- get_summaries_debug(model_list[[1]])




library(tidyverse)
library(bayesplot)
library(patchwork)
library(brms)

analyze_priors_posteriors <- function(model_list) {
  tryCatch({
    get_summaries <- function(model, model_name) {
      post_draws <- as_draws_df(model)
      params <- names(post_draws)[startsWith(names(post_draws), "b_")]
      
      priors <- prior_summary(model)
      
      map_dfr(params, function(param) {
        # Get prior info if available
        prior_info <- if(!is.null(priors$prior)) {
          priors$prior[priors$prior$coef == sub("b_", "", param), ]
        } else {
          NULL
        }
        
        tibble(
          model = model_name,
          parameter = param,
          prior_spec = if(!is.null(prior_info)) paste(prior_info$prior) else "NA",
          post_mean = mean(post_draws[[param]]),
          post_sd = sd(post_draws[[param]]),
          post_q2.5 = quantile(post_draws[[param]], 0.025),
          post_q97.5 = quantile(post_draws[[param]], 0.975),
          post_rhat = rhat(model)[[param]],
          post_ess = neff_ratio(model)[[param]]
        )
      })
    }
    
    summaries <- map_dfr(names(model_list), function(name) {
      message("Processing model: ", name)
      tryCatch(
        get_summaries(model_list[[name]], name),
        error = function(e) {
          message("Error in model ", name, ": ", e$message)
          return(NULL)
        }
      )
    })
    
    # Add model type and equation type
    summaries <- summaries %>%
      mutate(
        model_type = str_extract(model, "^[^_]+"),
        equation = str_extract(model, "(?<=_).+")
      )
    
    return(summaries)
  }, error = function(e) {
    message("Error in analyze_priors_posteriors: ", e$message)
    return(NULL)
  })
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
      
      # Create density plot
      ggplot() +
        geom_density(data = data.frame(value = posterior_vals),
                     aes(x = value, fill = "Posterior"),
                     alpha = 0.5) +
        labs(title = param,
             x = "Value",
             y = "Density") +
        theme_minimal() +
        if(abs(mean(posterior_vals)) > 1000) {
          scale_x_log10(labels = scales::scientific)
        } else {
          scale_x_continuous()
        }
    })
    
    # Combine plots for this model
    wrap_plots(param_plots) +
      plot_annotation(title = paste(name, "Model"))
  })
}

# Example usage:
summaries <- analyze_priors_posteriors(model_list)
plots <- plot_prior_posterior_densities(model_list)







#OLD



library(tidyverse)
library(bayesplot)
library(patchwork)
library(loo)
library(brms)

analyze_priors_posteriors <- function(model_list) {
  get_summaries <- function(model, model_name) {
    # Extract posterior samples using as_draws_df
    post_draws <- as_draws_df(model)
    
    # Get fixed effect parameters
    params <- names(post_draws)[startsWith(names(post_draws), "b_")]
    
    # Calculate summaries
    map_dfr(params, function(param) {
      tibble(
        model = model_name,
        parameter = param,
        post_mean = mean(post_draws[[param]]),
        post_sd = sd(post_draws[[param]]),
        post_q2.5 = quantile(post_draws[[param]], 0.025),
        post_q97.5 = quantile(post_draws[[param]], 0.975),
        post_rhat = rhat(model)[[param]],
        post_ess = neff_ratio(model)[[param]]
      )
    })
  }
  
  # Process all models
  summaries <- map_dfr(names(model_list), function(name) {
    tryCatch({
      get_summaries(model_list[[name]], name)
    }, error = function(e) {
      warning(paste("Error processing model:", name, "\n", e$message))
      return(NULL)
    })
  })
  
  return(summaries)
}

plot_prior_posterior_comparisons <- function(model_list) {
  # Create posterior density plots for each model
  plot_list <- imap(model_list, function(model, name) {
    post_draws <- as_draws_df(model)
    params <- names(post_draws)[startsWith(names(post_draws), "b_")]
    
    # Create density plots
    param_plots <- map(params, function(param) {
      ggplot() +
        geom_density(data = as.data.frame(post_draws[,param]), 
                     aes(x = .data[[param]]), 
                     fill = "blue", alpha = 0.5) +
        labs(title = param,
             x = "Value",
             y = "Density") +
        theme_minimal()
    })
    
    wrap_plots(param_plots) +
      plot_annotation(title = paste(name, "Model"))
  })
  
  return(plot_list)
}

check_parameter_correlations <- function(model_list) {
  # Function to get correlation matrix for parameters
  get_correlations <- function(model) {
    post_draws <- as_draws_df(model)
    params <- names(post_draws)[startsWith(names(post_draws), "b_")]
    cor(post_draws[, params])
  }
  
  # Get correlations for each model
  corr_list <- map(model_list, get_correlations)
  
  # Create correlation plots
  plot_list <- imap(corr_list, function(corr, name) {
    corr_df <- as.data.frame(as.table(corr))
    ggplot(corr_df, aes(x = Var1, y = Var2, fill = Freq)) +
      geom_tile() +
      scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                           midpoint = 0, limits = c(-1, 1)) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(title = paste(name, "Parameter Correlations"),
           x = "", y = "")
  })
  
  return(plot_list)
}

# Example usage:
# summaries <- analyze_priors_posteriors(model_list)
# plots <- plot_prior_posterior_comparisons(model_list) 
# correlation_plots <- check_parameter_correlations(model_list)

# Example usage:
summaries <- analyze_priors_posteriors(model_list)
prior_post_plots <- plot_prior_posterior_comparisons(model_list)
corr_plots <- check_parameter_correlations(model_list)
convergence_stats <- examine_convergence(model_list)