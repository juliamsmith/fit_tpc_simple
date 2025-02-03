#!/usr/bin/env Rscript

# Parse command line arguments
args <- commandArgs(trailingOnly = TRUE)
job_type <- args[1]  # "fit" or "compare"
model <- args[2]     # e.g., "lrf"
species <- args[3]   # e.g., "MS"
run_type <- ifelse(length(args) > 3, args[4], "test")  # default to "test"

# Validate run_type
if(!run_type %in% c("test", "run")) {
  stop('run_type must be either "test" or "run"')
}

submit_jobs <- function(job_type, model, species, run_type) {
  # Validate model type
  valid_models <- c("lrf", "deutsch", "ss", "lactin2", "rezende", "beta", "gaussian", "weibull")
  if(!model %in% valid_models) {
    stop(sprintf("model must be one of: %s", paste(valid_models, collapse=", ")))
  }
  
  # Read template based on job type
  if(job_type == "fit") {
    template <- readLines("slurm/run_model.sh")
  } else if(job_type == "compare") {
    template <- readLines("slurm/compare_dists.sh")
  } else {
    stop("job_type must be 'fit' or 'compare'")
  }
  
  # Create the script name before using it
  script_name <- sprintf("temp_%s_%s_%s_%s.sh", job_type, model, species, run_type)
  
  # Create and submit job
  job_script <- template
  job_script <- gsub("\\{MODEL\\}", model, job_script)
  job_script <- gsub("\\{SPECIES\\}", species, job_script)
  
  # Create the Rscript command with all parameters
  rscript_cmd <- sprintf("Rscript $WORKDIR/scripts/%s%s.R %s %s %s",
                        job_type, 
                        ifelse(job_type == "compare", "_dists", "_model"),
                        model, species, run_type)
  # Replace the Rscript command in the template
  job_script <- gsub("Rscript.*$", rscript_cmd, job_script)
  
  # Write the script to a file in the current directory
  writeLines(job_script, script_name)
  
  # Make the script executable
  system(sprintf("chmod +x %s", script_name))
  
  # Tell user what to do next
  cat(sprintf("\nPlease run this command in your shell:\nsbatch %s\n", script_name))
  cat(sprintf("\nAfter submitting, you can remove the temporary script with:\nrm %s\n", script_name))
}

# Execute if called directly
if(!interactive()) {
  submit_jobs(job_type, model, species, run_type)
}
