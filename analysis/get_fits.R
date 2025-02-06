library(stringr)
setwd("~/GitHub/fit_tpc_simple")

# Get file names
filenames <- list.files("output/fits", pattern="*.rds")

# Create model list by reading the RDS files directly
model_list <- setNames(
  lapply(filenames, function(f) readRDS(file.path("output/fits", f))),
  str_remove(filenames, ".rds")
)

for (model_name in names(model_list)) {
  current_model <- model_list[[model_name]]
  expose_functions(current_model, vectorize=TRUE)
}


