library(stringr)

setwd("~/GitHub/fit_tpc_simple")

filenames <- list.files("output/fits", pattern="*.rds")

model_list <- setNames(
  lapply(filenames, function(f) get(str_remove(f, ".rds"))),
  str_remove(filenames, ".rds")
)

for (model_name in names(model_list)) {
  current_model <- model_list[[model_name]]
  expose_functions(current_model, vectorize=TRUE)
}


