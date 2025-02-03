# setup_dirs.R
dirs <- c("output/fits", "output/logs")
sapply(dirs, dir.create, recursive = TRUE, showWarnings = FALSE)