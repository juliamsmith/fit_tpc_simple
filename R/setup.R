# R/setup.R
setup_packages <- function() {
  packages <- c("brms", "tidyverse")
  for(pkg in packages) {
    if(!require(pkg, character.only = TRUE)) {
      install.packages(pkg)
      library(pkg, character.only = TRUE)
    }
  }
}