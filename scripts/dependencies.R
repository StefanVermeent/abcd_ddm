library(groundhog)

# Dependencies for this project
pkgs <- 
  c(
    "tidyverse",
    "utils", 
    "gert",
    "glue",
    "stringdist",
    "rjags",
    "coda",
    "rstan",
    "hBayesDM",
    "htmltools",
    "gt",
    "flextable",
    "lavaan", 
    "lavaan.survey"
  )

# The date of which we want to install and load the package versions
date <- "2022-10-15"

# Using the groundhog package, we install and load the project dependencies
groundhog.library(pkg = pkgs, date = date, force.install = TRUE)


# Data folder
# Set this to `synthetic_data` if you want to reproduce the analyses
# using the synthetic data files.
data_folder <- 'closed_data'
