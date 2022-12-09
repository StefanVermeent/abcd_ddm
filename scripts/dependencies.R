library(groundhog)

# Dependencies for this project
pkgs <- 
  c(
    "tidyverse",
    "utils", 
    "gert",
    "glue",
    "stringdist",
    "runjags",
    "htmltools",
    "flextable",
    "lavaan", 
    "lavaan.survey",
    "assertthat",
    "english"
  )

# The date of which we want to install and load the package versions
date <- "2022-10-15"

# Using the groundhog package, we install and load the project dependencies
groundhog.library(pkg = pkgs, date = date)


# Data folder
# Set this to data if you want to reproduce the analyses
# using the synthetic data files.
data_folder <- 'closed_data'


# Source functions
source('scripts/custom_functions/read-functions.R')
source('scripts/custom_functions/general-functions.R')


