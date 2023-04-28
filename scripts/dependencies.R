#install.packages('groundhog')
library(groundhog)
meta.groundhog("2022-10-15")
groundhog::set.groundhog.folder(file.path(getwd(), 'groundhog'))

# Dependencies for this project
pkgs <- 
  c(
    "dplyr",
    "tidyr",
    "purrr",
    "readr",
    "stringr",
    "ggplot2",
    "utils", 
    "gert",
    "glue",
    "stringdist",
    "runjags",
    "RWiener",
    "htmltools",
    "flextable",
    "lavaan", 
    "lavaan.survey",
    "assertthat",
    "english",
    "ggsci",
    "lme4",
    "interactions",
    "ggeffects"
  )




# The date of which we want to install and load the package versions
date <- "2022-10-15"

# Using the groundhog package, we install and load the project dependencies
groundhog.library(pkg = pkgs, date = date)

# Do you want to use synthetic data or ABCD data to reproduce the manuscript?
data <- 'synthetic'
#data <- 'abcd'



data_suffix <- ifelse(data == 'synthetic', '_synth', '')


# Source functions
source('scripts/custom_functions/read-functions.R')
source('scripts/custom_functions/general-functions.R')
source('scripts/custom_functions/DBDA2E-utilities.R')

