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
    "hBayesDM"
  )

# The date of which we want to install and load the package versions
date <- "2022-10-15"

# Using the groundhog package, we install and load the project dependencies
groundhog.library(pkg = pkgs, date = date)
