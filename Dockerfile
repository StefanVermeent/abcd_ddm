## This is a Docker Container that can be used to make the scripts in this repository fully reproducible ##
## Docker makes sure that all the project dependencies (e.g., software/R package versions, operating system) 
## are identical to the versions that were used when creating the project.
## Thus, it ensures full computational reproducibility even when any of the dependencies have broken due to updates or deprecations.
## For more information on how to use Docker to reproduce the analyses, see the README file.

# Initiate RStudio cloud environment with version 4.1.2
FROM rocker/rstudio:4.1.2

LABEL maintainer="p.c.s.vermeent@gmail.com"
LABEL description="Rstudio container with necessary dependencies"


# Install the 'groundhog' package for package management, and install correct versions of packages
WORKDIR c:/repositories/abcd_ddm

ADD /scripts/ /scripts/

RUN R -e "install.packages('groundhog')"
RUN Rscript /scripts/dependencies.R

# Install JAGS and the wiener module
RUN apt-get update && . /etc/environment \
  && wget sourceforge.net/projects/mcmc-jags/files/JAGS/4.x/Source/JAGS-4.2.0.tar.gz  -O jags.tar.gz \
  && tar -xf jags.tar.gz \
  && cd JAGS* && ./configure && make -j4 && make install \
  && wget sourceforge.net/projects/jags-wiener/files/JAGS-WIENER-MODULE-1.1.tar.gz -O JAGS-WIENER-MODULE-1.1.tar.gz \
  && tar -xf JAGS-WIENER-MODULE-1.1.tar.gz \
  && cd JAGS* && ./configure && make -j4 && make install

