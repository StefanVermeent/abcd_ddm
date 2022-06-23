library(rstan)
library(hBayesDM)
library(RWiener)
library(tidyverse)
library(here)

set.seed(48548564)

hbayes_simdata <- 
  # Simulate participants along 4 samplesizes and generate DDM parameter values
  tibble(
    subjID = c(1:500, 1:1000, 1:5000, 1:10000),
    n      = c(rep(500, 500), rep(1000, 1000), rep(5000, 5000), rep(10000, 10000)),
    trials = 20,
    v      = runif(16500, -4, 4),
    a      = runif(16500, 0.5, 2),
    t0     = runif(16500, 0.2, 0.5),
    z      = 0.5
  ) 

hbayes_simdata_rt <- 
  hbayes_simdata %>%
  # Generate reaction times based on simulated (true) DDM parameter values
  pmap(function(subjID, n, trials, v, a, t0, z) {
    
    set.seed(4655)
    RWiener::rwiener(trials, alpha = a, tau = t0, delta = v, beta = z) %>%
      as_tibble() %>%
      mutate(
        subjID = subjID,
        choice = ifelse(resp == "upper", 2, 1)) %>%
      select(subjID, choice, RT = q) %>%
      mutate(
        n      = n,
        trials = trials,
        vsim   = v,
        asim   = a,
        t0sim  = t0
      )
    
  }) %>%
  bind_rows


# Run Hierarchical Bayesian Models ----------------------------------------

# Note that these we ran these models through a cloud computing platform with 16 CPU cores. 
# Even then, it takes several hours to run.

n_cores <- 16

hbayes_sim_500 <- choiceRT_ddm(
  data = hbayes_simdata_rt %>% filter(n == 500), niter = 4000, nwarmup = 1000, nchain = 4, ncore = n_cores
)

save(hbayes_sim_500, file = "~data/volume_2/hbayes_sim_results_500.Rdata")

hbayes_sim_1000 <- choiceRT_ddm(
  data = hbayes_simdata_rt %>% filter(n == 1000), niter = 4000, nwarmup = 1000, nchain = 4, ncore = n_cores
)

save(hbayes_sim_1000, file = "~data/volume_2/hbayes_sim_results_1000.Rdata")

hbayes_sim_5000 <- choiceRT_ddm(
  data = hbayes_simdata_rt %>% filter(n == 5000), niter = 4000, nwarmup = 1000, nchain = 4, ncore = n_cores
)

save(hbayes_sim_5000, file = "~data/volume_2/hbayes_sim_results_5000.Rdata")

hbayes_sim_10000 <- choiceRT_ddm(
  data = hbayes_simdata_rt %>% filter(n == 10000), niter = 4000, nwarmup = 1000, nchain = 4, ncore = n_cores
)

save(hbayes_sim_10000, file = "~data/volume_2/hbayes_sim_results_10000.Rdata")



# Load objects ------------------------------------------------------------
load(here("data", "staged_r_objects", "hbayes_simdata.RData"))
load(here("data", "staged_r_objects", "hbayes_sim_results_500.RData"))
load(here("data", "staged_r_objects", "hbayes_sim_results_1000.RData"))



