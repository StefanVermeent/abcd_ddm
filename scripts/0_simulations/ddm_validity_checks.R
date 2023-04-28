library(tidyverse)
library(RWiener)
library(runjags)
library(glue)
library(lmerTest)

source("scripts/custom_functions/DBDA2E-utilities.R")

# Simulation setup --------------------------------------------------------
## DDM settings ----
v_mean  <- 3.16
v_sd    <- 1.43 

a_mean  <- 1.10
a_sd    <- 0.35

t_mean  <- 0.51
t_sd    <- 0.075

z       <- 0.5

# Potential Shrinkage of high-adversity performance -----------------------

## Simulation 4: Lognormal adversity distribution ----

n_trials <- 20
n_subj   <-  1500
adversity_effect <- 0.1 # effect size of 0.1

set.seed(43567)


sim_parms4 <- 
  tibble(
    trials    = rep(n_trials, n_subj),
    id        = 1:n_subj,
    adversity = rlnorm(n_subj, 0, 0.3) |> scale() |> as.numeric(),
    intercept = rnorm(n_subj, v_mean, v_sd*1),
    v_sim     = intercept + (adversity_effect*adversity) + rnorm(n_subj, 0, 1),    
    a_sim     = rnorm(n_subj, a_mean, a_sd),
    t_sim     = rnorm(n_subj, t_mean, t_sd),
    z_sim     = 0.5
  ) |> 
  select(-intercept) |> 
  filter(if_all(matches('^(a_|t_)'), ~. > 0)) |> 
  mutate(id = 1:n())




sim_RT4 <- 
  sim_parms4 %>%
  mutate(
    responses = pmap(., function(trials,id,adversity,v_sim,a_sim,t_sim,z_sim) {
        RWiener::rwiener(n=trials, alpha=a_sim, tau=t_sim, beta=z_sim, delta=v_sim) |> 
          as_tibble() 
    })
  ) |> 
  unnest(responses) |> 
  select(subject = id, choice = resp, RT = q, adversity, trials) |> 
  mutate(choice = ifelse(choice == 'upper', 1, 0)) 

### Fit the model ----
model_sim4 <- "model {
  #likelihood function
  for (t in 1:nTrials) {
    y[t] ~ dwiener(alpha[subject[t]], 
                   tau[subject[t]], 
                   0.5, 
                   delta[subject[t]])
  }
  
  for (s in 1:nSubjects) {
    tau[s]  ~ dnorm(muTau, precTau) T(.0001, 1)
    delta[s] ~ dnorm(muDelta, precDelta) T(-10, 10)
    alpha[s]  ~ dnorm(muAlpha, precAlpha) T(.1, 5)
  }
  
  #priors
  muTau ~ dunif(.0001, 1)
  muDelta ~ dunif(-10, 10)
  muAlpha~ dunif(.1, 5) 
  
  precAlpha  ~ dgamma(.001, .001)
  precTau ~ dgamma(.001, .001)
  precDelta ~ dgamma(.001, .001)
}"

### Prepare data ----
sim4_data <- sim_RT4 |> 
  mutate(subject = rep(1:length(unique(sim_RT4$subject)), each = n_trials)) 

#Change error response times to negative for JAGS weiner module
y <- round(ifelse(sim4_data$choice == 0, (sim4_data$RT*-1), sim4_data$RT),3)

#Create numbers for JAGS
nTrials <- nrow(sim4_data)
nSubjects <- length(unique(sim4_data$subject))

#Create a list of the data; this gets sent to JAGS
datalist <- list(y = y, subject = sim4_data$subject, nTrials = nTrials, 
                 nSubjects = nSubjects)

### JAGS Specifications ----

#Need to tell JAGS where to start the samplers
#This function choses initial values randomly
initfunction <- function(chain){
  return(list(
    muAlpha = runif(1, .2, 4.9),
    muTau = runif(1, .01, .05),
    muDelta = runif(1, -9.9, 9.9),
    precAlpha = runif(1, .01, 100),
    precTau = runif(1, .01, 100),
    precDelta = runif(1, .01, 100),
    y = rep(NA, length(y)),
    .RNG.name = "lecuyer::RngStream",
    .RNG.seed = sample.int(1e10, 1, replace = F)))
}

#Create list of parameters to be monitored
parameters <- c("alpha", "tau", "delta", "muAlpha",
                "muTau", "muDelta", "precAlpha", "precTau", "precDelta", 
                "deviance")

nUseSteps = 1000 # Specify number of steps to run
nChains = 3 # Specify number of chains to run (one per processor)

### Run Model ----
startTime = proc.time()
ddm_sim4_mod <- run.jags(method = "parallel",
                         model = model_sim4,
                         monitor = parameters,
                         data = datalist,
                         inits = initfunction,
                         n.chains = nChains,
                         adapt = 1000, #how long the samplers "tune"
                         burnin = 2000, #how long of a burn in
                         sample = 2000,
                         modules = c("wiener", "lecuyer"),
                         summarise = F,
                         plots = F)
stopTime = proc.time()
elapsedTime = stopTime - startTime
show(elapsedTime/60) #Tells how long it took to run analysis

### Unpack Results ----

#Convert the runjags object to a coda format
codaSamples <- as.mcmc.list(ddm_sim4_mod)
mcmc_sim4 <- as.matrix(codaSamples, chains = F) |> 
  as_tibble()


# Traces for convergence checks
ddm_sim4_traces <- mcmc_sim4 |> 
  select(starts_with("mu")) |> 
  mutate(
    n = rep(1:2000, 3),
    chains = rep(1:3, each = 2000)) 


# Combine simulated and estimated DDM parameters
ddm_sim4_data <- mcmc_sim4 |> 
  pivot_longer(everything(), names_to = "parameter", values_to = "estimated") |> 
  group_by(parameter) |> 
  summarise(estimated = mean(estimated, na.rm = T)) |> 
  filter(str_detect(parameter, pattern = 'deviance|^mu|^prec', negate = T)) |> 
  separate(col = parameter, into = c('parameter', 'id'), sep = "\\[") |> 
  mutate(
    id = str_remove(id, pattern = "\\]$"),
  ) |> 
  mutate(
    id = as.numeric(id),
    parameter = case_when(
      parameter == 'alpha' ~ 'a',
      parameter == 'tau' ~ 't',
      parameter == 'delta' ~ 'v'
    )) |> 
  left_join(
    sim_parms4 |> 
      select(id, v_sim, a_sim, t_sim, adversity) |> 
      mutate(id = seq(1, n(), 1)) |> 
      pivot_longer(-c(adversity,id), names_to = 'parameter', values_to = 'simulated') |> 
      mutate(parameter = str_remove_all(parameter, "_sim$")) 
  ) |> 
  mutate(deviation = simulated-estimated) 


save(sim_RT4, ddm_sim4_traces, ddm_sim4_data, file = "analysis_objects/ddm_sim4_results.RData") 



