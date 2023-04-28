library(tidyverse)
library(RWiener)

source("scripts/custom_functions/DBDA2E-utilities.R")

lmt_clean <- readr::read_csv(paste0("data/lmt_clean", data_suffix, ".csv"))

lmt_clean |> 
  ggplot(aes(RT)) +
  geom_histogram()

lmt_missings <- lmt_clean |> 
  group_by(subj_idx) |> 
  summarise(n_na = sum(is.na(RT))/n())

ggplot(lmt_missings, aes(n_na)) +
  geom_histogram(bins = 50) +
  theme_classic() +
  labs(
    x = "\nNumber of missings",
    y = "Frequency\n"
  )

m_rt <- mean(lmt_clean$RT, na.rm = T)
m_acc <- sum(lmt_clean$correct==1)/nrow(lmt_clean)
m_na <- mean(lmt_missings$n_na)


# Simulate Missings -------------------------------------------------------

# Simulation setup --------------------------------------------------------
## DDM settings ----

set.seed(8466)

v_mean  <- 0.4
v_sd    <- 0.7

a_mean  <- 3
a_sd    <- 0.35
t_mean  <- 1
t_sd    <- 0.075
z       <- 0.5

n_trials <- 32

sim_parms5 <- tibble(
  id           = 1:1500,
  trials       = n_trials,
  v_sim        = rnorm(1500, v_mean, v_sd),
  a_sim       = rnorm(1500, a_mean, a_sd),
  t_sim       = rnorm(1500, t_mean, t_sd)
  )


# Data set containing all data
sim_RT5_complete <- 
  sim_parms5 %>%
  mutate(
    responses = pmap(., function(id,trials,v_sim,a_sim,t_sim) {
      data <- RWiener::rwiener(n=trials,alpha=a_sim, tau=t_sim, beta=0.5, delta=v_sim) |> 
        as_tibble() 
      
      
    })
  ) |> 
  unnest(responses) |> 
  select(subject = id, choice = resp, RT = q,trials) |> 
  mutate(choice = ifelse(choice == 'upper', 1, 0)) 

# Data set with RTs > 5 set to missing
sim_RT5_missing <- sim_RT5_complete |> 
  mutate(
    RT = ifelse(RT > 5, NA, RT)
  )



# Fit model to complete data ----------------------------------------------

model_sim5 <- "model {
  #likelihood function
  for (t in 1:nTrials) {
    y[t] ~ dwiener(alpha[subject[t]], 
                   tau[subject[t]], 
                   0.5, 
                   delta[subject[t]])
  }
  
  for (s in 1:nSubjects) {
    tau[s]  ~ dnorm(muTau, precTau) T(.0001, 2)
    delta[s] ~ dnorm(muDelta, precDelta) T(-10, 10)
    alpha[s]  ~ dnorm(muAlpha, precAlpha) T(.1, 5)
  }
  
  #priors
  muTau ~ dunif(.0001, 2)
  muDelta ~ dunif(-10, 10)
  muAlpha~ dunif(.1, 5) 
  
  precAlpha  ~ dgamma(.001, .001)
  precTau ~ dgamma(.001, .001)
  precDelta ~ dgamma(.001, .001)
}"

### Prepare data ----
sim5_data_complete <- sim_RT5_complete |> 
  mutate(subject = rep(1:length(unique(sim_RT5_complete$subject)), each = n_trials)) 

#Change error response times to negative for JAGS weiner module
y <- round(ifelse(sim5_data_complete$choice == 0, (sim5_data_complete$RT*-1), sim5_data_complete$RT),3)

#Create numbers for JAGS
nTrials <- nrow(sim5_data_complete)
nSubjects <- length(unique(sim5_data_complete$subject))

#Create a list of the data; this gets sent to JAGS
datalist <- list(y = y, subject = sim5_data_complete$subject, nTrials = nTrials, 
                 nSubjects = nSubjects)

### JAGS Specifications ----

#Need to tell JAGS where to start the samplers
#This function choses initial values randomly
initfunction <- function(chain){
  return(list(
    muAlpha = runif(1, .2, 4.9),
    muTau = runif(1, .01, 1.05),
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
ddm_sim5_mod_complete <- run.jags(method = "parallel",
                         model = model_sim5,
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
codaSamples <- as.mcmc.list(ddm_sim5_mod_complete)
mcmc_sim5_complete <- as.matrix(codaSamples, chains = F) |> 
  as_tibble()


# Traces for convergence checks
ddm_sim5_traces_complete <- mcmc_sim5_complete |> 
  select(starts_with("mu")) |> 
  mutate(
    n = rep(1:2000, 3),
    chains = rep(1:3, each = 2000)) 


# Combine simulated and estimated DDM parameters
ddm_sim5_data_complete <- mcmc_sim5_complete |> 
  pivot_longer(everything(), names_to = "parameter", values_to = "est_complete") |> 
  group_by(parameter) |> 
  summarise(est_complete = mean(est_complete, na.rm = T)) |> 
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
    sim_parms5 |> 
      select(id, v_sim, a_sim, t_sim) |> 
      mutate(id = seq(1, n(), 1)) |> 
      pivot_longer(-c(id), names_to = 'parameter', values_to = 'simulated') |> 
      mutate(parameter = str_remove_all(parameter, "_sim$")) 
  )

save(ddm_sim5_data_complete,ddm_sim5_traces_complete,mcmc_sim5_complete, file = 'analysis_objects/results_sim5.RData')



# Fit model to incomplete data --------------------------------------------

model_sim5_impute <- "model {
  #likelihood function
  for (t in 1:nTrials) {
    ybin[t] ~ dinterval(y[t], threshMat[t,]) 
    y[t] ~ dwiener(alpha[subject[t]], 
                   tau[subject[t]], 
                   0.5, 
                   delta[subject[t]])
  }
  for (s in 1:nSubjects) {
    tau[s]  ~ dnorm(muTau, precTau) T(.0001, 2)
    delta[s] ~ dnorm(muDelta, precDelta) T(-10, 10)
    alpha[s]  ~ dnorm(muAlpha, precAlpha) T(.1, 5)
  }
  
  #priors
  muTau ~ dunif(.0001, 2)
  muDelta ~ dunif(-10, 10)
  muAlpha~ dunif(.1, 5) 
  
  precAlpha  ~ dgamma(.001, .001)
  precTau ~ dgamma(.001, .001)
  precDelta ~ dgamma(.001, .001)
}"

set.seed(374)
sim5_data_missing <- sim_RT5_missing |> 
  group_by(subject) |> 
  mutate(
    # If response is missing, impute correctness probabilistically
    prop = sum(choice)/n(),
    choice = ifelse(is.na(RT),
                     sample(c(0,1), prob = c(1-sum(choice)/n(), sum(choice)/n()), size = 100, replace = T),
                     choice)
  )

# Store RTs and condition per trial (incorrect RTs are coded negatively)
y_impute <- round(ifelse(sim_RT5_missing$choice == 0, (sim_RT5_missing$RT*-1), sim_RT5_missing$RT),3)

ybin_impute = 1:nrow(sim_RT5_missing) |> 
  map_dbl(function(x){
    if(is.na(sim_RT5_missing$RT[x])) {
      if(sim_RT5_missing$choice[x] == 0) 0 else 2
    } else 1
  })

#Create a matrix of cutoff times for JAGS
threshMat_impute <- as.matrix(data.frame(thresh1 = rep(-5.001, nrow(sim_RT5_missing)), 
                                  thresh2 = rep(5.001, nrow(sim_RT5_missing))))

#Create initial values for missing data
yInit_impute = 1:nrow(sim_RT5_missing) |> 
  map_dbl(function(x){
    if(is.na(y_impute[x])) { # If RT has been cut off
      if(ybin_impute[x] == 0) { # And has been imputed as 'incorrect'
        threshMat_impute[x,1]-.001 # Initialize below the threshold
      } else {
        threshMat_impute[x,2]+.001
      }
    } else NA
  })
#End of data censoring procedure


#Create numbers for JAGS
nTrials_impute <- nrow(sim5_data_missing)
nSubjects_impute <- length(unique(sim5_data_missing$subject))

#Create a list of the data; this gets sent to JAGS
datalist_impute <- list(y = y_impute, ybin = ybin_impute,  threshMat = threshMat_impute, 
                 subject = sim5_data_missing$subject, nTrials = nTrials_impute, 
                 nSubjects = nSubjects_impute)

### JAGS Specifications ----

#Need to tell JAGS where to start the samplers
#This function choses initial values randomly
initfunction_impute <- function(chain){
  return(list(
    muAlpha = runif(1, .2, 4.9),
    muTau = runif(1, .01, .05),
    muDelta = runif(1, -9.9, 9.9),
    precAlpha = runif(1, .01, 100),
    precTau = runif(1, .01, 100),
    precDelta = runif(1, .01, 100),
    y = yInit_impute,
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
ddm_sim5_mod_missing <- run.jags(method = "parallel",
                                  model = model_sim5_impute,
                                  monitor = parameters,
                                  data = datalist_impute,
                                  inits = initfunction_impute,
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
codaSamples <- as.mcmc.list(ddm_sim5_mod_missing)
mcmc_sim5_missing <- as.matrix(codaSamples, chains = F) |> 
  as_tibble()


# Traces for convergence checks
ddm_sim5_traces_missing <- mcmc_sim5_missing |> 
  select(starts_with("mu")) |> 
  mutate(
    n = rep(1:2000, 3),
    chains = rep(1:3, each = 2000)) 


# Combine simulated and estimated DDM parameters
ddm_sim5_data_missing <- mcmc_sim5_missing |> 
  pivot_longer(everything(), names_to = "parameter", values_to = "est_missing") |> 
  group_by(parameter) |> 
  summarise(est_missing = mean(est_missing, na.rm = T)) |> 
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
    sim_parms5 |> 
      select(id, v_sim, a_sim, t_sim) |> 
      mutate(id = seq(1, n(), 1)) |> 
      pivot_longer(-c(id), names_to = 'parameter', values_to = 'simulated') |> 
      mutate(parameter = str_remove_all(parameter, "_sim$")) 
  )

ddm_sim5_data <- 
  left_join(ddm_sim5_data_complete, ddm_sim5_data_missing)

save(
  sim_RT5_complete, ddm_sim5_traces_complete, ddm_sim5_data_complete,
  sim_RT5_missing, ddm_sim5_traces_missing, ddm_sim5_data_missing,
  ddm_sim5_data,
  file = "analysis_objects/ddm_sim5_results.RData"
)
