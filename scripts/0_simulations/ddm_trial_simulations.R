library(tidyverse)
library(RWiener)
library(runjags)
library(glue)

source("scripts/custom_functions/DBDA2E-utilities.R")


# Simulation setup --------------------------------------------------------

## Simulation parameters ----
n_trials_con1 <- 12
n_trials_con2 <- 8
n_trials_total <- n_trials_con1 + n_trials_con2
n_subj   <-  1500

## DDM settings ----
v_con1_mean  <- 3.16
v_con1_sd    <- 1.43 

v_con2_mean  <- 2.70
v_con2_sd    <- 1.54

a_mean       <- 1.10
a_sd         <- 0.35

t_con1_mean  <- 0.51
t_con1_sd    <- 0.075

t_con2_mean  <- 0.58
t_con2_sd    <- 0.09

z            <- 0.5

## Simulate data ----

set.seed(43667)

sim_parms <- 
  tibble(
    trials_con1 = rep(n_trials_con1, n_subj),
    trials_con2 = rep(n_trials_con2, n_subj),
    id          = 1:n_subj,
    a_sim       = rnorm(n_subj, a_mean, a_sd),
    z_sim       = 0.5
  ) |> 
  mutate(
    faux::rnorm_multi(n = n_subj, vars = 2, mu = c(t_con1_mean, t_con2_mean), sd = c(t_con1_sd, t_con2_sd), r = 0.7) |> 
      as_tibble() |> 
      rename(t_con1_sim = X1, t_con2_sim = X2),
    faux::rnorm_multi(n = n_subj, vars = 2, mu = c(v_con1_mean, v_con2_mean), sd = c(v_con1_sd, v_con2_sd), r = 0.3) |> 
      as_tibble() |> 
      rename(v_con1_sim = X1, v_con2_sim = X2),
  ) |> 
  filter(if_all(matches('^(a|t)'), ~. > 0))

sim_RT <- 
  sim_parms %>%
  mutate(
    responses = pmap(., function(trials_con1,trials_con2,id,v_con1_sim,v_con2_sim,a_sim,t_con1_sim,t_con2_sim,z_sim) {
      
      bind_rows(
        # Condition 1
        RWiener::rwiener(n=trials_con1, alpha=a_sim, tau=t_con1_sim, beta=z_sim, delta=v_con1_sim) |> 
          as_tibble() |> 
          mutate(
            con = 'con1'
          ),
        # Condition 2
        RWiener::rwiener(n=trials_con2, alpha=a_sim, tau=t_con2_sim, beta=z_sim, delta=v_con2_sim) |> 
          as_tibble() |> 
          mutate(
            con = 'con2'
          )
      )
    })
  ) |> 
  unnest(responses) |> 
  select(subject = id, choice = resp, RT = q, condition = con, trials_con1, trials_con2) |> 
  mutate(choice = ifelse(choice == 'upper', 1, 0)) 



# Simulation 1: 1 condition, 8 trials, N = 1,500 --------------------------

## Model Specification ----

model_sim1 <- "model {
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

## Prepare data ----
sim1_data <- sim_RT |> 
  mutate(subject = rep(1:n_subj, each = n_trials_total)) |> 
  filter(condition == "con2") # Filter the condition with 8 trials

#Change error response times to negative for JAGS weiner module
y <- round(ifelse(sim1_data$choice == 0, (sim1_data$RT*-1), sim1_data$RT),3)

#Create numbers for JAGS
nTrials <- nrow(sim1_data)
nSubjects <- length(unique(sim1_data$subject))

#Create a list of the data; this gets sent to JAGS
datalist <- list(y = y, subject = sim1_data$subject, nTrials = nTrials, 
                 nSubjects = nSubjects)

## JAGS Specifications ----

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

## Run Model
startTime = proc.time()
ddm_sim1_mod <- run.jags(method = "parallel",
                         model = model_sim1,
                         monitor = parameters,
                         data = datalist,
                         inits = initfunction,
                         n.chains = nChains,
                         adapt = 1000, #how long the samplers "tune"
                         burnin = 2000, #how long of a burn in
                         sample = 10000,
                         thin = 10, #thin if high autocorrelation to avoid huge files
                         modules = c("wiener", "lecuyer"),
                         summarise = F,
                         plots = F)
stopTime = proc.time()
elapsedTime = stopTime - startTime
show(elapsedTime/60) #Tells how long it took to run analysis


# Unpack Results ----------------------------------------------------------


#Convert the runjags object to a coda format
codaSamples <- as.mcmc.list(ddm_sim1_mod)
mcmc_sim1 <- as.matrix(codaSamples, chains = F) |> 
  as_tibble()


# Traces for convergence checks
ddm_sim1_traces <- mcmc_sim1 |> 
  select(muAlpha, muTau, muDelta) |> 
  mutate(n = rep(1:10000, 3),
         chains = rep(1:3, each = 10000))

# Combine simulated and estimated DDM parameters
ddm_sim1_data <- mcmc_sim1 |> 
  pivot_longer(everything(), names_to = "parameter", values_to = "estimated") |> 
  group_by(parameter) |> 
  summarise(estimated = mean(estimated, na.rm = T)) |> 
  filter(str_detect(parameter, pattern = 'deviance|^mu|^prec', negate = T)) |> 
  separate(col = parameter, into = c('parameter', 'id'), sep = "\\[") |> 
  mutate(id = str_remove(id, pattern = "\\]$")) |> 
  mutate(
    id = as.numeric(id),
    parameter = case_when(
      parameter == 'alpha' ~ 'a',
      parameter == 'tau' ~ 't',
      parameter == 'delta' ~ 'v'
    )) |> 
  left_join(
    sim_parms |> 
      select(id, starts_with('v_'), 'a_sim', starts_with('t_')) |> 
      pivot_longer(-id, names_to = 'parameter', values_to = 'simulated') |> 
      mutate(parameter = str_remove_all(parameter, "_sim$")) |> 
      separate(parameter, into = c('parameter', 'condition'), sep = "_") |> 
      select(-condition)
  )

ddm_sim1_cor <- ddm_sim1_data |>
  group_by(parameter) |> 
  summarise(r = cor(estimated, simulated))
  
save(ddm_sim1_traces, ddm_sim1_data, ddm_sim1_cor, file = "synthetic_data/ddm_sim1_results.RData") 





# Simulation 2: Two Conditions, N = 1,500 ---------------------------------

## Model Specification ----
model_sim2 <- "model {
  #likelihood function
  for (t in 1:nTrials) {
    y[t] ~ dwiener(alpha[subject[t]], 
                   tau[condition[t], subject[t]], 
                   0.5, 
                   delta[condition[t], subject[t]])
  }
  
  for (s in 1:nSubjects) {
    for (c in 1:nCon) {
      tau[c, s]  ~ dnorm(muTau[c], precTau) T(.0001, 1)
      delta[c, s] ~ dnorm(muDelta[c] , precDelta) T(-10, 10)
    }
    alpha[s]  ~ dnorm(muAlpha, precAlpha) T(.1, 5)
  }
  
  #priors
  for (c in 1:nCon){ 
    muTau[c] ~ dunif(.0001, 1)
    muDelta[c] ~ dunif(-10, 10)
  } 
  muAlpha~ dunif(.1, 5) 
  
  precAlpha  ~ dgamma(.001, .001)
  precTau ~ dgamma(.001, .001)
  precDelta ~ dgamma(.001, .001)
}"


## Prepare data ----

sim2_data <- sim_RT |> 
  mutate(
    condition = factor(condition),
    subject = rep(1:n_subj, each = n_trials_total))

#Change error response times to negative for JAGS weiner module
y <- round(ifelse(sim2_data$choice == 0, (sim2_data$RT*-1), sim2_data$RT),3)

#Make the condition variable; these start at 1
#Only drift rate and non-decision time vary by condition
#1 = white, 2 = black
condition <- as.numeric(sim2_data$condition)



#Create numbers for looping purposes
nTrials <- nrow(sim2_data)
nSubjects <- length(unique(sim2_data$subject))
nCondition <- max(condition)



#Create a list of the data; this gets sent to JAGS
datalist <- list(y = y, condition = condition, 
                 subject = sim2_data$subject, nTrials = nTrials, 
                 nSubjects = nSubjects, nCon = nCondition)

## JAGS Specifications ----

#Need to tell JAGS where to start the samplers
#This function choses initial values randomly
initfunction <- function(chain){
  return(list(
    muAlpha = runif(1, .2, 4.9),
    muTau = runif(2, .01, .05),
    muDelta = runif(2, -9.9, 9.9),
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
nChains = 3# Specify number of chains to run (one per processor)

#Run the model in runjags
startTime = proc.time()
ddm_sim2_mod <- run.jags(method = "parallel",
                      model = model_sim2,
                      monitor = parameters,
                      data = datalist,
                      inits = initfunction,
                      n.chains = nChains,
                      adapt = 1000, #how long the samplers "tune"
                      burnin = 2000, #how long of a burn in
                      sample = 2000,
                      thin = 1, #thin if high autocorrelation to avoid huge files
                      modules = c("wiener", "lecuyer"),
                      summarise = F,
                      plots = F)
stopTime = proc.time()
elapsedTime = stopTime - startTime
show(elapsedTime/60) #Tells how long it took to run analysis


## Unpack Results ----

#Convert the runjags object to a coda format
codaSamples <- as.mcmc.list(ddm_sim2_mod)
mcmc_sim2 <- as.matrix(codaSamples, chains = F) |> 
  as_tibble()


# Traces for convergence checks
ddm_sim2_traces <- mcmc_sim2 |> 
  select(starts_with("mu")) |> 
  mutate(
    n = rep(1:2000, 3),
    chains = rep(1:3, each = 2000)) 
  

# Combine simulated and estimated DDM parameters
ddm_sim2_data <- mcmc_sim2 |> 
  pivot_longer(everything(), names_to = "parameter", values_to = "estimated") |> 
  group_by(parameter) |> 
  summarise(estimated = mean(estimated, na.rm = T)) |> 
  filter(str_detect(parameter, pattern = 'deviance|^mu|^prec', negate = T)) |> 
  separate(col = parameter, into = c('parameter', 'id'), sep = "\\[") |> 
  mutate(
    id = str_remove(id, pattern = "\\]$"),
    id = ifelse(parameter %in% c('delta', 'tau'),
                str_replace_all(id, "([0-9]*),([0-9]*)", "\\2,\\1"),
                id
                )
    ) |> 
  separate(id, into = c('id', 'condition')) |> 
  mutate(
    id = as.numeric(id),
    parameter = case_when(
      parameter == 'alpha' ~ 'a',
      parameter == 'tau' ~ 't',
      parameter == 'delta' ~ 'v'
    )) |> 
  left_join(
    sim_parms |> 
      select(id, starts_with('v_'), 'a_sim', starts_with('t_')) |> 
      pivot_longer(-id, names_to = 'parameter', values_to = 'simulated') |> 
      mutate(parameter = str_remove_all(parameter, "_sim$")) |> 
      separate(parameter, into = c('parameter', 'condition'), sep = "_") |> 
      mutate(condition = str_remove_all(condition, "con"))
  )

ddm_sim2_cor <- ddm_sim2_data |>
  group_by(parameter) |> 
  summarise(r = cor(estimated, simulated))



# Simulation 3: Increase N to 10,000 --------------------------------------

## Simulate data ----

set.seed(430265)

n_subj <- 10000

sim_parms3 <- 
  tibble(
    trials_con1 = rep(n_trials_con1, n_subj),
    trials_con2 = rep(n_trials_con2, n_subj),
    id          = 1:n_subj,
    a_sim       = rnorm(n_subj, a_mean, a_sd),
    z_sim       = 0.5
  ) |> 
  mutate(
    faux::rnorm_multi(n = n_subj, vars = 2, mu = c(t_con1_mean, t_con2_mean), sd = c(t_con1_sd, t_con2_sd), r = 0.7) |> 
      as_tibble() |> 
      rename(t_con1_sim = X1, t_con2_sim = X2),
    faux::rnorm_multi(n = n_subj, vars = 2, mu = c(v_con1_mean, v_con2_mean), sd = c(v_con1_sd, v_con2_sd), r = 0.3) |> 
      as_tibble() |> 
      rename(v_con1_sim = X1, v_con2_sim = X2),
  ) |> 
  mutate(
    a_sim = ifelse(a_sim < 0, -1*a_sim, a_sim),
    t_con1_sim = ifelse(t_con1_sim < 0, -1*t_con1_sim, t_con1_sim),
    t_con2_sim = ifelse(t_con2_sim < 0, -1*t_con2_sim, t_con2_sim),
  ) 

sim_RT3 <- 
  sim_parms %>%
  mutate(
    responses = pmap(., function(trials_con1,trials_con2,id,v_con1_sim,v_con2_sim,a_sim,t_con1_sim,t_con2_sim,z_sim) {
      
      bind_rows(
        # Condition 1
        RWiener::rwiener(n=trials_con1, alpha=a_sim, tau=t_con1_sim, beta=z_sim, delta=v_con1_sim) |> 
          as_tibble() |> 
          mutate(
            con = 'con1'
          ),
        # Condition 2
        RWiener::rwiener(n=trials_con2, alpha=a_sim, tau=t_con2_sim, beta=z_sim, delta=v_con2_sim) |> 
          as_tibble() |> 
          mutate(
            con = 'con2'
          )
      )
    })
  ) |> 
  unnest(responses) |> 
  select(subject = id, choice = resp, RT = q, condition = con, trials_con1, trials_con2) |> 
  mutate(choice = ifelse(choice == 'upper', 1, 0)) 


# Prepare Data ------------------------------------------------------------

sim3_data <- sim_RT3 |> 
  mutate(
    condition = factor(condition),
    subject = rep(1:n_subj, each = n_trials_total))

#Change error response times to negative for JAGS weiner module
y <- round(ifelse(sim3_data$choice == 0, (sim3_data$RT*-1), sim3_data$RT),3)

#Make the condition variable; these start at 1
#Only drift rate and non-decision time vary by condition
#1 = white, 2 = black
condition <- as.numeric(sim3_data$condition)



#Create numbers for looping purposes
nTrials <- nrow(sim3_data)
nSubjects <- length(unique(sim3_data$subject))
nCondition <- max(condition)



#Create a list of the data; this gets sent to JAGS
datalist <- list(y = y, condition = condition, 
                 subject = sim3_data$subject, nTrials = nTrials, 
                 nSubjects = nSubjects, nCon = nCondition)

## JAGS Specifications ----

#Need to tell JAGS where to start the samplers
#This function choses initial values randomly
initfunction <- function(chain){
  return(list(
    muAlpha = runif(1, .2, 4.9),
    muTau = runif(2, .01, .05),
    muDelta = runif(2, -9.9, 9.9),
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
nChains = 3# Specify number of chains to run (one per processor)

#Run the model in runjags
ddm_sim3_mod <- run.jags(method = "parallel",
                         model = model_sim2,
                         monitor = parameters,
                         data = datalist,
                         inits = initfunction,
                         n.chains = nChains,
                         adapt = 1000, #how long the samplers "tune"
                         burnin = 2000, #how long of a burn in
                         sample = 2000,
                         thin = 1, #thin if high autocorrelation to avoid huge files
                         modules = c("wiener", "lecuyer"),
                         summarise = F,
                         plots = F)



## Unpack Results ----

#Convert the runjags object to a coda format
codaSamples <- as.mcmc.list(ddm_sim3_mod)
mcmc_sim3 <- as.matrix(codaSamples, chains = F) |> 
  as_tibble()


# Traces for convergence checks
ddm_sim3_traces <- mcmc_sim3 |> 
  select(starts_with("mu")) |> 
  mutate(
    n = rep(1:2000, 3),
    chains = rep(1:3, each = 2000)) 


# Combine simulated and estimated DDM parameters
ddm_sim3_data <- mcmc_sim3 |> 
  pivot_longer(everything(), names_to = "parameter", values_to = "estimated") |> 
  group_by(parameter) |> 
  summarise(estimated = mean(estimated, na.rm = T)) |> 
  filter(str_detect(parameter, pattern = 'deviance|^mu|^prec', negate = T)) |> 
  separate(col = parameter, into = c('parameter', 'id'), sep = "\\[") |> 
  mutate(
    id = str_remove(id, pattern = "\\]$"),
    id = ifelse(parameter %in% c('delta', 'tau'),
                str_replace_all(id, "([0-9]*),([0-9]*)", "\\2,\\1"),
                id
    )
  ) |> 
  separate(id, into = c('id', 'condition')) |> 
  mutate(
    id = as.numeric(id),
    parameter = case_when(
      parameter == 'alpha' ~ 'a',
      parameter == 'tau' ~ 't',
      parameter == 'delta' ~ 'v'
    )) |> 
  left_join(
    sim_parms3 |> 
      select(id, starts_with('v_'), 'a_sim', starts_with('t_')) |> 
      pivot_longer(-id, names_to = 'parameter', values_to = 'simulated') |> 
      mutate(parameter = str_remove_all(parameter, "_sim$")) |> 
      separate(parameter, into = c('parameter', 'condition'), sep = "_") |> 
      mutate(condition = str_remove_all(condition, "con"))
  )

ddm_sim3_cor <- ddm_sim3_data |>
  group_by(parameter) |> 
  summarise(r = cor(estimated, simulated))



save(ddm_sim3_traces, ddm_sim3_data, ddm_sim3_cor, file = "synthetic_data/ddm_sim3_results.RData") 

  

