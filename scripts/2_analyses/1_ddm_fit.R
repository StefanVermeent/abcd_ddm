lmt_clean     <- readr::read_csv(paste0("data/lmt_clean", data_suffix, ".csv"))
flanker_clean <- readr::read_csv(paste0("data/flanker_clean", data_suffix, ".csv"))
pcps_clean    <- readr::read_csv(paste0("data/pcps_clean", data_suffix, ".csv"))
dccs_clean    <- readr::read_csv(paste0("data/dccs_clean", data_suffix, ".csv"))


# 1. Overview of Model Specifications ----------------------------------------

## 1.1 Standard model, no condition effects ----

# Note: This model will be used for the Processing Speed and Mental Rotation Task

mod_base_1con <- "model {
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

## 1.2 Standard model including condition effects ----

# Note: This model will be used for the Flanker Task and Attention Shifting Task

mod_base_2con <- "model {
  #likelihood function
  for (t in 1:nTrials) {
    y[t] ~ dwiener(alpha[subject[t]], 
                   tau[condition[t], subject[t]], 
                   0.5, 
                   delta[condition[t], subject[t]])
  }
  for (s in 1:nSubjects) {
    for (c in 1:nCon) {
      tau[c, s]  ~ dnorm(muTau[c], precTau) T(.0001, 2)
      delta[c, s] ~ dnorm(muDelta[c] , precDelta) T(-10, 10)
    }
    alpha[s]  ~ dnorm(muAlpha, precAlpha) T(.1, 5)
  }
  #priors
  for (c in 1:nCon){ 
    muTau[c] ~ dunif(.0001, 2)
    muDelta[c] ~ dunif(-10, 10)
  } 
  muAlpha~ dunif(.1, 5) 
  
  precAlpha  ~ dgamma(.001, .001)
  precTau ~ dgamma(.001, .001)
  precDelta ~ dgamma(.001, .001)
}"


## 1.3 Model with missingness imputation

# Note: This model will be used for the Mental Rotation Task to fix the truncated RT distribution

mod_base_1con_impute <- "model {
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

initfunction_1con <- function(chain){
  return(list(
    muAlpha = runif(1, .2, 4.9),
    muTau = runif(1, .01, 1.05),
    muDelta = runif(1, -9.9, 9.9),
    precAlpha = runif(1, .01, 100),
    precTau = runif(1, .01, 100),
    precDelta = runif(1, .01, 100),
    y = yInit,
    .RNG.name = "lecuyer::RngStream",
    .RNG.seed = sample.int(1e10, 1, replace = F)))
}
initfunction_2con <- function(chain){
  return(list(
    muAlpha = runif(1, .2, 4.9),
    muTau = runif(2, .01, 1.05),
    muDelta = runif(2, -9.9, 9.9),
    precAlpha = runif(1, .01, 100),
    precTau = runif(1, .01, 100),
    precDelta = runif(1, .01, 100),
    y = yInit,
    .RNG.name = "lecuyer::RngStream",
    .RNG.seed = sample.int(1e10, 1, replace = F)))
}

initfunction_1con_impute <- function(chain){
  return(list(
    muAlpha = runif(1, .2, 4.9),
    muTau = runif(1, .01, 1.05),
    muDelta = runif(1, -9.9, 9.9),
    precAlpha = runif(1, .01, 100),
    precTau = runif(1, .01, 100),
    precDelta = runif(1, .01, 100),
    y = yInit_impute,
    .RNG.name = "lecuyer::RngStream",
    .RNG.seed = sample.int(1e10, 1, replace = F)))
}

# 2. DDM Estimation ----------------------------------------------------------

## 2.1 Mental Rotation Task ----

### 2.1.2 Model 1: Imputed missings ----

# Create numeric participant IDs
lmt_id_matches <- lmt_clean |> 
  distinct(subj_idx) |> 
  mutate(subj_idx_num = 1:n())

lmt_clean <- lmt_clean |> 
  left_join(lmt_id_matches)


#Calculations below are necessary because data are censored beyond 5 seconds
#We probabilistically assign 0 (incorrect) or 1 (correct) to missing trials to estimate data

set.seed(37843564)
lmt_clean_impute <- lmt_clean |> 
  group_by(subj_idx) |> 
  mutate(
    # If response is missing, impute correctness probabilistically
    prop = sum(correct)/n(),
    correct = ifelse(is.na(RT),
                     sample(c(0,1), prob = c(1-sum(correct)/n(), sum(correct)/n()), size = 100, replace = T),
                     correct)
  )

# Store RTs and condition per trial (incorrect RTs are coded negatively)
lmt_y_impute <- round(ifelse(lmt_clean_impute$correct == 0, (lmt_clean_impute$RT*-1), lmt_clean_impute$RT),3)

lmt_ybin_impute = 1:nrow(lmt_clean_impute) |> 
  map_dbl(function(x){
    if(is.na(lmt_clean_impute$RT[x])) {
      if(lmt_clean_impute$correct[x] == 0) 0 else 2
    } else 1
  })

#Create a matrix of cutoff times for JAGS
threshMat_impute <- as.matrix(data.frame(thresh1 = rep(-5.001, nrow(lmt_clean_impute)), 
                                  thresh2 = rep(5.001, nrow(lmt_clean_impute))))

#Create initial values for missing data
yInit_impute = 1:nrow(lmt_clean_impute) |> 
  map_dbl(function(x){
    if(is.na(lmt_y_impute[x])) { # If RT has been cut off
      if(lmt_ybin_impute[x] == 0) { # And has been imputed as 'incorrect'
        threshMat_impute[x,1]-.001 # Initialize below the threshold
      } else {
        threshMat_impute[x,2]+.001
      }
    } else NA
  })
#End of data censoring procedure

#Create numbers for JAGS
lmt_nTrials    <- nrow(lmt_clean_impute)
lmt_nSubjects  <- length(unique(lmt_clean_impute$subj_idx))

#Create a list of the data; this gets sent to JAGS
lmt_datalist <- list(y = lmt_y_impute, ybin = lmt_ybin_impute, threshMat = threshMat_impute,
                     subject = lmt_clean_impute$subj_idx_num,
                     nTrials = lmt_nTrials, nSubjects = lmt_nSubjects)

# JAGS Specifications

#Create list of parameters to be monitored
parameters <- c("alpha", "tau", "delta", "muAlpha",
                "muTau", "muDelta", "precAlpha", "precTau", "precDelta", 
                "deviance")

nUseSteps = 1000 # Specify number of steps to run
nChains = 3 # Specify number of chains to run (one per processor)

# Run Model
ddm_lmt_mod1 <- run.jags(method = "parallel",
                         model = mod_base_1con_impute,
                         monitor = parameters,
                         data = lmt_datalist,
                         inits = initfunction_1con_impute,
                         n.chains = nChains,
                         adapt = 1000, #how long the samplers "tune"
                         burnin = 2000, #how long of a burn in
                         sample = 1000,
                         thin = 10, #thin if high autocorrelation to avoid huge files
                         modules = c("wiener", "lecuyer"),
                         summarise = F,
                         plots = F)


# Extract results

#Convert the runjags object to a coda format
mcmc_lmt_mod1 <- as.matrix(as.mcmc.list(ddm_lmt_mod1), chains = F) |> 
  as_tibble()


save(mcmc_lmt_mod1, file = 'analysis_objects/ddm_lmt_mod1.RData')


### 2.1.3 Model 2: Without imputing missings ----

# Dataset without missings for model that does not do imputation
lmt_clean_noImp <- lmt_clean |> 
  filter(!is.na(RT))

lmt_y          <- round(ifelse(lmt_clean_noImp$correct == 0, (lmt_clean_noImp$RT*-1), lmt_clean_noImp$RT),3)
yInit          <- rep(NA, length(lmt_y))

#Create numbers for JAGS
lmt_nTrials    <- nrow(lmt_clean_noImp)
lmt_nSubjects  <- length(unique(lmt_clean_noImp$subj_idx))

#Create a list of the data; this gets sent to JAGS
lmt_datalist_noImp <- list(y = lmt_y, subject = lmt_clean_noImp$subj_idx_num,
                           nTrials = lmt_nTrials, nSubjects = lmt_nSubjects)

# JAGS Specifications

#Create list of parameters to be monitored
parameters <- c("alpha", "tau", "delta", "muAlpha",
                "muTau", "muDelta", "precAlpha", "precTau", "precDelta", 
                "deviance")

nUseSteps = 1000 # Specify number of steps to run
nChains = 3 # Specify number of chains to run (one per processor)

# Run Model
ddm_lmt_mod2 <- run.jags(method = "parallel",
                         model = mod_base_1con,
                         monitor = parameters,
                         data = lmt_datalist_noImp,
                         inits = initfunction_1con,
                         n.chains = nChains,
                         adapt = 1000, #how long the samplers "tune"
                         burnin = 2000, #how long of a burn in
                         sample = 10000,
                         thin = 10, #thin if high autocorrelation to avoid huge files
                         modules = c("wiener", "lecuyer"),
                         summarise = F,
                         plots = F)

# Extract results

#Convert the runjags object to a coda format
mcmc_lmt_mod2 <- as.matrix(as.mcmc.list(ddm_lmt_mod2), chains = F) |> 
  as_tibble()

save(mcmc_lmt_mod2, file = 'analysis_objects/ddm_lmt_mod2.RData')





## 2.2 Flanker Task ----

### 2.2.1 Model 1: Separate conditions ----

# Create numeric participant IDs
flanker_id_matches <- flanker_clean |> 
  distinct(subj_idx) |> 
  mutate(subj_idx_num = 1:n())

flanker_clean <- flanker_clean |> 
  mutate(condition = ifelse(condition == 'congruent', 1, 2)) |>
  left_join(flanker_id_matches)

# Store RTs and condition per trial (incorrect RTs are coded negatively)
flanker_y          <- round(ifelse(flanker_clean$correct == 0, (flanker_clean$RT*-1), flanker_clean$RT),3)
yInit              <- rep(NA, length(flanker_y))
flanker_condition  <- as.numeric(flanker_clean$condition)

#Create numbers for JAGS
flanker_nTrials    <- nrow(flanker_clean)
flanker_nSubjects  <- length(unique(flanker_clean$subj_idx))
flanker_nCondition <- max(flanker_condition)

#Create a list of the data; this gets sent to JAGS
flanker_datalist <- list(y = flanker_y, subject = flanker_clean$subj_idx_num, condition = flanker_condition,
                         nTrials = flanker_nTrials, nCon = flanker_nCondition,
                         nSubjects = flanker_nSubjects)

# JAGS Specifications

#Create list of parameters to be monitored
parameters <- c("alpha", "tau", "delta", "muAlpha",
                "muTau", "muDelta", "precAlpha", "precTau", "precDelta", 
                "deviance")

nUseSteps = 1000 # Specify number of steps to run
nChains = 3 # Specify number of chains to run (one per processor)

# Run Model
ddm_flanker_mod1 <- run.jags(method = "parallel",
                             model = mod_base_2con,
                             monitor = parameters,
                             data = flanker_datalist,
                             inits = initfunction_2con,
                             n.chains = nChains,
                             adapt = 1000, #how long the samplers "tune"
                             burnin = 2000, #how long of a burn in
                             sample = 1000,
                             thin = 10, #thin if high autocorrelation to avoid huge files
                             modules = c("wiener", "lecuyer"),
                             summarise = F,
                             plots = F)

# Extract results

#Convert the runjags object to a coda format
mcmc_flanker_mod1 <- as.matrix(as.mcmc.list(ddm_flanker_mod1), chains = F) |> 
  as_tibble()

save(mcmc_flanker_mod1, ddm_flanker_mod1, file = 'analysis_objects/ddm_flanker_mod1.RData')


diagMCMC(as.mcmc.list(ddm_flanker_mod1))


### 2.2.2 Model 2: Collapse across conditions ----

# Run Model
ddm_flanker_mod2 <- run.jags(method = "parallel",
                             model = mod_base_1con,
                             monitor = parameters,
                             data = flanker_datalist,
                             inits = initfunction_1con,
                             n.chains = nChains,
                             adapt = 1000, #how long the samplers "tune"
                             burnin = 2000, #how long of a burn in
                             sample = 1000,
                             thin = 10, #thin if high autocorrelation to avoid huge files
                             modules = c("wiener", "lecuyer"),
                             summarise = F,
                             plots = F)

# Extract results

#Convert the runjags object to a coda format
mcmc_flanker_mod2 <- as.matrix(as.mcmc.list(ddm_flanker_mod2), chains = F) |> 
  as_tibble()

save(mcmc_flanker_mod2, ddm_flanker_mod2, file = 'analysis_objects/ddm_flanker_mod2.RData')


diagMCMC(as.mcmc.list(ddm_flanker_mod2))

## 2.3 Processing Speed Task ----

# Create numeric participant IDs
pcps_id_matches <- pcps_clean |> 
  distinct(subj_idx) |> 
  mutate(subj_idx_num = 1:n())

pcps_clean <- pcps_clean |> 
  left_join(pcps_id_matches)

# Store RTs and condition per trial (incorrect RTs are coded negatively)
pcps_y          <- round(ifelse(pcps_clean$correct == 0, (pcps_clean$RT*-1), pcps_clean$RT),3)
yInit             <- rep(NA, length(pcps_y))

#Create numbers for JAGS
pcps_nTrials    <- nrow(pcps_clean)
pcps_nSubjects  <- length(unique(pcps_clean$subj_idx))

#Create a list of the data; this gets sent to JAGS
pcps_datalist <- list(y = pcps_y, subject = pcps_clean$subj_idx_num,
                      nTrials = pcps_nTrials, nSubjects = pcps_nSubjects)

# JAGS Specifications

#Create list of parameters to be monitored
parameters <- c("alpha", "tau", "delta", "muAlpha",
                "muTau", "muDelta", "precAlpha", "precTau", "precDelta", 
                "deviance")

nUseSteps = 1000 # Specify number of steps to run
nChains = 3 # Specify number of chains to run (one per processor)

# Run Model
ddm_pcps_mod1 <- run.jags(method = "parallel",
                          model = mod_base_1con,
                          monitor = parameters,
                          data = pcps_datalist,
                          inits = initfunction_1con,
                          n.chains = nChains,
                          adapt = 1000, #how long the samplers "tune"
                          burnin = 2000, #how long of a burn in
                          sample = 12000,
                          thin = 10, #thin if high autocorrelation to avoid huge files
                          modules = c("wiener", "lecuyer"),
                          summarise = F,
                          plots = F)

# Extract results

#Convert the runjags object to a coda format
mcmc_pcps_mod1 <- as.matrix(as.mcmc.list(ddm_pcps_mod1), chains = F) |> 
  as_tibble()

save(mcmc_pcps_mod1, file = 'analysis_objects/ddm_pcps_mod1.RData')


## 2.4 Attention Shifting Task ----

### 2.4.1 Model 1: Separate conditions ----

# Create numeric participant IDs
dccs_id_matches <- dccs_clean |> 
  distinct(subj_idx) |> 
  mutate(subj_idx_num = 1:n())

dccs_clean <- dccs_clean |> 
  mutate(condition = ifelse(condition == 'repeat', 1, 2)) |>
  left_join(dccs_id_matches)

# Store RTs and condition per trial (incorrect RTs are coded negatively)
dccs_y          <- round(ifelse(dccs_clean$correct == 0, (dccs_clean$RT*-1), dccs_clean$RT),3)
yInit             <- rep(NA, length(dccs_y))
dccs_condition  <- as.numeric(dccs_clean$condition)

#Create numbers for JAGS
dccs_nTrials    <- nrow(dccs_clean)
dccs_nSubjects  <- length(unique(dccs_clean$subj_idx))
dccs_nCondition <- max(dccs_condition)

#Create a list of the data; this gets sent to JAGS
dccs_datalist <- list(y = dccs_y, subject = dccs_clean$subj_idx_num, condition = dccs_condition,
                      nTrials = dccs_nTrials, nCon = dccs_nCondition,
                      nSubjects = dccs_nSubjects)

# JAGS Specifications

#Create list of parameters to be monitored
parameters <- c("alpha", "tau", "delta", "muAlpha",
                "muTau", "muDelta", "precAlpha", "precTau", "precDelta", 
                "deviance")

nUseSteps = 1000 # Specify number of steps to run
nChains = 3 # Specify number of chains to run (one per processor)

# Run Model
ddm_dccs_mod1 <- run.jags(method = "parallel",
                          model = mod_base_2con,
                          monitor = parameters,
                          data = dccs_datalist,
                          inits = initfunction_2con,
                          n.chains = nChains,
                          adapt = 1000, #how long the samplers "tune"
                          burnin = 2000, #how long of a burn in
                          sample = 1000,
                          thin = 10, #thin if high autocorrelation to avoid huge files
                          modules = c("wiener", "lecuyer"),
                          summarise = F,
                          plots = F)

# Extract results

#Convert the runjags object to a coda format
mcmc_dccs_mod1 <- as.matrix(as.mcmc.list(ddm_dccs_mod1), chains = F) |> 
  as_tibble()

save(ddm_dccs_mod1, mcmc_dccs_mod1, file = 'analysis_objects/ddm_dccs_mod1.RData')


### 2.4.2 Model 2: Collapse across conditions ----

# Run Model
ddm_dccs_mod2 <- run.jags(method = "parallel",
                          model = mod_base_1con,
                          monitor = parameters,
                          data = dccs_datalist,
                          inits = initfunction_1con,
                          n.chains = nChains,
                          adapt = 1000, #how long the samplers "tune"
                          burnin = 2000, #how long of a burn in
                          sample = 1000,
                          thin = 10, #thin if high autocorrelation to avoid huge files
                          modules = c("wiener", "lecuyer"),
                          summarise = F,
                          plots = F)

# Extract results

#Convert the runjags object to a coda format
mcmc_dccs_mod2 <- as.matrix(as.mcmc.list(ddm_dccs_mod2), chains = F) |> 
  as_tibble()

save(ddm_dccs_mod2, mcmc_dccs_mod2, file = 'analysis_objects/ddm_dccs_mod2.RData')
