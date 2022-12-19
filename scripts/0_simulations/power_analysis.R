
# Libraries ---------------------------------------------------------------

library(lavaan)
library(tidyverse)
library(furrr)
library(RWiener)



# Simulation settings -----------------------------------------------------

nsim    <- 1:500
nSample <- c(1500, 2500, 3500, 4500, 5500, 6500, 7500, 8500)
beta    <- c(0.06, 0.08, 0.1)

cores   <-  availableCores() - 2
plan("multisession", workers = cores) # Use parallel processing


### The Looping function below does five things:
### 1. Specify the population model, which specifies factor loadings and adversity-cognition associations at the population level.
###    Beta-coefficients are fixed, but factor loadings are randomly generated between 0.6 and 0.95 following a uniform distribution.
### 2. Specify the sample model, which is identical to the population model but without the parameter specifications (these will be estimated)
### 3. Simulate data based on the population model.
### 4. Fit the sample model to the data.
### 5. Store results

set.seed(38584654)

power_results <- 
  expand_grid(sim = nsim, n = nSample, beta = beta) |> 
  mutate(sim = 1:n()) %>% 
  mutate(
    results = future_pmap(.l = ., function(sim, n, beta){
      
      # 1. SPECIFY POPULATION MODEL
      population_model <-
        paste0(
          ' 
      # General Drift rate factor
      v_general =~ NA*pcps_v + ',runif(1, 0.6, 0.8),'*dccs_v + ',runif(1, 0.6, 0.8),'*rotation_v + ',runif(1, 0.6, 0.8),'*flanker_v ',
      '\na_general =~ NA*pcps_a + ',runif(1, 0.6, 0.8),'*dccs_a + ',runif(1, 0.6, 0.8),'*rotation_a + ',runif(1, 0.6, 0.8),'*flanker_a ',
      '\nt0_general =~ NA*pcps_t0 + ',runif(1, 0.6, 0.8),'*dccs_t0 + ',runif(1, 0.6, 0.8),'*rotation_t0 + ',runif(1, 0.6, 0.8),'*flanker_t0',
          
          
      '# Task-specific variance
      flanker_v_l     =~ NA*flanker_v
      dccs_v_l        =~ NA*dccs_v
      rotation_v_l    =~ NA*rotation_v
      pcps_v_l        =~ NA*pcps_v
      
      flanker_a_l     =~ NA*flanker_a
      dccs_a_l        =~ NA*dccs_a
      rotation_a_l    =~ NA*rotation_a
      pcps_a_l        =~ NA*pcps_a
      
      flanker_t0_l    =~ NA*flanker_t0
      dccs_t0_l       =~ NA*dccs_t0
      rotation_t0_l   =~ NA*rotation_t0
      pcps_t0_l       =~ NA*pcps_t0
 
 
      # Only covariances between general latent factors and residual variances of the same task.
      v_general       ~~ 1*v_general + a_general + t0_general
      a_general       ~~ 1*a_general + t0_general
      t0_general       ~~ 1*t0_general
      
      flanker_v_l     ~~ 1*flanker_v_l + flanker_a_l + flanker_t0_l + 0*dccs_v_l + 0*dccs_a_l + 0*dccs_t0_l + 0*rotation_v_l + 0*rotation_a_l + 0*rotation_t0_l + 
                         0*pcps_v_l + 0*pcps_a_l + 0*pcps_t0_l
      flanker_a_l     ~~ 1*flanker_a_l + flanker_t0_l + 0*dccs_v_l + 0*dccs_a_l + 0*dccs_t0_l + 0*rotation_v_l + 0*rotation_a_l + 0*rotation_t0_l + 
                         0*pcps_v_l + 0*pcps_a_l + 0*pcps_t0_l
      flanker_t0_l    ~~ 1*flanker_t0_l + 0*dccs_v_l + 0*dccs_a_l + 0*dccs_t0_l + 0*rotation_v_l + 0*rotation_a_l + 0*rotation_t0_l + 
                         0*pcps_v_l + 0*pcps_a_l + 0*pcps_t0_l
      
      dccs_v_l        ~~ 1*dccs_v_l + dccs_a_l + dccs_t0_l + 0*rotation_v_l + 0*rotation_a_l + 0*rotation_t0_l + 
                         0*pcps_v_l + 0*pcps_a_l + 0*pcps_t0_l
      dccs_a_l        ~~ 1*dccs_a_l + dccs_t0_l + 0*rotation_v_l + 0*rotation_a_l + 0*rotation_t0_l + 0*pcps_v_l + 0*pcps_a_l + 0*pcps_t0_l
      dccs_t0_l       ~~ 1*dccs_t0_l + 0*rotation_v_l + 0*rotation_a_l + 0*rotation_t0_l + 0*pcps_v_l + 0*pcps_a_l + 0*pcps_t0_l
      
      rotation_v_l    ~~ 1*rotation_v_l + rotation_a_l + rotation_t0_l + 0*pcps_v_l + 0*pcps_a_l + 0*pcps_t0_l
      rotation_a_l    ~~ 1*rotation_a_l + rotation_t0_l + 0*pcps_v_l + 0*pcps_a_l + 0*pcps_t0_l
      rotation_t0_l   ~~ 1*rotation_t0_l + 0*pcps_v_l + 0*pcps_a_l + 0*pcps_t0_l
      
      pcps_v_l        ~~ 1*pcps_v_l + pcps_a_l + pcps_t0_l
      pcps_a_l        ~~ 1*pcps_a_l + pcps_t0_l
      pcps_t0_l        ~~ 1*pcps_t0_l
       

      # No covariances between manifest variables
      flanker_v     ~~ 0*flanker_v + 0*flanker_a + 0*flanker_t0 + 0*dccs_v + 0*dccs_a + 0*dccs_t0 + 0*rotation_v + 0*rotation_a + 0*rotation_t0 + 0*pcps_v + 0*pcps_a + 0*pcps_t0
      flanker_a     ~~ 0*flanker_a + 0*flanker_t0 + 0*dccs_v + 0*dccs_a + 0*dccs_t0 + 0*rotation_v + 0*rotation_a + 0*rotation_t0 + 0*pcps_v + 0*pcps_a + 0*pcps_t0
      flanker_t0    ~~ 0*flanker_t0 + 0*dccs_v + 0*dccs_a + 0*dccs_t0 + 0*rotation_v + 0*rotation_a + 0*rotation_t0 + 0*pcps_v + 0*pcps_a + 0*pcps_t0
      dccs_v        ~~ 0*dccs_v + 0*dccs_a + 0*dccs_t0 + 0*rotation_v + 0*rotation_a + 0*rotation_t0 + 0*pcps_v + 0*pcps_a + 0*pcps_t0
      dccs_a        ~~ 0*dccs_a + 0*dccs_t0 + 0*rotation_v + 0*rotation_a + 0*rotation_t0 + 0*pcps_v + 0*pcps_a + 0*pcps_t0
      dccs_t0       ~~ 0*dccs_t0 + 0*rotation_v + 0*rotation_a + 0*rotation_t0 + 0*pcps_v + 0*pcps_a + 0*pcps_t0
      rotation_v    ~~ 0*rotation_v + 0*rotation_a + 0*rotation_t0 + 0*pcps_v + 0*pcps_a + 0*pcps_t0
      rotation_a    ~~ 0*rotation_a + 0*rotation_t0 + 0*pcps_v + 0*pcps_a + 0*pcps_t0
      rotation_t0   ~~ 0*rotation_t0 + 0*pcps_v + 0*pcps_a + 0*pcps_t0
      pcps_v        ~~ 0*pcps_v + 0*pcps_a + 0*pcps_t0
      pcps_a        ~~ 0*pcps_a + 0*pcps_t0
      pcps_t0       ~~ 0*pcps_t0
      
      # No covariances between general drift rate and residual variances
      v_general     ~~ 0*flanker_v_l + 0*flanker_a_l + 0*flanker_t0_l + 0*dccs_v_l + 0*dccs_a_l + 0*dccs_t0_l + 0*rotation_v_l + 0*rotation_a_l + 0*rotation_t0_l + 
                       0*pcps_v_l + 0*pcps_a_l + 0*pcps_t0_l
      a_general     ~~ 0*flanker_v_l + 0*flanker_a_l + 0*flanker_t0_l + 0*dccs_v_l + 0*dccs_a_l + 0*dccs_t0_l + 0*rotation_v_l + 0*rotation_a_l + 0*rotation_t0_l + 
                       0*pcps_v_l + 0*pcps_a_l + 0*pcps_t0_l
      t0_general    ~~ 0*flanker_v_l + 0*flanker_a_l + 0*flanker_t0_l + 0*dccs_v_l + 0*dccs_a_l + 0*dccs_t0_l + 0*rotation_v_l + 0*rotation_a_l + 0*rotation_t0_l + 
                       0*pcps_v_l + 0*pcps_a_l + 0*pcps_t0_l                  
      
      # Task-general association
      v_general         ~ ',-1*beta,'*threat',
      '\na_general      ~ ',-1*beta,'*threat',
      '\nt0_general     ~ ',-1*beta,'*threat',
      
      '\nv_general      ~ ',-1*beta,'*deprivation',
      '\na_general      ~ ',-1*beta,'*deprivation',
      '\nt0_general     ~ ',-1*beta,'*deprivation',
        
      # Task-specific associations
      '\nflanker_v_l    ~ ',beta,'*threat',
      '\nflanker_a_l    ~ ',beta,'*threat',
      '\nflanker_t0_l   ~ ',beta,'*threat',
      '\ndccs_v_l       ~ ',beta,'*threat',
      '\ndccs_a_l       ~ ',beta,'*threat',
      '\ndccs_t0_l      ~ ',beta,'*threat',
      '\nrotation_v_l   ~ ',beta,'*threat',
      '\nrotation_a_l   ~ ',beta,'*threat',
      '\nrotation_t0_l  ~ ',beta,'*threat',
      
      '\nflanker_v_l    ~ ',beta,'*deprivation',
      '\nflanker_a_l    ~ ',beta,'*deprivation',
      '\nflanker_t0_l   ~ ',beta,'*deprivation',
      '\ndccs_v_l       ~ ',beta,'*deprivation',
      '\ndccs_a_l       ~ ',beta,'*deprivation',
      '\ndccs_t0_l      ~ ',beta,'*deprivation',
      '\nrotation_v_l   ~ ',beta,'*deprivation',
      '\nrotation_a_l   ~ ',beta,'*deprivation',
      '\nrotation_t0_l  ~ ',beta,'*deprivation'
      )
      
      
      # 2. SPECIFY SAMPLE MODEL
      sample_model <- ' 
      # General Drift rate factor
      v_general  =~ NA*pcps_v + dccs_v + rotation_v + flanker_v 
      a_general  =~ NA*pcps_a + dccs_a + rotation_a + flanker_a 
      t0_general =~ NA*pcps_t0 + dccs_t0 + rotation_t0 + flanker_t0


      # Task-specific variance
      flanker_v_l     =~ NA*flanker_v
      dccs_v_l        =~ NA*dccs_v
      rotation_v_l    =~ NA*rotation_v
      pcps_v_l        =~ NA*pcps_v
      
      flanker_a_l     =~ NA*flanker_a
      dccs_a_l        =~ NA*dccs_a
      rotation_a_l    =~ NA*rotation_a
      pcps_a_l        =~ NA*pcps_a
      
      flanker_t0_l    =~ NA*flanker_t0
      dccs_t0_l       =~ NA*dccs_t0
      rotation_t0_l   =~ NA*rotation_t0
      pcps_t0_l       =~ NA*pcps_t0
 
 
      # Only covariances between general latent factors and residual variances of the same task.
      v_general       ~~ 1*v_general + a_general + t0_general
      a_general       ~~ 1*a_general + t0_general
      t0_general      ~~ 1*t0_general
      
      flanker_v_l     ~~ 1*flanker_v_l + flanker_a_l + flanker_t0_l + 0*dccs_v_l + 0*dccs_a_l + 0*dccs_t0_l + 0*rotation_v_l + 0*rotation_a_l + 0*rotation_t0_l + 
                         0*pcps_v_l + 0*pcps_a_l + 0*pcps_t0_l
      flanker_a_l     ~~ 1*flanker_a_l + flanker_t0_l + 0*dccs_v_l + 0*dccs_a_l + 0*dccs_t0_l + 0*rotation_v_l + 0*rotation_a_l + 0*rotation_t0_l + 
                         0*pcps_v_l + 0*pcps_a_l + 0*pcps_t0_l
      flanker_t0_l    ~~ 1*flanker_t0_l + 0*dccs_v_l + 0*dccs_a_l + 0*dccs_t0_l + 0*rotation_v_l + 0*rotation_a_l + 0*rotation_t0_l + 
                         0*pcps_v_l + 0*pcps_a_l + 0*pcps_t0_l
      
      dccs_v_l        ~~ 1*dccs_v_l + dccs_a_l + dccs_t0_l + 0*rotation_v_l + 0*rotation_a_l + 0*rotation_t0_l + 
                         0*pcps_v_l + 0*pcps_a_l + 0*pcps_t0_l
      dccs_a_l        ~~ 1*dccs_a_l + dccs_t0_l + 0*rotation_v_l + 0*rotation_a_l + 0*rotation_t0_l + 0*pcps_v_l + 0*pcps_a_l + 0*pcps_t0_l
      dccs_t0_l       ~~ 1*dccs_t0_l + 0*rotation_v_l + 0*rotation_a_l + 0*rotation_t0_l + 0*pcps_v_l + 0*pcps_a_l + 0*pcps_t0_l
      
      rotation_v_l    ~~ 1*rotation_v_l + rotation_a_l + rotation_t0_l + 0*pcps_v_l + 0*pcps_a_l + 0*pcps_t0_l
      rotation_a_l    ~~ 1*rotation_a_l + rotation_t0_l + 0*pcps_v_l + 0*pcps_a_l + 0*pcps_t0_l
      rotation_t0_l   ~~ 1*rotation_t0_l + 0*pcps_v_l + 0*pcps_a_l + 0*pcps_t0_l
      
      pcps_v_l        ~~ 1*pcps_v_l + pcps_a_l + pcps_t0_l
      pcps_a_l        ~~ 1*pcps_a_l + pcps_t0_l
      pcps_t0_l       ~~ 1*pcps_t0_l
       

      # No covariances between manifest variables
      flanker_v     ~~ 0*flanker_v + 0*flanker_a + 0*flanker_t0 + 0*dccs_v + 0*dccs_a + 0*dccs_t0 + 0*rotation_v + 0*rotation_a + 0*rotation_t0 + 0*pcps_v + 0*pcps_a + 0*pcps_t0
      flanker_a     ~~ 0*flanker_a + 0*flanker_t0 + 0*dccs_v + 0*dccs_a + 0*dccs_t0 + 0*rotation_v + 0*rotation_a + 0*rotation_t0 + 0*pcps_v + 0*pcps_a + 0*pcps_t0
      flanker_t0    ~~ 0*flanker_t0 + 0*dccs_v + 0*dccs_a + 0*dccs_t0 + 0*rotation_v + 0*rotation_a + 0*rotation_t0 + 0*pcps_v + 0*pcps_a + 0*pcps_t0
      dccs_v        ~~ 0*dccs_v + 0*dccs_a + 0*dccs_t0 + 0*rotation_v + 0*rotation_a + 0*rotation_t0 + 0*pcps_v + 0*pcps_a + 0*pcps_t0
      dccs_a        ~~ 0*dccs_a + 0*dccs_t0 + 0*rotation_v + 0*rotation_a + 0*rotation_t0 + 0*pcps_v + 0*pcps_a + 0*pcps_t0
      dccs_t0       ~~ 0*dccs_t0 + 0*rotation_v + 0*rotation_a + 0*rotation_t0 + 0*pcps_v + 0*pcps_a + 0*pcps_t0
      rotation_v    ~~ 0*rotation_v + 0*rotation_a + 0*rotation_t0 + 0*pcps_v + 0*pcps_a + 0*pcps_t0
      rotation_a    ~~ 0*rotation_a + 0*rotation_t0 + 0*pcps_v + 0*pcps_a + 0*pcps_t0
      rotation_t0   ~~ 0*rotation_t0 + 0*pcps_v + 0*pcps_a + 0*pcps_t0
      pcps_v        ~~ 0*pcps_v + 0*pcps_a + 0*pcps_t0
      pcps_a        ~~ 0*pcps_a + 0*pcps_t0
      pcps_t0       ~~ 0*pcps_t0
      
      # No covariances between general drift rate and residual variances
      v_general     ~~ 0*flanker_v_l + 0*flanker_a_l + 0*flanker_t0_l + 0*dccs_v_l + 0*dccs_a_l + 0*dccs_t0_l + 0*rotation_v_l + 0*rotation_a_l + 0*rotation_t0_l + 
                       0*pcps_v_l + 0*pcps_a_l + 0*pcps_t0_l
      a_general     ~~ 0*flanker_v_l + 0*flanker_a_l + 0*flanker_t0_l + 0*dccs_v_l + 0*dccs_a_l + 0*dccs_t0_l + 0*rotation_v_l + 0*rotation_a_l + 0*rotation_t0_l + 
                       0*pcps_v_l + 0*pcps_a_l + 0*pcps_t0_l
      t0_general    ~~ 0*flanker_v_l + 0*flanker_a_l + 0*flanker_t0_l + 0*dccs_v_l + 0*dccs_a_l + 0*dccs_t0_l + 0*rotation_v_l + 0*rotation_a_l + 0*rotation_t0_l + 
                       0*pcps_v_l + 0*pcps_a_l + 0*pcps_t0_l                  
      
      # Task-general association
      v_general     ~ threat
      a_general     ~ threat
      t0_general    ~ threat

      v_general     ~ deprivation
      a_general     ~ deprivation
      t0_general    ~ deprivation

      # Task-specific associations
      flanker_v_l   ~ threat
      flanker_a_l   ~ threat
      flanker_t0_l  ~ threat
      dccs_v_l      ~ threat
      dccs_a_l      ~ threat
      dccs_t0_l     ~ threat
      rotation_v_l  ~ threat
      rotation_a_l  ~ threat
      rotation_t0_l ~ threat
      
      flanker_v_l   ~ deprivation
      flanker_a_l   ~ deprivation
      flanker_t0_l  ~ deprivation
      dccs_v_l      ~ deprivation
      dccs_a_l      ~ deprivation
      dccs_t0_l     ~ deprivation
      rotation_v_l  ~ deprivation
      rotation_a_l  ~ deprivation
      rotation_t0_l ~ deprivation
      '
      
      # 3. SIMULATE DATA
      data = lavaan::simulateData(
        model = population_model, 
        sample.nobs = n
      ) |> 
      mutate(across(everything(), scale))
      
      
      # 4. FIT SAMPLE MODEL TO DATA
      fit <- sem(sample_model, data=data)
      
      
      # 5. STORE RESULTS
      estimates <- parameterEstimates(fit) |> 
        as_tibble() |> 
        filter(op == "~") |> 
        mutate(
          sim  = sim,
          n    = n,
          beta = beta
        ) |> 
        bind_cols(fitstats)
        
     
      
    }, .options = furrr::furrr_options(seed = TRUE)) 
  )

plan("sequential")

# Plot power curves
power <- power_results |> 
  select(-sim, -n, -beta) |> 
  unnest(results) |> 
  select(sim, n, beta, lhs, rhs, est, pvalue, cfi, rmsea) |> 
  drop_na(sim) |> 
  mutate(Type = ifelse(str_detect(lhs, "general"), "General Factor", "Unique Variance")) |> 
  group_by(n, Type, beta, lhs, rhs) |> 
  summarise(
    mean_est = mean(est),
    mean_cfi = mean(cfi),
    mean_rmsea = mean(rmsea),
    power    = sum(pvalue < .05)/n()*100
  ) |> 
  ungroup() 

save(power, file = 'closed_data/power.RData')






