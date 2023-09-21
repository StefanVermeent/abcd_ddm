
# Libraries ---------------------------------------------------------------

library(lavaan)
library(tidyverse)
library(furrr)
library(RWiener)



# Simulation settings -----------------------------------------------------

nsim    <- 1:500
nSample <- c(1500)
beta    <- c(0.1)

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
        v_general =~ ', runif(1, 1,2),'*pcps_v + ',runif(1, 1,2),'*in1_v + ',runif(1, 1,2),'*in2_v + ',runif(1, 1,2),'*sh1_v + ', runif(1, 1,2),'*sh2_v +', runif(1, 1,2), '*sh3_v',
          '
        # Unique factors
        
        \nv_inh =~ ', runif(1, 1, 2), '*in1_v +', runif(1, 1, 2),'*in2_v',
        '\nv_shift =~ ', runif(1, 1, 2), '*sh1_v +', runif(1, 1, 2), '*sh2_v + ', runif(1, 1, 2), '*sh3_v',
        
        '
        v_general ~~ 0*v_inh + 0*v_shift
        v_inh ~~ 0*v_shift
        ',

        # ** ADVERSITY ASSOCIATIONS **
        # Task-General associations',
        '\nv_general ~ ', beta, '*deprivation',
          
          
          # Task-Specific associations
          '\nv_inh   ~ ', beta, '*deprivation',
          '\nv_shift   ~ ', beta, '*deprivation'
        )
      
      
      
      # 2. SPECIFY SAMPLE MODEL
      sample_model <- 
        '
  # General factors
  v_general  =~ pcps_v + in1_v + in2_v + sh1_v + sh2_v + sh3_v
  
  # Unique factors
  v_inh =~ in1_v + in2_v
  v_shift =~ sh1_v + sh2_v + sh3_v
  
  # Manifest residual variances
  v_general ~~ 0*v_inh + 0*v_shift
  v_inh ~~ 0*v_shift
        

  
  # ** ADVERSITY ASSOCIATIONS **
  # Task-General associations
  v_general     ~ deprivation
  
  # Task-Specific associations
  v_inh ~ deprivation
  v_shift ~ deprivation
'
      
      # 3. SIMULATE DATA
      
      
      data <-  lavaan::simulateData(
        model = population_model, 
        sample.nobs = n,
        # standardized = TRUE,
        orthogonal = TRUE,
        std.lv = F,
        auto.cov.lv.x = FALSE,
        auto.cov.y = FALSE,
        auto.fix.first = F
      ) |> 
        mutate(across(everything(), scale))
      
      
      # 4. FIT SAMPLE MODEL TO DATA
      fit <- 
        sem(sample_model,
            data=data,
            orthogonal = TRUE,
            std.lv = TRUE,
            auto.cov.lv.x = FALSE,
            auto.cov.y = FALSE,
            auto.fix.first = TRUE
        )
     
      # 5. STORE RESULTS
      estimates <- parameterEstimates(fit, standardized = F) |> 
        as_tibble() |> 
        filter(op == "~") |> 
        mutate(
          sim  = sim,
          n    = n,
          beta = beta
        )
      estimates
      
    }, .options = furrr::furrr_options(seed = TRUE)) 
  )

plan("sequential")

# Plot power curves
power <- power_results |> 
  select(-sim, -n, -beta) |> 
  unnest(results) |> 
  select(sim, n, beta, lhs, rhs, est, pvalue) |> 
  drop_na(sim) |> 
  mutate(Type = ifelse(str_detect(lhs, "general"), "General Factor", "Unique Variance")) |> 
  group_by(n, Type, beta, rhs) |> 
  drop_na(pvalue) |> 
  summarise(
    median_est = median(est),
    power    = sum(pvalue < .05)/n()*100
  ) |> 
  ungroup() 

save(power, file = 'closed_data/power.RData')






