
# Load objects ------------------------------------------------------------

source("scripts/custom_functions/ddm_functions.R")

lmt_clean     <- readr::read_csv(paste0("data/lmt_clean", data_suffix, ".csv"))
flanker_clean <- readr::read_csv(paste0("data/flanker_clean", data_suffix, ".csv"))
pcps_clean    <- readr::read_csv(paste0("data/pcps_clean", data_suffix, ".csv"))
dccs_clean    <- readr::read_csv(paste0("data/dccs_clean", data_suffix, ".csv"))

pcps_clean_mod2 <- pcps_clean |> 
  filter(RT >= 1)

flanker_clean_mod4 <- flanker_clean |> 
  filter(RT < 6)

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

# Load DDM model objects
load("analysis_objects/ddm_lmt_mod1.RData")
load("analysis_objects/ddm_lmt_mod2.RData")
load("analysis_objects/ddm_flanker_mod1.RData")
load("analysis_objects/ddm_flanker_mod2.RData")
load("analysis_objects/ddm_dccs_mod1.RData")
load("analysis_objects/ddm_dccs_mod2.RData")
load("analysis_objects/ddm_pcps_mod1.RData")
load("analysis_objects/ddm_pcps_mod2.RData")

load("analysis_objects/ddm_flanker_mod_expl3.RData")
load("analysis_objects/ddm_flanker_mod_expl4.RData")
load("analysis_objects/ddm_dccs_mod_expl.RData")

# Load ID matches to link DDM IDs to ABCD IDs
lmt_id_matches <- lmt_clean |> 
  distinct(subj_idx) |> 
  mutate(subj_idx_num = 1:n())

flanker_id_matches <- flanker_clean |> 
  distinct(subj_idx) |> 
  mutate(subj_idx_num = 1:n())

dccs_id_matches <- dccs_clean |> 
  distinct(subj_idx) |> 
  mutate(subj_idx_num = 1:n())

pcps_id_matches <- pcps_clean |> 
  distinct(subj_idx) |> 
  mutate(subj_idx_num = 1:n())

pcps_id_matches_mod2 <- pcps_clean |>
  filter(RT >= 1) |> 
  distinct(subj_idx) |> 
  mutate(subj_idx_num = 1:n())

# 1. Extract traces ----
lmt_mod1_traces <- extract_traces(mcmc_lmt_mod1, chains = 3, iterations = 1000)
lmt_mod2_traces <- extract_traces(mcmc_lmt_mod2, chains = 3, iterations = 1000)

flanker_mod1_traces <- extract_traces_2con(mcmc_flanker_mod1, chains = 3, iterations = 1000)
flanker_mod2_traces <- extract_traces(mcmc_flanker_mod2, chains = 3, iterations = 1000)

dccs_mod1_traces <- extract_traces_2con(mcmc_dccs_mod1, chains = 3, iterations = 1000)
dccs_mod2_traces <- extract_traces(mcmc_dccs_mod2, chains = 3, iterations = 1000)

pcps_mod1_traces <- extract_traces(mcmc_pcps_mod1, chains = 3, iterations = 1000)
pcps_mod2_traces <- extract_traces(mcmc_pcps_mod2, chains = 3, iterations = 1000)

flanker_mod_expl_traces <- extract_traces_2con_expl(mcmc_flanker_mod_expl, chains = 3, iterations = 1000)
flanker_mod_expl_traces2 <- extract_traces_2con_expl(mcmc_flanker_mod_expl, chains = 3, iterations = 1000)
flanker_mod_expl_traces3 <- extract_traces_2con_expl(mcmc_flanker_mod_expl3, chains = 3, iterations = 1000)
dccs_mod_expl_traces <- extract_traces_2con_expl(mcmc_dccs_mod_expl, chains = 3, iterations = 1000)


# 2. Extract parameter estimates ----
# Primary models
lmt_mod1_param_est <- extract_ddm_estimates(mcmc = mcmc_lmt_mod1, task_prefix = "rotation_", id_matches = lmt_id_matches)
lmt_mod2_param_est <- extract_ddm_estimates(mcmc = mcmc_lmt_mod2, task_prefix = "rotation_", id_matches = lmt_id_matches)

flanker_mod1_param_est <- extract_ddm_estimates_2con(mcmc = mcmc_flanker_mod1, task_prefix = "flanker_", id_matches = flanker_id_matches)
flanker_mod2_param_est <- extract_ddm_estimates(mcmc = mcmc_flanker_mod2, task_prefix = "flanker_", id_matches = flanker_id_matches)

dccs_mod1_param_est <- extract_ddm_estimates_2con(mcmc = mcmc_dccs_mod1, task_prefix = "dccs_", id_matches = dccs_id_matches)
dccs_mod2_param_est <- extract_ddm_estimates(mcmc = mcmc_dccs_mod2, task_prefix = "dccs_", id_matches = dccs_id_matches)

pcps_mod1_param_est <- extract_ddm_estimates(mcmc = mcmc_pcps_mod1, task_prefix = "pcps_", id_matches = pcps_id_matches)
pcps_mod2_param_est <- extract_ddm_estimates(mcmc = mcmc_pcps_mod2, task_prefix = "pcps_", id_matches = pcps_id_matches_mod2)

# Exploratory models
flanker_mod_expl_param_est <- extract_ddm_estimates_2con_expl(mcmc = mcmc_flanker_mod_expl, task_prefix = "flanker_", id_matches = flanker_id_matches)
flanker_mod_expl_param_est2 <- extract_ddm_estimates_2con_expl(mcmc = mcmc_flanker_mod_expl, task_prefix = "flanker_", id_matches = flanker_id_matches)
flanker_mod_expl_param_est3 <- extract_ddm_estimates_2con_expl(mcmc = mcmc_flanker_mod_expl3, task_prefix = "flanker_", id_matches = flanker_id_matches)
dccs_mod_expl_param_est <- extract_ddm_estimates_2con_expl(mcmc = mcmc_dccs_mod_expl, task_prefix = "dccs_", id_matches = dccs_id_matches)

# 3. Generate simulated datasets for model fit assessment ----

## 3.1 Mental Rotation ----

### 3.1.1 Model 1: Imputed scores ----

lmt_mod1_fit <- lmt_mod1_param_est %>%
  mutate(
    responses = pmap(., function(subj_idx, rotation_a, rotation_v, rotation_t) {
      RWiener::rwiener(n=100, alpha=rotation_a, tau=rotation_t, beta=0.5, delta=rotation_v) |> 
        as_tibble()
    })
  ) |> 
  unnest(responses) |> 
  select(subj_idx, choice_sim = resp, RT = q) |> 
  mutate(choice_sim = ifelse(choice_sim == 'upper', 1, 0)) |> 
  group_by(subj_idx) |> 
  summarise(
    RT_25 = quantile(RT, 0.25),
    RT_50 = quantile(RT, 0.50),
    RT_75 = quantile(RT, 0.75),
    acc_sim   = sum(choice_sim) / n()
  ) |> 
  pivot_longer(starts_with("RT"), names_to = "percentile", values_to = "RT_sim") |> 
  left_join(
    lmt_clean_impute |> 
      drop_na(RT) |> 
      group_by(subj_idx) |> 
      summarise(
        RT_25 = quantile(RT, 0.25),
        RT_50 = quantile(RT, 0.50),
        RT_75 = quantile(RT, 0.75),
        acc   = sum(correct, na.rm = T) / n()
      ) |> 
      pivot_longer(starts_with("RT"), names_to = "percentile", values_to = "RT")
  )
  
save(lmt_mod1_traces, lmt_mod1_param_est, lmt_mod1_fit, file = "analysis_objects/ddm_lmt_mod1_parsed.RData")


### 3.1.2 Model 2: Remove missing scores ----

lmt_mod2_fit <- lmt_mod2_param_est %>%
  mutate(
    responses = pmap(., function(subj_idx, rotation_a, rotation_v, rotation_t) {
      RWiener::rwiener(n=100, alpha=rotation_a, tau=rotation_t, beta=0.5, delta=rotation_v) |> 
        as_tibble()
    })
  ) |> 
  unnest(responses) |> 
  select(subj_idx, choice_sim = resp, RT = q) |> 
  mutate(choice_sim = ifelse(choice_sim == 'upper', 1, 0)) |> 
  group_by(subj_idx) |> 
  summarise(
    RT_25 = quantile(RT, 0.25),
    RT_50 = quantile(RT, 0.50),
    RT_75 = quantile(RT, 0.75),
    acc_sim   = sum(choice_sim) / n()
  ) |> 
  pivot_longer(starts_with("RT"), names_to = "percentile", values_to = "RT_sim") |> 
  left_join(
    lmt_clean |> 
      drop_na(RT) |> 
      group_by(subj_idx) |> 
      summarise(
        RT_25 = quantile(RT, 0.25),
        RT_50 = quantile(RT, 0.50),
        RT_75 = quantile(RT, 0.75),
        acc   = sum(correct, na.rm = T) / n()
      ) |> 
      pivot_longer(starts_with("RT"), names_to = "percentile", values_to = "RT")
  )

save(lmt_mod2_traces, lmt_mod2_param_est, lmt_mod2_fit, file = "analysis_objects/ddm_lmt_mod2_parsed.RData")

## 3.2 Flanker ----

### 3.2.1 Model 1: Separate conditions ----

set.seed(284)

flanker_mod1_fit <- flanker_mod1_param_est %>%
  mutate(
    responses = pmap(., function(subj_idx, flanker_v1, flanker_v2, flanker_a, flanker_t1, flanker_t2) {
      
      bind_rows(
        # Condition 1
        RWiener::rwiener(n=100, alpha=flanker_a, tau=flanker_t1, beta=0.5, delta=flanker_v1) |> 
          as_tibble() |> 
          mutate(
            condition = "congruent"
          ),
        # Condition 2
        RWiener::rwiener(n=100, alpha=flanker_a, tau=flanker_t2, beta=0.5, delta=flanker_v2) |> 
          as_tibble() |> 
          mutate(
            condition = 'incongruent'
          )
      )
    })
  ) |> 
  unnest(responses) |> 
  select(subj_idx, condition, choice_sim = resp, RT = q) |> 
  mutate(choice_sim = ifelse(choice_sim == 'upper', 1, 0)) |> 
  group_by(subj_idx, condition) |> 
  summarise(
    RT_25 = quantile(RT, 0.25),
    RT_50 = quantile(RT, 0.50),
    RT_75 = quantile(RT, 0.75),
    acc_sim   = sum(choice_sim) / n()
  ) |> 
  pivot_longer(starts_with("RT"), names_to = "percentile", values_to = "RT_sim") |> 
  left_join(
    flanker_clean |> 
      drop_na(RT) |> 
      group_by(subj_idx, condition) |> 
      summarise(
        RT_25 = quantile(RT, 0.25),
        RT_50 = quantile(RT, 0.50),
        RT_75 = quantile(RT, 0.75),
        acc   = sum(correct, na.rm = T) / n()
      ) |> 
      pivot_longer(starts_with("RT"), names_to = "percentile", values_to = "RT")
  ) |> 
  left_join(mnlfa_ivs) 

save(flanker_mod1_traces, flanker_mod1_param_est, flanker_mod1_fit, file = "analysis_objects/ddm_flanker_mod1_parsed.RData")

### 3.2.2 Model 2: Collapse across conditions ----

flanker_mod2_fit <- flanker_mod2_param_est %>%
  mutate(
    responses = pmap(., function(subj_idx, flanker_a, flanker_v, flanker_t) {
      RWiener::rwiener(n=100, alpha=flanker_a, tau=flanker_t, beta=0.5, delta=flanker_v) |> 
        as_tibble()
    })
  ) |> 
  unnest(responses) |> 
  select(subj_idx, choice_sim = resp, RT = q) |> 
  mutate(choice_sim = ifelse(choice_sim == 'upper', 1, 0)) |> 
  group_by(subj_idx) |> 
  summarise(
    RT_25 = quantile(RT, 0.25),
    RT_50 = quantile(RT, 0.50),
    RT_75 = quantile(RT, 0.75),
    acc_sim   = sum(choice_sim) / n()
  ) |> 
  pivot_longer(starts_with("RT"), names_to = "percentile", values_to = "RT_sim") |> 
  left_join(
    flanker_clean |> 
      drop_na(RT) |> 
      group_by(subj_idx) |> 
      summarise(
        RT_25 = quantile(RT, 0.25),
        RT_50 = quantile(RT, 0.50),
        RT_75 = quantile(RT, 0.75),
        acc   = sum(correct, na.rm = T) / n()
      ) |> 
      pivot_longer(starts_with("RT"), names_to = "percentile", values_to = "RT")
  ) |> 
  left_join(mnlfa_ivs)

save(flanker_mod2_traces, flanker_mod2_param_est, flanker_mod2_fit, file = "analysis_objects/ddm_flanker_mod2_parsed.RData")

## 3.3 Attention Shifting ----

### 3.3.1 Model 1: Separate conditions ----

dccs_mod1_dic <- mcmc_dccs_mod1 |> 
  summarise(dic = mean(deviance)) |> 
  pull()

set.seed(284)

dccs_mod1_fit <- dccs_mod1_param_est %>%
  mutate(
    responses = pmap(., function(subj_idx, dccs_v1, dccs_v2, dccs_a, dccs_t1, dccs_t2) {
      
      bind_rows(
        # Condition 1
        RWiener::rwiener(n=100, alpha=dccs_a, tau=dccs_t1, beta=0.5, delta=dccs_v1) |> 
          as_tibble() |> 
          mutate(
            condition = "repeat"
          ),
        # Condition 2
        RWiener::rwiener(n=100, alpha=dccs_a, tau=dccs_t2, beta=0.5, delta=dccs_v2) |> 
          as_tibble() |> 
          mutate(
            condition = 'switch'
          )
      )
    })
  ) |> 
  unnest(responses) |> 
  select(subj_idx, condition, choice_sim = resp, RT = q) |> 
  mutate(choice_sim = ifelse(choice_sim == 'upper', 1, 0)) |> 
  group_by(subj_idx, condition) |> 
  summarise(
    RT_25 = quantile(RT, 0.25),
    RT_50 = quantile(RT, 0.50),
    RT_75 = quantile(RT, 0.75),
    acc_sim   = sum(choice_sim) / n()
  ) |> 
  pivot_longer(starts_with("RT"), names_to = "percentile", values_to = "RT_sim") |> 
  left_join(
    dccs_clean |> 
      drop_na(RT) |> 
      group_by(subj_idx, condition) |> 
      summarise(
        RT_25 = quantile(RT, 0.25),
        RT_50 = quantile(RT, 0.50),
        RT_75 = quantile(RT, 0.75),
        acc   = sum(correct, na.rm = T) / n()
      ) |> 
      pivot_longer(starts_with("RT"), names_to = "percentile", values_to = "RT")
  ) |> 
  left_join(mnlfa_ivs) 


save(dccs_mod1_traces, dccs_mod1_param_est, dccs_mod1_fit, file = "analysis_objects/ddm_dccs_mod1_parsed.RData")

### 3.3.2 Model 2: Collapse across conditions ----

dccs_mod2_fit <- dccs_mod2_param_est %>%
  mutate(
    responses = pmap(., function(subj_idx, dccs_a, dccs_v, dccs_t) {
      RWiener::rwiener(n=100, alpha=dccs_a, tau=dccs_t, beta=0.5, delta=dccs_v) |> 
        as_tibble()
    })
  ) |> 
  unnest(responses) |> 
  select(subj_idx, choice_sim = resp, RT = q) |> 
  mutate(choice_sim = ifelse(choice_sim == 'upper', 1, 0)) |> 
  group_by(subj_idx) |> 
  summarise(
    RT_25 = quantile(RT, 0.25),
    RT_50 = quantile(RT, 0.50),
    RT_75 = quantile(RT, 0.75),
    acc_sim   = sum(choice_sim) / n()
  ) |> 
  pivot_longer(starts_with("RT"), names_to = "percentile", values_to = "RT_sim") |> 
  left_join(
    dccs_clean |> 
      drop_na(RT) |> 
      group_by(subj_idx) |> 
      summarise(
        RT_25 = quantile(RT, 0.25),
        RT_50 = quantile(RT, 0.50),
        RT_75 = quantile(RT, 0.75),
        acc   = sum(correct, na.rm = T) / n()
      ) |> 
      pivot_longer(starts_with("RT"), names_to = "percentile", values_to = "RT")
  ) |> 
  left_join(mnlfa_ivs)

save(dccs_mod2_traces, dccs_mod2_param_est, dccs_mod2_fit, file = "analysis_objects/ddm_dccs_mod2_parsed.RData")


## 3.4 Processing Speed ----

### 3.4.1 Model 1: Standard model, 10.000 iterations ----

pcps_mod1_fit <- pcps_mod1_param_est %>%
  mutate(
    responses = pmap(., function(subj_idx, pcps_a, pcps_v, pcps_t) {
      RWiener::rwiener(n=100, alpha=pcps_a, tau=pcps_t, beta=0.5, delta=pcps_v) |> 
        as_tibble()
    })
  ) |> 
  unnest(responses) |> 
  select(subj_idx, choice_sim = resp, RT = q) |> 
  mutate(choice_sim = ifelse(choice_sim == 'upper', 1, 0)) |> 
  group_by(subj_idx) |> 
  summarise(
    RT_25 = quantile(RT, 0.25),
    RT_50 = quantile(RT, 0.50),
    RT_75 = quantile(RT, 0.75),
    acc_sim   = sum(choice_sim) / n()
  ) |> 
  pivot_longer(starts_with("RT"), names_to = "percentile", values_to = "RT_sim") |> 
  left_join(
    pcps_clean |> 
      drop_na(RT) |> 
      group_by(subj_idx) |> 
      summarise(
        RT_25 = quantile(RT, 0.25),
        RT_50 = quantile(RT, 0.50),
        RT_75 = quantile(RT, 0.75),
        acc   = sum(correct, na.rm = T) / n()
      ) |> 
      pivot_longer(starts_with("RT"), names_to = "percentile", values_to = "RT")
  )

save(pcps_mod1_traces, pcps_mod1_param_est, pcps_mod1_fit, file = "analysis_objects/ddm_pcps_mod1_parsed.RData")

## Explore PCPS non-decision time outliers

map_df(seq(0.4, 2, 0.1), function(x){
  pcps_clean |> 
    filter(RT < x) |> 
    summarise(accuracy = sum(correct) / n()) |> 
    mutate(cutoff = x)
})

### 3.4.2 Model 2: Remove outliers < 1 seconds ----

pcps_mod2_fit <- pcps_mod2_param_est %>%
  mutate(
    responses = pmap(., function(subj_idx, pcps_a, pcps_v, pcps_t) {
      RWiener::rwiener(n=100, alpha=pcps_a, tau=pcps_t, beta=0.5, delta=pcps_v) |> 
        as_tibble()
    })
  ) |> 
  unnest(responses) |> 
  select(subj_idx, choice_sim = resp, RT = q) |> 
  mutate(choice_sim = ifelse(choice_sim == 'upper', 1, 0)) |> 
  group_by(subj_idx) |> 
  summarise(
    RT_25 = quantile(RT, 0.25),
    RT_50 = quantile(RT, 0.50),
    RT_75 = quantile(RT, 0.75),
    acc_sim   = sum(choice_sim) / n()
  ) |> 
  pivot_longer(starts_with("RT"), names_to = "percentile", values_to = "RT_sim") |> 
  left_join(
    pcps_clean_mod2 |> 
      drop_na(RT) |> 
      group_by(subj_idx) |> 
      summarise(
        RT_25 = quantile(RT, 0.25),
        RT_50 = quantile(RT, 0.50),
        RT_75 = quantile(RT, 0.75),
        acc   = sum(correct, na.rm = T) / n()
      ) |> 
      pivot_longer(starts_with("RT"), names_to = "percentile", values_to = "RT")
  )

save(pcps_mod2_traces, pcps_mod2_param_est, pcps_mod2_fit, file = "analysis_objects/ddm_pcps_mod2_parsed.RData")


# 4. Combine DDM data --------------------------------------------------------

ddm_data <- 
  reduce(
    list(
      dccs_mod2_param_est, 
      flanker_mod2_param_est,
      pcps_mod2_param_est,
      lmt_mod1_param_est
    ),
    full_join
  ) |> 
  select(subj_idx, everything())

write_csv(ddm_data, "data/ddm_data.csv")



# 5. Generate simulated datasets with original trial numbers --------------

# Below, we calculate fit indices the way we originally preregistered it, by 
# simulating the same number of trials as the original task.

## 5.1 Mental Rotation ----

### 5.1.1 Model 1: Imputed scores ----

lmt_mod1_fit_orig <- lmt_mod1_param_est %>%
  mutate(
    responses = pmap(., function(subj_idx, rotation_a, rotation_v, rotation_t) {
      RWiener::rwiener(n=32, alpha=rotation_a, tau=rotation_t, beta=0.5, delta=rotation_v) |> 
        as_tibble()
    })
  ) |> 
  unnest(responses) |> 
  select(subj_idx, choice_sim = resp, RT = q) |> 
  mutate(choice_sim = ifelse(choice_sim == 'upper', 1, 0)) |> 
  group_by(subj_idx) |> 
  summarise(
    RT_25 = quantile(RT, 0.25),
    RT_50 = quantile(RT, 0.50),
    RT_75 = quantile(RT, 0.75),
    acc_sim   = sum(choice_sim) / n()
  ) |> 
  pivot_longer(starts_with("RT"), names_to = "percentile", values_to = "RT_sim") |> 
  left_join(
    lmt_clean_impute |> 
      drop_na(RT) |> 
      group_by(subj_idx) |> 
      summarise(
        RT_25 = quantile(RT, 0.25),
        RT_50 = quantile(RT, 0.50),
        RT_75 = quantile(RT, 0.75),
        acc   = sum(correct, na.rm = T) / n()
      ) |> 
      pivot_longer(starts_with("RT"), names_to = "percentile", values_to = "RT")
  )

save(lmt_mod1_fit_orig, file = "analysis_objects/ddm_lmt_mod1_orig_parsed.RData")


### 5.1.2 Model 2: Remove missing scores ----

lmt_mod2_fit_orig <- lmt_mod2_param_est %>%
  mutate(
    responses = pmap(., function(subj_idx, rotation_a, rotation_v, rotation_t) {
      RWiener::rwiener(n=32, alpha=rotation_a, tau=rotation_t, beta=0.5, delta=rotation_v) |> 
        as_tibble()
    })
  ) |> 
  unnest(responses) |> 
  select(subj_idx, choice_sim = resp, RT = q) |> 
  mutate(choice_sim = ifelse(choice_sim == 'upper', 1, 0)) |> 
  group_by(subj_idx) |> 
  summarise(
    RT_25 = quantile(RT, 0.25),
    RT_50 = quantile(RT, 0.50),
    RT_75 = quantile(RT, 0.75),
    acc_sim   = sum(choice_sim) / n()
  ) |> 
  pivot_longer(starts_with("RT"), names_to = "percentile", values_to = "RT_sim") |> 
  left_join(
    lmt_clean |> 
      drop_na(RT) |> 
      group_by(subj_idx) |> 
      summarise(
        RT_25 = quantile(RT, 0.25),
        RT_50 = quantile(RT, 0.50),
        RT_75 = quantile(RT, 0.75),
        acc   = sum(correct, na.rm = T) / n()
      ) |> 
      pivot_longer(starts_with("RT"), names_to = "percentile", values_to = "RT")
  )

save(lmt_mod2_fit_orig, file = "analysis_objects/ddm_lmt_mod2_orig_parsed.RData")

## 5.2 Flanker ----

### 5.2.1 Model 1: Separate conditions ----

set.seed(284)

flanker_mod1_fit_orig <- flanker_mod1_param_est %>%
  mutate(
    responses = pmap(., function(subj_idx, flanker_v1, flanker_v2, flanker_a, flanker_t1, flanker_t2) {
      
      bind_rows(
        # Condition 1
        RWiener::rwiener(n=8, alpha=flanker_a, tau=flanker_t1, beta=0.5, delta=flanker_v1) |> 
          as_tibble() |> 
          mutate(
            condition = "congruent"
          ),
        # Condition 2
        RWiener::rwiener(n=12, alpha=flanker_a, tau=flanker_t2, beta=0.5, delta=flanker_v2) |> 
          as_tibble() |> 
          mutate(
            condition = 'incongruent'
          )
      )
    })
  ) |> 
  unnest(responses) |> 
  select(subj_idx, condition, choice_sim = resp, RT = q) |> 
  mutate(choice_sim = ifelse(choice_sim == 'upper', 1, 0)) |> 
  group_by(subj_idx, condition) |> 
  summarise(
    RT_25 = quantile(RT, 0.25),
    RT_50 = quantile(RT, 0.50),
    RT_75 = quantile(RT, 0.75),
    acc_sim   = sum(choice_sim) / n()
  ) |> 
  pivot_longer(starts_with("RT"), names_to = "percentile", values_to = "RT_sim") |> 
  left_join(
    flanker_clean |> 
      drop_na(RT) |> 
      group_by(subj_idx, condition) |> 
      summarise(
        RT_25 = quantile(RT, 0.25),
        RT_50 = quantile(RT, 0.50),
        RT_75 = quantile(RT, 0.75),
        acc   = sum(correct, na.rm = T) / n()
      ) |> 
      pivot_longer(starts_with("RT"), names_to = "percentile", values_to = "RT")
  ) |> 
  left_join(mnlfa_ivs) 

save(flanker_mod1_fit_orig, file = "analysis_objects/ddm_flanker_mod1_orig_parsed.RData")

### 5.2.2 Model 2: Collapse across conditions ----

flanker_mod2_fit_orig <- flanker_mod2_param_est %>%
  mutate(
    responses = pmap(., function(subj_idx, flanker_a, flanker_v, flanker_t) {
      RWiener::rwiener(n=20, alpha=flanker_a, tau=flanker_t, beta=0.5, delta=flanker_v) |> 
        as_tibble()
    })
  ) |> 
  unnest(responses) |> 
  select(subj_idx, choice_sim = resp, RT = q) |> 
  mutate(choice_sim = ifelse(choice_sim == 'upper', 1, 0)) |> 
  group_by(subj_idx) |> 
  summarise(
    RT_25 = quantile(RT, 0.25),
    RT_50 = quantile(RT, 0.50),
    RT_75 = quantile(RT, 0.75),
    acc_sim   = sum(choice_sim) / n()
  ) |> 
  pivot_longer(starts_with("RT"), names_to = "percentile", values_to = "RT_sim") |> 
  left_join(
    flanker_clean |> 
      drop_na(RT) |> 
      group_by(subj_idx) |> 
      summarise(
        RT_25 = quantile(RT, 0.25),
        RT_50 = quantile(RT, 0.50),
        RT_75 = quantile(RT, 0.75),
        acc   = sum(correct, na.rm = T) / n()
      ) |> 
      pivot_longer(starts_with("RT"), names_to = "percentile", values_to = "RT")
  ) |> 
  left_join(mnlfa_ivs)

save(flanker_mod2_fit_orig, file = "analysis_objects/ddm_flanker_mod2_orig_parsed.RData")

## 5.3 Attention Shifting ----

### 5.3.1 Model 1: Separate conditions ----

set.seed(284)

dccs_mod1_fit_orig <- dccs_mod1_param_est %>%
  mutate(
    responses = pmap(., function(subj_idx, dccs_v1, dccs_v2, dccs_a, dccs_t1, dccs_t2) {
      
      bind_rows(
        # Condition 1
        RWiener::rwiener(n=23, alpha=dccs_a, tau=dccs_t1, beta=0.5, delta=dccs_v1) |> 
          as_tibble() |> 
          mutate(
            condition = "repeat"
          ),
        # Condition 2
        RWiener::rwiener(n=7, alpha=dccs_a, tau=dccs_t2, beta=0.5, delta=dccs_v2) |> 
          as_tibble() |> 
          mutate(
            condition = 'switch'
          )
      )
    })
  ) |> 
  unnest(responses) |> 
  select(subj_idx, condition, choice_sim = resp, RT = q) |> 
  mutate(choice_sim = ifelse(choice_sim == 'upper', 1, 0)) |> 
  group_by(subj_idx, condition) |> 
  summarise(
    RT_25 = quantile(RT, 0.25),
    RT_50 = quantile(RT, 0.50),
    RT_75 = quantile(RT, 0.75),
    acc_sim   = sum(choice_sim) / n()
  ) |> 
  pivot_longer(starts_with("RT"), names_to = "percentile", values_to = "RT_sim") |> 
  left_join(
    dccs_clean |> 
      drop_na(RT) |> 
      group_by(subj_idx, condition) |> 
      summarise(
        RT_25 = quantile(RT, 0.25),
        RT_50 = quantile(RT, 0.50),
        RT_75 = quantile(RT, 0.75),
        acc   = sum(correct, na.rm = T) / n()
      ) |> 
      pivot_longer(starts_with("RT"), names_to = "percentile", values_to = "RT")
  ) |> 
  left_join(mnlfa_ivs) 


save(dccs_mod1_fit_orig, file = "analysis_objects/ddm_dccs_mod1_orig_parsed.RData")

### 5.3.2 Model 2: Collapse across conditions ----

dccs_mod2_fit_orig <- dccs_mod2_param_est %>%
  mutate(
    responses = pmap(., function(subj_idx, dccs_a, dccs_v, dccs_t) {
      RWiener::rwiener(n=30, alpha=dccs_a, tau=dccs_t, beta=0.5, delta=dccs_v) |> 
        as_tibble()
    })
  ) |> 
  unnest(responses) |> 
  select(subj_idx, choice_sim = resp, RT = q) |> 
  mutate(choice_sim = ifelse(choice_sim == 'upper', 1, 0)) |> 
  group_by(subj_idx) |> 
  summarise(
    RT_25 = quantile(RT, 0.25),
    RT_50 = quantile(RT, 0.50),
    RT_75 = quantile(RT, 0.75),
    acc_sim   = sum(choice_sim) / n()
  ) |> 
  pivot_longer(starts_with("RT"), names_to = "percentile", values_to = "RT_sim") |> 
  left_join(
    dccs_clean |> 
      drop_na(RT) |> 
      group_by(subj_idx) |> 
      summarise(
        RT_25 = quantile(RT, 0.25),
        RT_50 = quantile(RT, 0.50),
        RT_75 = quantile(RT, 0.75),
        acc   = sum(correct, na.rm = T) / n()
      ) |> 
      pivot_longer(starts_with("RT"), names_to = "percentile", values_to = "RT")
  ) |> 
  left_join(mnlfa_ivs)

save(dccs_mod2_fit_orig, file = "analysis_objects/ddm_dccs_mod2_orig_parsed.RData")


## 5.4 Processing Speed ----

### 5.4.1 Model 1: Standard model, 10.000 iterations ----

pcps_mod1_fit_orig <- pcps_mod1_param_est %>%
  mutate(
    responses = pmap(., function(subj_idx, pcps_a, pcps_v, pcps_t) {
      RWiener::rwiener(n=32, alpha=pcps_a, tau=pcps_t, beta=0.5, delta=pcps_v) |> 
        as_tibble()
    })
  ) |> 
  unnest(responses) |> 
  select(subj_idx, choice_sim = resp, RT = q) |> 
  mutate(choice_sim = ifelse(choice_sim == 'upper', 1, 0)) |> 
  group_by(subj_idx) |> 
  summarise(
    RT_25 = quantile(RT, 0.25),
    RT_50 = quantile(RT, 0.50),
    RT_75 = quantile(RT, 0.75),
    acc_sim   = sum(choice_sim) / n()
  ) |> 
  pivot_longer(starts_with("RT"), names_to = "percentile", values_to = "RT_sim") |> 
  left_join(
    pcps_clean |> 
      drop_na(RT) |> 
      group_by(subj_idx) |> 
      summarise(
        RT_25 = quantile(RT, 0.25),
        RT_50 = quantile(RT, 0.50),
        RT_75 = quantile(RT, 0.75),
        acc   = sum(correct, na.rm = T) / n()
      ) |> 
      pivot_longer(starts_with("RT"), names_to = "percentile", values_to = "RT")
  )

save(pcps_mod1_fit_orig, file = "analysis_objects/ddm_pcps_mod1_orig_parsed.RData")

## Explore PCPS non-decision time outliers

map_df(seq(0.4, 2, 0.1), function(x){
  pcps_clean |> 
    filter(RT < x) |> 
    summarise(accuracy = sum(correct) / n()) |> 
    mutate(cutoff = x)
})

### 5.4.2 Model 2: Remove outliers < 1 seconds ----

pcps_mod2_fit_orig <- pcps_mod2_param_est %>%
  mutate(
    responses = pmap(., function(subj_idx, pcps_a, pcps_v, pcps_t) {
      RWiener::rwiener(n=32, alpha=pcps_a, tau=pcps_t, beta=0.5, delta=pcps_v) |> 
        as_tibble()
    })
  ) |> 
  unnest(responses) |> 
  select(subj_idx, choice_sim = resp, RT = q) |> 
  mutate(choice_sim = ifelse(choice_sim == 'upper', 1, 0)) |> 
  group_by(subj_idx) |> 
  summarise(
    RT_25 = quantile(RT, 0.25),
    RT_50 = quantile(RT, 0.50),
    RT_75 = quantile(RT, 0.75),
    acc_sim   = sum(choice_sim) / n()
  ) |> 
  pivot_longer(starts_with("RT"), names_to = "percentile", values_to = "RT_sim") |> 
  left_join(
    pcps_clean_mod2 |> 
      drop_na(RT) |> 
      group_by(subj_idx) |> 
      summarise(
        RT_25 = quantile(RT, 0.25),
        RT_50 = quantile(RT, 0.50),
        RT_75 = quantile(RT, 0.75),
        acc   = sum(correct, na.rm = T) / n()
      ) |> 
      pivot_longer(starts_with("RT"), names_to = "percentile", values_to = "RT")
  )

save(pcps_mod2_fit_orig, file = "analysis_objects/ddm_pcps_mod2_orig_parsed.RData")

  

# 6. Exploratory models ---------------------------------------------------

## 6.1 Flanker Task ----

set.seed(284)

flanker_mod_expl_fit3 <- flanker_mod_expl_param_est3 %>%
  mutate(
    responses = pmap(., function(subj_idx, flanker_v1, flanker_v2, flanker_a1, flanker_a2, flanker_t1, flanker_t2) {
      
      bind_rows(
        # Condition 1
        RWiener::rwiener(n=100, alpha=flanker_a1, tau=flanker_t1, beta=0.5, delta=flanker_v1) |> 
          as_tibble() |> 
          mutate(
            condition = "congruent"
          ),
        # Condition 2
        RWiener::rwiener(n=100, alpha=flanker_a2, tau=flanker_t2, beta=0.5, delta=flanker_v2) |> 
          as_tibble() |> 
          mutate(
            condition = 'incongruent'
          )
      )
    })
  ) |> 
  unnest(responses) |> 
  select(subj_idx, condition, choice_sim = resp, RT = q) |> 
  mutate(choice_sim = ifelse(choice_sim == 'upper', 1, 0)) |> 
  group_by(subj_idx, condition) |> 
  summarise(
    RT_25 = quantile(RT, 0.25),
    RT_50 = quantile(RT, 0.50),
    RT_75 = quantile(RT, 0.75),
    acc_sim   = sum(choice_sim) / n()
  ) |> 
  pivot_longer(starts_with("RT"), names_to = "percentile", values_to = "RT_sim") |> 
  left_join(
    flanker_clean |> 
      drop_na(RT) |> 
      group_by(subj_idx, condition) |> 
      summarise(
        RT_25 = quantile(RT, 0.25),
        RT_50 = quantile(RT, 0.50),
        RT_75 = quantile(RT, 0.75),
        acc   = sum(correct, na.rm = T) / n()
      ) |> 
      pivot_longer(starts_with("RT"), names_to = "percentile", values_to = "RT")
  )

save(flanker_mod_expl_param_est3, flanker_mod_expl_traces3, flanker_mod_expl_fit3, file = "analysis_objects/ddm_flanker_mod_expl_parsed3.RData")

flanker_mod_expl_fit3 |> 
  group_by(condition, percentile) |> 
  summarise(
    r_RT = cor(RT_sim, RT)
  )

flanker_mod_expl_fit3 |> 
  select(-percentile, -RT_sim, -RT) |> 
  distinct() |> 
  group_by(condition) |> 
  summarise(
    r_acc = cor(acc_sim, acc)
  )

## 6.2 Attention Shifting ----

set.seed(284)

dccs_mod_expl_fit <- dccs_mod_expl_param_est %>%
  mutate(
    responses = pmap(., function(subj_idx, dccs_v1, dccs_v2, dccs_a1, dccs_a2, dccs_t) {
      
      bind_rows(
        # Condition 1
        RWiener::rwiener(n=100, alpha=dccs_a1, tau=dccs_t, beta=0.5, delta=dccs_v1) |> 
          as_tibble() |> 
          mutate(
            condition = "repeat"
          ),
        # Condition 2
        RWiener::rwiener(n=100, alpha=dccs_a2, tau=dccs_t, beta=0.5, delta=dccs_v2) |> 
          as_tibble() |> 
          mutate(
            condition = 'switch'
          )
      )
    })
  ) |> 
  unnest(responses) |> 
  select(subj_idx, condition, choice_sim = resp, RT = q) |> 
  mutate(choice_sim = ifelse(choice_sim == 'upper', 1, 0)) |> 
  group_by(subj_idx, condition) |> 
  summarise(
    RT_25 = quantile(RT, 0.25),
    RT_50 = quantile(RT, 0.50),
    RT_75 = quantile(RT, 0.75),
    acc_sim   = sum(choice_sim) / n()
  ) |> 
  pivot_longer(starts_with("RT"), names_to = "percentile", values_to = "RT_sim") |> 
  left_join(
    dccs_clean |> 
      drop_na(RT) |> 
      group_by(subj_idx, condition) |> 
      summarise(
        RT_25 = quantile(RT, 0.25),
        RT_50 = quantile(RT, 0.50),
        RT_75 = quantile(RT, 0.75),
        acc   = sum(correct, na.rm = T) / n()
      ) |> 
      pivot_longer(starts_with("RT"), names_to = "percentile", values_to = "RT")
  )

save(dccs_mod_expl_fit, file = "analysis_objects/ddm_dccs_mod_expl_parsed.RData")

dccs_mod_expl_fit |> 
  group_by(condition, percentile) |> 
  summarise(
    r_RT = cor(RT_sim, RT)
  )

dccs_mod_expl_fit |> 
  select(-percentile, -RT_sim, -RT) |> 
  distinct() |> 
  group_by(condition) |> 
  summarise(
    r_acc = cor(acc_sim, acc)
  )

# 7. Combine DDM data --------------------------------------------------------

ddm_data_expl <- 
  reduce(
    list(
      dccs_mod_expl_param_est2, 
      flanker_mod_expl_param_est3,
      pcps_mod2_param_est,
      lmt_mod1_param_est
    ),
    full_join
  ) |> 
  select(subj_idx, everything())

write_csv(ddm_data_expl, "data/ddm_data_expl.csv")

