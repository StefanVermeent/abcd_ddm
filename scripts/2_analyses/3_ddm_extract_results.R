source("scripts/custom_functions/ddm_functions.R")

# Load raw data files
lmt_clean     <- readr::read_csv(paste0("data/lmt_clean", data_suffix, ".csv"))
flanker_clean <- readr::read_csv(paste0("data/flanker_clean", data_suffix, ".csv"))
pcps_clean    <- readr::read_csv(paste0("data/pcps_clean", data_suffix, ".csv"))
dccs_clean    <- readr::read_csv(paste0("data/dccs_clean", data_suffix, ".csv"))

# Load IVs
mnlfa_ivs     <- readr::read_csv("data/mnlfa_ivs_clean.csv") |> 
  mutate(
  dep_mnlfa_c   = scale(dep_mnlfa),
  threat_mnlfa_c = scale(threat_mnlfa),
  dep_mnlfa_cat = case_when(
    dep_mnlfa_c < -1 ~ "< -1SD",
    dep_mnlfa_c > 1 ~ "> 1SD",
    dep_mnlfa_c > -1 & dep_mnlfa_c < 1 ~ "≥1*SD*≤"
  ),
  threat_mnlfa_cat = case_when(
    threat_mnlfa_c < -1 ~ "< -1SD",
    threat_mnlfa_c > 1 ~ "> 1SD",
    threat_mnlfa_c > -1 & threat_mnlfa_c < 1 ~ "≥1*SD*≤"
  ),
  across(c(dep_mnlfa_c, threat_mnlfa_c), as.numeric)
)

# Load DDM model objects
load("analysis_objects/ddm_lmt_mod1.RData")
load("analysis_objects/ddm_lmt_mod2.RData")
load("analysis_objects/ddm_flanker_mod1.RData")
load("analysis_objects/ddm_flanker_mod2.RData")
load("analysis_objects/ddm_dccs_mod1.RData")
load("analysis_objects/ddm_dccs_mod2.RData")
load("analysis_objects/ddm_pcps_mod1.RData")

# Load ID matches to link DDM IDs to ABCD IDs
lmt_id_matches <- lmt_clean |> 
  distinct(subj_idx) |> 
  mutate(subj_idx_num = 1:n())

flanker_id_matches <- flanker_clean |> 
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

# 2. Extract parameter estimates ----
lmt_mod1_param_est <- extract_ddm_estimates(mcmc = mcmc_lmt_mod1, task_prefix = "rotation_", id_matches = lmt_id_matches)
lmt_mod1_param_est <- extract_ddm_estimates(mcmc = mcmc_lmt_mod2, task_prefix = "rotation_", id_matches = lmt_id_matches)

flanker_mod1_param_est <- extract_ddm_estimates_2con(mcmc = mcmc_flanker_mod1, task_prefix = "flanker_", id_matches = flanker_id_matches)
flanker_mod1_param_est <- extract_ddm_estimates(mcmc = mcmc_flanker_mod2, task_prefix = "flanker_", id_matches = flanker_id_matches)

dccs_mod1_param_est <- extract_ddm_estimates_2con(mcmc = mcmc_dccs_mod1, task_prefix = "dccs_", id_matches = dccs_id_matches)
dccs_mod1_param_est <- extract_ddm_estimates(mcmc = mcmc_dccs_mod2, task_prefix = "dccs_", id_matches = dccs_id_matches)

pcps_mod1_param_est <- extract_ddm_estimates(mcmc = mcmc_pcps_mod1, task_prefix = "pcps_", id_matches = pcps_id_matches)

# 3. Generate simulated datasets for model fit assessment ----

## 3.1 Mental Rotation Model 1: Imputed scores ----

lmt_mod1_fit <- lmt_mod1_param_est %>%
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

lmt_mod1_fit |> 
  group_by(percentile) |> 
  summarise(r = cor(RT_sim, RT))
  
save(lmt_mod1_traces, lmt_mod1_param_est, lmt_mod1_fit, file = "analysis_objects/ddm_lmt_model_parsed.RData")


## 3.2 Mental Rotation Model 2: Remove missing scores ----

lmt_mod2_fit <- lmt_mod2_param_est %>%
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

lmt_mod2_fit |> 
  group_by(percentile) |> 
  summarise(r = cor(RT_sim, RT))

save(lmt_mod2_traces, lmt_mod2_param_est, lmt_mod2_fit, file = "analysis_objects/ddm_lmt_model_parsed.RData")

## 3.3 Flanker Model 1: Separate conditions ----

set.seed(284)

flanker_mod1_fit <- flanker_mod1_param_est %>%
  mutate(
    responses = pmap(., function(subj_idx, flanker_v1, flanker_v2, flanker_a, flanker_t1, flanker_t2) {
      
      bind_rows(
        # Condition 1
        RWiener::rwiener(n=10, alpha=flanker_a, tau=flanker_t1, beta=0.5, delta=flanker_v1) |> 
          as_tibble() |> 
          mutate(
            condition = "congruent"
          ),
        # Condition 2
        RWiener::rwiener(n=10, alpha=flanker_a, tau=flanker_t2, beta=0.5, delta=flanker_v2) |> 
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

## 3.4 Flanker Model 2: Collapse across conditions ----

flanker_mod2_fit <- flanker_mod2_param_est %>%
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

save(flanker_mod2_traces, dccs_mod2_param_est, dccs_mod2_fit, file = "analysis_objects/ddm_flanker_mod2_parsed.RData")

## 3.5 Attention Shifting Model 1: Separate conditions ----

set.seed(284)

dccs_mod1_fit <- dccs_mod1_param_est %>%
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


save(dccs_mod1_traces, dccs_mod1_param_est, dccs_mod1_fit, file = "analysis_objects/ddm_dccs_mod1_parsed.RData")


## 3.6 Attention Shifting Model 2: Collapse across conditions ----

dccs_mod2_fit <- dccs_mod2_param_est %>%
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

save(dccs_mod2_traces, dccs_mod2_param_est, dccs_mod2_fit, file = "analysis_objects/ddm_dccs_mod2_parsed.RData")


## 3.7 Processing Speed ----

pcps_mod1_fit <- pcps_mod1_param_est %>%
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

pcps_mod1_fit |> 
  group_by(percentile) |> 
  summarise(r = cor(RT_sim, RT))

save(pcps_mod1_traces, pcps_mod1_param_est, pcps_mod1_fit, file = "analysis_objects/ddm_pcps_model_parsed.RData")
