set.seed(92759)

# Load real data ----------------------------------------------------------

lmt_clean     <- readr::read_csv("data/lmt_clean.csv")
flanker_clean <- readr::read_csv("data/flanker_clean.csv")
pcps_clean    <- readr::read_csv("data/pcps_clean.csv")
dccs_clean    <- readr::read_csv("data/dccs_clean.csv")
training_set  <- readr::read_csv('data/training_set.csv')
test_set      <- readr::read_csv('data/test_set.csv')


# Anonymize participant IDs 

anon_ids <- reduce(
  list(
    lmt_clean |>  select(subj_idx),
    flanker_clean |> select(subj_idx),
    dccs_clean |> select(subj_idx),
    pcps_clean |> select(subj_idx)
  ),
    bind_rows
  ) |> 
  distinct() |> 
  mutate(
    subj_idx = sample(subj_idx, n(), replace = FALSE),
    subj_idx_an = 1:n()
  )


# Synthetic Cognitive Task Data -------------------------------------------

## Mental Rotation Task ----

lmt_clean_synth <- 
  lmt_clean |> 
  left_join(anon_ids) |> 
  select(subj_idx = subj_idx_an, correct, RT) |> 
  arrange(subj_idx) |> 
  group_by(subj_idx) |> 
  # Calculate relevant summary stats of real ABCD participants
  mutate(
    prob_correct = sum(correct == 1) / n(),
    prob_missing = sum(is.na(RT)) / n()) |> 
  group_by(subj_idx, correct) |> 
  mutate(
    mean_RT = mean(log(RT), na.rm = T),
    sd_RT   = sd(log(RT), na.rm = T)
  ) %>%
  # Simulate accuracy and RT based on real summary stats
  mutate(
    correct = pmap_dbl(list(prob_correct), function(prob_correct) { 
      sample(c(1,0), size = 1, prob = c(prob_correct, 1-prob_correct))
      }),
    RT = pmap_dbl(list(prob_missing, mean_RT, sd_RT, correct), function(prob_missing, mean_RT, sd_RT, correct) {
      
      if(sample(c(TRUE,FALSE), 1, prob = c(prob_missing, 1-prob_missing))) { # If missing
        rt <- NA
      } else {
        rt <- rnorm(1, mean_RT, sd_RT)
      }
      rt
    }),
    RT = exp(RT),
    RT = ifelse(RT < 0.3, runif(1, 0.3, 0.6), RT),
    RT = ifelse(RT > 5, runif(1, 3, 5), RT)
    ) |> 
  ungroup() |> 
  select(-c(prob_correct, mean_RT, sd_RT, prob_missing))


## Flanker Task ----
  
flanker_clean_synth <- 
  flanker_clean |> 
  left_join(anon_ids) |> 
  select(subj_idx = subj_idx_an, correct, RT, condition) |> 
  arrange(subj_idx) |> 
  group_by(subj_idx, condition) |> 
  # Calculate relevant summary stats of real ABCD participants
  mutate(
    prob_correct = sum(correct == 1) / n()
  ) |> 
  group_by(subj_idx, correct, condition) |> 
  mutate(
    mean_RT = mean(log(RT), na.rm = T),
    sd_RT   = sd(log(RT), na.rm = T),
    sd_RT   = ifelse(is.na(sd_RT), 1.96*abs(mean_RT), sd_RT)
  ) %>%
  # Simulate accuracy and RT based on real summary stats
  mutate(
    correct = pmap_dbl(list(prob_correct), function(prob_correct) { 
      sample(c(1,0), size = 1, prob = c(prob_correct, 1-prob_correct))
    }),
    RT = pmap_dbl(list(mean_RT, sd_RT, correct), function(mean_RT, sd_RT, correct) {
      rt <- rnorm(1, mean_RT, sd_RT)
      rt
    }),
    RT = exp(RT),
    RT = ifelse(RT < 0.3, runif(1, 0.3, 0.6), RT),
    RT = ifelse(RT > 10, runif(1, 7, 10), RT)
  ) |> 
  ungroup() |> 
  select(-c(prob_correct, mean_RT, sd_RT))


## Attention Shifting Task ----

dccs_clean_synth <- 
  dccs_clean |> 
  left_join(anon_ids) |> 
  select(subj_idx = subj_idx_an, correct, RT, condition) |> 
  arrange(subj_idx) |> 
  group_by(subj_idx, condition) |> 
  # Calculate relevant summary stats of real ABCD participants
  mutate(
    prob_correct = sum(correct == 1) / n()
  ) |> 
  group_by(subj_idx, correct, condition) |> 
  mutate(
    mean_RT = mean(log(RT), na.rm = T),
    sd_RT   = sd(log(RT), na.rm = T),
    sd_RT   = ifelse(is.na(sd_RT), 1.96*abs(mean_RT), sd_RT)
  ) %>%
  # Simulate accuracy and RT based on real summary stats
  mutate(
    correct = pmap_dbl(list(prob_correct), function(prob_correct) { 
      sample(c(1,0), size = 1, prob = c(prob_correct, 1-prob_correct))
    }),
    RT = pmap_dbl(list(mean_RT, sd_RT, correct), function(mean_RT, sd_RT, correct) {
      rt <- rnorm(1, mean_RT, sd_RT)
      rt
    }),
    RT = exp(RT),
    RT = ifelse(RT < 0.3, runif(1, 0.3, 0.6), RT),
    RT = ifelse(RT > 10, runif(1, 7, 10), RT)
  ) |> 
  ungroup() |> 
  select(-c(prob_correct, mean_RT, sd_RT))

## Processing Speed Task ----

pcps_clean_synth <- 
  pcps_clean |> 
  left_join(anon_ids) |> 
  select(subj_idx = subj_idx_an, correct, RT) |> 
  arrange(subj_idx) |> 
  group_by(subj_idx) |> 
  # Calculate relevant summary stats of real ABCD participants
  mutate(
    prob_correct = sum(correct == 1) / n()
  ) |> 
  group_by(subj_idx, correct) |> 
  mutate(
    mean_RT = mean(log(RT), na.rm = T),
    sd_RT   = sd(log(RT), na.rm = T)
  ) %>%
  # Simulate accuracy and RT based on real summary stats
  mutate(
    correct = pmap_dbl(list(prob_correct), function(prob_correct) { 
      sample(c(1,0), size = 1, prob = c(prob_correct, 1-prob_correct))
    }),
    RT = pmap_dbl(list(mean_RT, sd_RT, correct), function(mean_RT, sd_RT, correct) {
      rt <- rnorm(1, mean_RT, sd_RT)
      rt
    }),
    RT = exp(RT),
    RT = ifelse(RT < 0.3, runif(1, 0.3, 0.6), RT),
    RT = ifelse(RT > 10, runif(1, 7, 10), RT)
  ) |> 
  ungroup() |> 
  select(-c(prob_correct, mean_RT, sd_RT))


# Synthetic Training/Test Sets --------------------------------------------

training_set_synth <- training_set |> 
  left_join(anon_ids) |> 
  select(-subj_idx) |> 
  rename(subj_idx = subj_idx_an) |> 
  group_by(rel_family_id) |> 
  mutate(rel_family_id = abs(rel_family_id + floor(runif(1, -100, 100))))


test_set_synth <- test_set |> 
  left_join(anon_ids) |> 
  select(-subj_idx) |> 
  rename(subj_idx = subj_idx_an) |> 
  group_by(rel_family_id) |> 
  mutate(rel_family_id = abs(rel_family_id + floor(runif(1, -100, 100))))

  


# Save Synthetic Data Files -----------------------------------------------

save(lmt_clean_synth, flanker_clean_synth, dccs_clean_synth, pcps_clean_synth, exclusions, file = "analysis_objects/tasks_clean_synth.RData")
readr::write_csv(training_set_synth, 'analysis_objects/training_set_synth.csv')
readr::write_csv(test_set_synth, 'analysis_objects/test_set_synth.csv')
