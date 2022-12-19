load(glue("{data_folder}/tasks_raw.RData"))

# Read TBI data
tryCatch(
  {
    tbi <- read_delim(file = "data/abcd_tbi01.txt") |> 
      filter(eventname == "baseline_year_1_arm_1") |> 
      select(subjectkey, tbi_ss_worst_overall) |> 
      rename(subj_idx = subjectkey) |> 
      mutate(tbi_ss_worst_overall = as.numeric(tbi_ss_worst_overall))
  },
  error=function(cond){ 
    message("Error: File not found. If you do not have access to the ABCD data, you can skip the scripts under `1_data_prep` and continue to the scripts under `2_analyses` using the synthetic data that are provided in the `data` folder. If you *do* have access to the ABCD data, make sure that you place the required data in the `data` folder. See the README file for more information.")
  }
)

exclusions <- list()

# 1. Mental Rotation task ----------------------------------------------------

## 1.1 Trial-level exclusions ----

plot_RTs(lmt_raw |> rename(correct = response), title = "Mental Rotation - Raw data RTs")

# Trials of the Little Man Task timed out after 5 s. So, we start out by removing trials that have RTs > 5s,
# which should not be possible.

lmt_clean <- lmt_raw |> 
  left_join(tbi) |>
  rename(correct = response) |> 
  filter(RT < 5.1 | is.na(RT)) |> 
  mutate(
    ex_missing_response = ifelse(is.na(RT) | is.na(correct), TRUE, FALSE),
    ex_fast_RT          = ifelse(RT < 0.3, TRUE, FALSE),
  ) |> 
  group_by(subj_idx) |> 
  mutate(ex_slow_RT = ifelse(scale(log(RT)) |> as.numeric() > 3, TRUE, FALSE)) |> 
  ungroup()


exclusions$lmt_trial <-  
  lmt_clean |> 
  summarise(
    ex_missing_response = sum(ifelse(is.na(RT)|is.na(correct), TRUE, FALSE) / n()) * 100,
    ex_fast_RT          = (sum(ex_fast_RT == TRUE, na.rm = T) / n()) * 100,
    ex_RT_above_5       = (sum(lmt_raw$RT > 5, na.rm = T)/nrow(lmt_raw)*100),
    ex_slow_RT          = (sum(ex_slow_RT == TRUE, na.rm = T) / n()) * 100
  )


# Apply trial-level exclusions
lmt_clean <- lmt_clean |> 
  filter(if_all(c(ex_fast_RT, ex_slow_RT), ~ . == FALSE | is.na(.))) |> 
  select(-starts_with('ex'))


## 1.2 Case-wise exclusions ----

lmt_clean |> 
  count(subj_idx) |> 
  ggplot(aes(n)) +
  geom_histogram()


exclusions$lmt_participant <- 
  lmt_clean |> 
  group_by(subj_idx) |> 
  summarise(
    ex_tbi             = ifelse(tbi_ss_worst_overall >= 3, TRUE, FALSE),
    ex_below_20_trials = ifelse(n() < 20, TRUE, FALSE),
    ex_0_accuracy      = ifelse(sum(correct == 1)/n()*100 == 0, TRUE, FALSE)
  ) |> 
  distinct() |> 
  ungroup() |> 
  summarise(
    ex_tbi             = sum(ex_tbi, na.rm = T),
    ex_below_20_trials = sum(ex_below_20_trials, na.rm = T),
    ex_0_accuracy      = sum(ex_0_accuracy, na.rm = TRUE)
    
  )


# Apply participant-level exclusions
lmt_clean <- lmt_clean |> 
  group_by(subj_idx) |> 
  mutate(
    ex_tbi             = ifelse(tbi_ss_worst_overall >= 3, TRUE, FALSE),
    ex_below_20_trials = ifelse(n() < 20, TRUE, FALSE),
    ex_0_accuracy      = ifelse(sum(correct == 1)/n()*100 == 0, TRUE, FALSE)
  ) |> 
  filter(if_all(c(ex_tbi, ex_below_20_trials, ex_0_accuracy), ~ . == FALSE)) |> 
  select(-starts_with('ex')) |> 
  mutate(task = 1) |> 
  ungroup() 

# Check RT and accuracy distribution
plot_meanRTs(lmt_clean, title = "Mental Rotation - RT After initial cleaning")
plot_meanAcc(lmt_clean, title = "Mental Rotation -  Accuracy after initial cleaning")

# Check for response biases
lmt_response_bias <- lmt_clean |> 
  group_by(subj_idx) |> 
  mutate(
    correct_response = case_when(
      lmt_response == "leftButton" & correct == 1 ~ 1,
      lmt_response == "rightButton" & correct == 1 ~ 2,
      lmt_response == 'leftButton' & correct == 0 ~ 2,
      lmt_response == "rightButton" & correct == 0 ~ 1
    )
  ) |> 
  summarise(
    actual_bias = sum(correct_response == 1) / n()*100,
    response    = sum(lmt_response == "leftButton")/n()*100,
    acc         = sum(correct == 1) / n()) |> 
  ungroup() |> 
  mutate(
    actual_bias = ifelse(actual_bias < 50, 100 - actual_bias, actual_bias),
    response = ifelse(response < 50, 100 - response, response),
  ) |> 
  ungroup() |> 
  filter(response > 70) |> 
  arrange(desc(response)) 

# Additional exclusions
lmt_clean <- lmt_clean |> 
  mutate(
    ex_response_biases = ifelse(subj_idx %in% unique(lmt_response_bias$subj_idx), TRUE, FALSE)
  ) 

exclusions$lmt_participant <- exclusions$lmt_participant |> 
  mutate(
    ex_response_biases = lmt_clean |> select(subj_idx, ex_response_biases) |> distinct() |> filter(ex_response_biases) |> nrow()
  )

# Exclude participants with response bias
lmt_clean <- lmt_clean |> 
  filter(if_all(starts_with("ex"), ~. == FALSE))

# Check RT and accuracy distribution
plot_meanRTs(lmt_clean, title = "LMT - RT After response bias removal")
plot_meanAcc(lmt_clean, title = "LMT -  Accuracy after response bias removal")



# 2. Flanker Task ---------------------------------------------------------

## 2.1 Trial-level exclusions ----


plot_RTs(flanker_raw, title = "Flanker - Raw data RTs")

ggplot(flanker_raw, aes(RT)) +
  geom_histogram()

quantile(flanker_raw$RT, c(0, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99, 0.995, 1))

# Based on the full distribution of RTs, we first filter all trials with a
# RT > 10 s are first removed, which corresponds to the time-out cut-off used in the task 
# (Although some RTs exceed 10 s).

flanker_clean <- flanker_raw |> 
  left_join(tbi) |>
  rename(condition = congruency) |> 
  filter(RT < 10) |> 
  mutate(
    ex_missing_response = ifelse(is.na(Response), TRUE, FALSE),
    ex_missing_acc      = ifelse(is.na(correct), TRUE, FALSE),
    ex_fast_RT          = ifelse(RT < 0.3, TRUE, FALSE),
  ) |> 
  group_by(subj_idx, condition) |> 
  mutate(ex_slow_RT = ifelse(scale(log(RT)) |> as.numeric() > 3, TRUE, FALSE)) |> 
  ungroup()

exclusions$flanker_trial <- 
  flanker_clean |> 
  summarise(
    ex_missing_response = (sum(ex_missing_response == TRUE) / n()) * 100,
    ex_fast_RT          = (sum(ex_fast_RT == TRUE, na.rm = T) / n()) * 100,
    ex_slow_RT          = (sum(ex_slow_RT == TRUE, na.rm = T) / n()) * 100,
    ex_RT_above_10       = (sum(flanker_raw$RT > 10)/nrow(flanker_raw)*100)
  )


# Apply trial-level exclusions
flanker_clean <- flanker_clean |> 
  filter(if_all(c(ex_missing_response, ex_fast_RT, ex_slow_RT), ~ . == FALSE)) |> 
  select(-starts_with('ex'))


## 2.2 Case-wise exclusions ----

flanker_clean |> 
  count(subj_idx) |> 
  ggplot(aes(n)) +
  geom_histogram(bins = 50)

exclusions$flanker_participant <- 
  flanker_clean |> 
  group_by(subj_idx) |> 
  summarise(
    ex_tbi             = ifelse(tbi_ss_worst_overall >= 3, TRUE, FALSE),
    ex_below_15_trials = ifelse(n() < 18, TRUE, FALSE),
  ) |> 
  distinct() |> 
  ungroup() |> 
  summarise(
    ex_tbi      = sum(ex_tbi, na.rm = T),
    ex_below_15_trials = sum(ex_below_15_trials, na.rm = T)
  )


# Apply participant-level exclusions
flanker_clean <- flanker_clean |> 
  group_by(subj_idx) |> 
  mutate(
    ex_tbi             = ifelse(tbi_ss_worst_overall >= 3, TRUE, FALSE),
    ex_below_15_trials = ifelse(n() < 18, TRUE, FALSE)
  ) |> 
  filter(if_all(c(ex_tbi, ex_below_15_trials), ~ . == FALSE)) |> 
  select(-starts_with('ex')) |> 
  mutate(task = 2) |> 
  ungroup()


# Check RT and accuracy distribution
plot_meanRTs(flanker_clean, title = "Flanker - RT After initial cleaning")
plot_meanAcc(flanker_clean, title = "Flanker -  Accuracy after initial cleaning")

# Check for response biases
flanker_response_bias <- flanker_clean |> 
  group_by(subj_idx) |> 
  mutate(
    correct_response = case_when(
      Response == 1 & correct == 1 ~ 1,
      Response == 2 & correct == 1 ~ 2,
      Response == 1 & correct == 0 ~ 2,
      Response == 2 & correct == 0 ~ 1
    )
  ) |> 
  summarise(
    actual_bias = sum(correct_response == 1) / n()*100,
    response    = sum(Response == 1)/n()*100,
    acc         = sum(correct == 1) / n()) |> 
  ungroup() |> 
  mutate(
    actual_bias = ifelse(actual_bias < 50, 100 - actual_bias, actual_bias),
    response = ifelse(response < 50, 100 - response, response),
  ) |> 
  ungroup() |> 
  filter(response > 70) |> 
  arrange(desc(response)) 

# Only one participant with a response bias 72.2% while the actual bias was 61.1%
# Does not look like an odd data point, so we'll leave it alone.

exclusions$flanker_participant <- exclusions$flanker_participant |> 
  mutate(ex_response_biases = 0)


# 3. Processing Speed Task ------------------------------------------------

## 3.1 Trial-level exclusions ----

plot_RTs(pcps_raw, title = "PCPS - Raw data RTs")

# Several severe outliers, even extending the 90 seconds that the task is
# supposed to take.

quantile(pcps_raw$RT, c(0, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99, 0.995, 1))

# Based on the full distribution of RTs, we first filter all trials with a
# RT > 10 s to get rid of extremely long RTs.
pcps_clean1 <- pcps_raw |> 
  left_join(tbi) |> 
  filter(RT < 10) |> 
  mutate(
    ex_missing_response = ifelse(is.na(Response), TRUE, FALSE),
    ex_missing_acc      = ifelse(is.na(correct), TRUE, FALSE),
    ex_fast_RT          = ifelse(RT < 0.3, TRUE, FALSE),
  ) |> 
  group_by(subj_idx) |> 
  mutate(ex_slow_RT = ifelse(scale(log(RT)) |> as.numeric() > 3, TRUE, FALSE)) |> 
  ungroup()

plot_RTs(pcps_clean1, title = "PCPS - RTs After first trial-level exclusions")

# The plot shows a number of fast outliers on incorrect trials, 
# which can bias the analyses. Therefore, we also exclude trials
# < -3 SD from the intra-individual mean

pcps_clean2 <- pcps_clean1 |> 
  group_by(subj_idx) |> 
  mutate(ex_fast_intra_RT = ifelse(scale(log(RT))|> as.numeric() < -3, TRUE, FALSE)) |> 
  ungroup()

exclusions$pcps_trial <- 
  pcps_clean2 |> 
  summarise(
    ex_missing_response = (sum(ex_missing_response == TRUE) / n()) * 100,
    ex_fast_RT          = (sum(ex_fast_RT == TRUE, na.rm = T) / n()) * 100,
    ex_slow_RT          = (sum(ex_slow_RT == TRUE, na.rm = T) / n()) * 100,
    ex_fast_intra_RT    = (sum(ex_fast_intra_RT == TRUE, na.rm = T) / n()) * 100,
    ex_RT_above_10      = sum(pcps_raw$RT > 10, na.rm = T) / nrow(pcps_raw) * 100
  )


# Apply trial-level exclusions
pcps_clean <- pcps_clean2 |> 
  filter(if_all(c(ex_missing_response, ex_fast_RT, ex_slow_RT, ex_fast_intra_RT), ~ . == FALSE)) |> 
  select(-starts_with('ex'))

plot_RTs(pcps_clean, title = "PCPS - RTs After trial-level exclusions")

# Number of fast outliers is reduced, but not fully.


## 3.2 Case-wise exclusions ----

# < 15 (n = 34)
pcps_clean |> 
  count(subj_idx) |> 
  ggplot(aes(n)) +
  geom_histogram() +
  labs(title = "Number of trials for PCPS")

# Check RT and accuracy distribution
plot_meanRTs(pcps_clean, title = "PCPS - Mean RT before case-wise exclusions")
plot_meanAcc(pcps_clean, title = "PCPS -  Mean Accuracy before case-wise exclusions")



exclusions$pcps_participant <- 
  pcps_clean |> 
  group_by(subj_idx) |> 
  summarise(
    ex_tbi             = ifelse(tbi_ss_worst_overall >= 3, TRUE, FALSE),
    ex_below_15_trials = ifelse(n() < 15, TRUE, FALSE),
    ex_0_accuracy      = ifelse(sum(correct == 1)/n()*100 == 0, TRUE, FALSE)
  ) |> 
  distinct() |> 
  ungroup() |> 
  summarise(
    ex_tbi             = sum(ex_tbi, na.rm = T),
    ex_below_15_trials = sum(ex_below_15_trials, na.rm = T),
    ex_0_accuracy      = sum(ex_0_accuracy, na.rm = T)
  )


# Apply participant-level exclusions
pcps_clean <- pcps_clean |> 
  group_by(subj_idx) |> 
  mutate(
    ex_tbi             = ifelse(tbi_ss_worst_overall >= 3, TRUE, FALSE),
    ex_below_15_trials = ifelse(n() < 15, TRUE, FALSE),
    ex_0_accuracy      = ifelse(sum(correct == 1)/n()*100 == 0, TRUE, FALSE)
  ) |> 
  filter(if_all(c(ex_tbi, ex_below_15_trials, ex_0_accuracy), ~ . == FALSE)) |> 
  select(-starts_with('ex')) |> 
  mutate(task = 4) |> 
  ungroup()

# Check RT and accuracy distribution
plot_meanRTs(pcps_clean, title = "PCPS - mean RT After case-wise cleaning")
plot_meanAcc(pcps_clean, title = "PCPS -  mean Accuracy after case-wise cleaning")

# Check for response biases
pcps_response_bias <- pcps_clean |> 
  group_by(subj_idx) |> 
  mutate(
    correct_response = case_when(
      Response == 1 & correct == 1 ~ 1,
      Response == 2 & correct == 1 ~ 2,
      Response == 1 & correct == 0 ~ 2,
      Response == 2 & correct == 0 ~ 1
    )
  ) |> 
  summarise(
    actual_bias = sum(correct_response == 1) / n()*100,
    response    = sum(Response == 1)/n()*100,
    acc         = sum(correct == 1) / n()) |> 
  ungroup() |> 
  mutate(
    actual_bias = ifelse(actual_bias < 50, 100 - actual_bias, actual_bias),
    response = ifelse(response < 50, 100 - response, response),
  ) |> 
  ungroup() |> 
  filter(response > 70) |> 
  arrange(desc(response)) 

# Additional case-wise exclusions

pcps_clean <- pcps_clean |> 
  mutate(
    ex_response_biases = ifelse(subj_idx %in% unique(pcps_response_bias$subj_idx), TRUE, FALSE)
    ) |> 
  group_by(subj_idx) |> 
  mutate(
    ex_decreasing_effort = ifelse((sum(correct == 1)/n() * 100) < 40, TRUE, FALSE)
  )

exclusions$pcps_participant <- exclusions$pcps_participant |> 
  mutate(
    ex_response_biases = pcps_clean |> select(subj_idx, ex_response_biases) |> distinct() |> filter(ex_response_biases) |> nrow(),
    ex_decreasing_effort = pcps_clean |> select(subj_idx, ex_decreasing_effort) |> distinct() |> filter(ex_decreasing_effort) |> nrow()
  )

# Exclude participants with response bias
pcps_clean <- pcps_clean |> 
  filter(if_all(starts_with("ex"), ~. == FALSE))

# Check RT and accuracy distribution
plot_meanRTs(pcps_clean, title = "PCPS - RT After response bias removal")
plot_meanAcc(pcps_clean, title = "PCPS -  Accuracy after response bias removal")




# 4. Attention Shifting task ----------------------------------------------

## 4.1 Trial-level exclusions ----


plot_RTs(dccs_raw, title = "DCCS - Raw data RTs")


quantile(dccs_raw$RT, c(0, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99, 0.995, 1))

# Based on the full distribution of RTs, we first filter all trials with a
# RT > 10 s are removed (this corresponds to the timeout cut-off used during the task).


dccs_clean <- dccs_raw |> 
  left_join(tbi) |> 
  rename(condition = ItemID) |> 
  filter(RT < 10) |> 
  mutate(
    ex_missing_response = ifelse(is.na(Response), TRUE, FALSE),
    ex_missing_acc      = ifelse(is.na(correct), TRUE, FALSE),
    ex_fast_RT          = ifelse(RT < 0.3, TRUE, FALSE),
  ) |> 
  group_by(subj_idx, condition) |> 
  mutate(ex_slow_RT = ifelse(scale(log(RT)) |> as.numeric() > 3, TRUE, FALSE)) |> 
  ungroup()

exclusions$dccs_trial <- 
  dccs_clean |> 
  summarise(
    ex_missing_response = (sum(ex_missing_response == TRUE) / n()) * 100,
    ex_fast_RT          = (sum(ex_fast_RT == TRUE, na.rm = T) / n()) * 100,
    ex_slow_RT          = (sum(ex_slow_RT == TRUE, na.rm = T) / n()) * 100,
    ex_RT_above_10      = sum(dccs_raw$RT > 10, na.rm = T) / nrow(dccs_raw) * 100
  )


# Apply trial-level exclusions
dccs_clean <- dccs_clean |> 
  filter(if_all(c(ex_missing_response, ex_fast_RT, ex_slow_RT), ~ . == FALSE)) |> 
  select(-starts_with('ex'))


## 4.2 Case-wise exclusions ----

# < 20 (n = 19) 
dccs_clean |> 
  count(subj_idx) |> 
  ggplot(aes(n)) +
  geom_histogram()

# Check RT and accuracy distribution
plot_meanRTs(dccs_clean, title = "DCCS - Mean RT before case-wise exclusions")
plot_meanAcc(dccs_clean, title = "DCCS -  Mean Accuracy before case-wise exclusions")




exclusions$dccs_participant <- 
  dccs_clean |> 
  group_by(subj_idx) |> 
  summarise(
    ex_tbi             = ifelse(tbi_ss_worst_overall >= 3, TRUE, FALSE),
    ex_below_20_trials = ifelse(n() < 20, TRUE, FALSE)
  ) |> 
  distinct() |> 
  ungroup() |> 
  summarise(
    ex_tbi             = sum(ex_tbi, na.rm = T),
    ex_below_20_trials = sum(ex_below_20_trials, na.rm = T)
  )


# Apply participant-level exclusions
dccs_clean <- dccs_clean |> 
  group_by(subj_idx) |> 
  mutate(
    ex_tbi             = ifelse(tbi_ss_worst_overall >= 3, TRUE, FALSE)  ,
    ex_below_20_trials = ifelse(n() < 20, TRUE, FALSE)
  ) |> 
  filter(if_all(c(ex_tbi, ex_below_20_trials), ~ . == FALSE)) |> 
  select(-starts_with('ex')) |> 
  mutate(task = 3)|> 
  ungroup()


# Check RT and accuracy distribution
plot_meanRTs(dccs_clean, title = "DCCS - mean RT After case-wise cleaning")
plot_meanAcc(dccs_clean, title = "DCCS -  mean Accuracy after case-wise cleaning")

# Check for response biases
dccs_response_bias <- dccs_clean |> 
  group_by(subj_idx) |> 
  mutate(
    correct_response = case_when(
      Response == 1 & correct == 1 ~ 1,
      Response == 2 & correct == 1 ~ 2,
      Response == 1 & correct == 0 ~ 2,
      Response == 2 & correct == 0 ~ 1
    )
  ) |> 
  summarise(
    actual_bias = sum(correct_response == 1) / n()*100,
    response    = sum(Response == 1)/n()*100,
    acc         = sum(correct == 1) / n()) |> 
  ungroup() |> 
  mutate(
    actual_bias = ifelse(actual_bias < 50, 100 - actual_bias, actual_bias),
    response = ifelse(response < 50, 100 - response, response),
  ) |> 
  ungroup() |> 
  filter(response > 70) |> 
  arrange(desc(response)) 

# Additional case-wise exclusions

# Several participants showed (near) perfect accuracy on the switch trials, but
# (close to) 0 accuracy on the easier repeat trials. Thus, they made switches
# on all or nearly all trials.
dccs_n_switches <- dccs_clean |> 
  group_by(subj_idx, condition) |> 
  summarise(acc = sum(correct == 1)/n()*100) |> 
  pivot_wider(names_from = 'condition', values_from = 'acc') |> 
  group_by(`repeat`, switch) |> 
  mutate(n = n())

ggplot(dccs_n_switches,aes(x = `repeat`, y = switch)) + geom_point(aes(size = n))

# We decided to remove participants with < 25% accuracy on repeat trials combined
# with > 75% accuracy on switch trials.


dccs_clean <- dccs_clean |> 
  mutate(
    ex_response_biases = ifelse(subj_idx %in% unique(pcps_response_bias$subj_idx), TRUE, FALSE),
    ex_switching_bias = ifelse(subj_idx %in% unique(dccs_n_switches |> filter(`repeat` < 25 & switch > 75) |> pull(subj_idx)), TRUE, FALSE)
  ) 

exclusions$dccs_participant <- exclusions$dccs_participant |> 
  mutate(
    ex_response_biases = dccs_clean |> select(subj_idx, ex_response_biases) |> distinct() |> filter(ex_response_biases) |> nrow(),
    ex_switching_bias = dccs_clean |> select(subj_idx, ex_switching_bias) |> distinct() |> filter(ex_switching_bias) |> nrow()
  )

# Exclude participants with response bias
dccs_clean <- dccs_clean |> 
  filter(if_all(starts_with("ex"), ~. == FALSE))

# Check RT and accuracy distribution
plot_meanRTs(dccs_clean, title = "DCCS - RT After response bias removal")
plot_meanAcc(dccs_clean, title = "DCCS -  Accuracy after response bias removal")



# Task distributions after cleaning
list(pcps_clean, flanker_clean, lmt_clean, dccs_clean) |> 
  map_df(function(x) x |> select(RT, task, correct)) |> 
  mutate(
    task = case_when(
      task == 1 ~ "Mental Rotation",
      task == 2 ~ "Flanker",
      task == 3 ~ "Attention Shifting",
      task == 4 ~ "Processing Speed",
    )) |> 
  ggplot(aes(RT, group = factor(correct), fill = factor(correct))) +
  geom_density(alpha = 0.6) +
  facet_wrap(~task, scales = 'free')

# Round all exclusions to two decimals
exclusions <- exclusions |> map(function(x) x |> mutate(across(everything(), ~round(., 2) |> as.character())))

save(lmt_clean, flanker_clean, pcps_clean, dccs_clean, nih_ref_ids, exclusions, file = "analysis_objects/tasks_clean.RData")







