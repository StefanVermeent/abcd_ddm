
# Libraries and files -----------------------------------------------------

source('scripts/dependencies.R')
source('scripts/custom_functions/read-functions.R')

data_folder <- "closed_data"

load(glue('{data_folder}/tasks_raw.RData'))


# Read TBI data
tbi <- read_delim(file = "closed_data/abcd_tbi01.txt") |> 
  filter(eventname == "baseline_year_1_arm_1") |> 
  select(subjectkey, tbi_ss_worst_overall) |> 
  rename(subj_idx = subjectkey) |> 
  mutate(tbi_ss_worst_overall = as.numeric(tbi_ss_worst_overall))


exclusions <- list()

# 1. Mental Rotation task ----------------------------------------------------

## 1.1 Trial-level exclusions ----

ggplot(lmt_raw, aes(RT)) +
  geom_histogram()

# Trials of the Little Man Task timed out after 5 s. So, we start out by removing trials that have RTs > 5s,
# which should not be possible.

lmt_clean <- lmt_raw |> 
  left_join(tbi) |>
  rename(correct = response) |> 
  filter(RT < 5 | is.na(RT)) |> 
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
    ex_missing_response = sum(ifelse(is.na(RT), TRUE, FALSE) / n()) * 100,
    ex_fast_RT          = (sum(ex_fast_RT == TRUE, na.rm = T) / n()) * 100,
    ex_RT_above_5       = (sum(lmt_raw$RT > 5, na.rm = T)/nrow(lmt_raw)*100),
    ex_slow_RT          = (sum(ex_slow_RT == TRUE, na.rm = T) / n()) * 100
  )


# Apply trial-level exclusions
lmt_clean <- lmt_clean |> 
  filter(if_all(c(ex_fast_RT, ex_slow_RT), ~ . == FALSE)) |> 
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


# 2. Flanker Task ---------------------------------------------------------

## 2.1 Trial-level exclusions ----

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




# 3. Processing Speed Task ------------------------------------------------

## 3.1 Trial-level exclusions ----

ggplot(pcps_raw, aes(RT)) +
  geom_histogram(bins = 1000) +
  coord_cartesian(xlim = c(0, 25))

quantile(pcps_raw$RT, c(0, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99, 0.995, 1))

# Based on the full distribution of RTs, we first filter all trials with a
# RT > 10 s to get rid of extremely long RTs.
pcps_clean <- pcps_raw |> 
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

pcps_clean |> 
  ggplot(aes(RT, group = factor(correct), fill = factor(correct))) +
  geom_density(alpha = 0.6)

# The plot shows a number of fast outliers on incorrect trials, 
# which can bias the analyses. Therefore, we also exclude trials
# < -3 SD from the intra-individual mean

pcps_clean <- pcps_clean |> 
  group_by(subj_idx) |> 
  mutate(ex_fast_intra_RT = ifelse(scale(log(RT))|> as.numeric() < -3, TRUE, FALSE)) |> 
  ungroup()



exclusions$pcps_trial <- 
  pcps_clean |> 
  summarise(
    ex_missing_response = (sum(ex_missing_response == TRUE) / n()) * 100,
    ex_fast_RT          = (sum(ex_fast_RT == TRUE, na.rm = T) / n()) * 100,
    ex_slow_RT          = (sum(ex_slow_RT == TRUE, na.rm = T) / n()) * 100,
    ex_fast_intra_RT    = (sum(ex_fast_intra_RT == TRUE, na.rm = T) / n()) * 100,
    ex_RT_above_10      = sum(pcps_raw$RT > 10, na.rm = T) / nrow(pcps_raw) * 100
  )


# Apply trial-level exclusions
pcps_clean <- pcps_clean |> 
  filter(if_all(c(ex_missing_response, ex_fast_RT, ex_slow_RT, ex_fast_intra_RT), ~ . == FALSE)) |> 
  select(-starts_with('ex'))

pcps_clean |> 
  ggplot(aes(RT, group = factor(correct), fill = factor(correct))) +
  geom_density(alpha = 0.6)

## 3.2 Case-wise exclusions ----

# < 15 (n = 34)
pcps_clean |> 
  count(subj_idx) |> 
  ggplot(aes(n)) +
  geom_histogram() +
  labs(title = "Number of trials for PCPS")

pcps_clean |> 
  group_by(subj_idx) |> 
  summarise(acc = sum(correct == 1)/n()*100) |> 
  ggplot(aes(acc)) +
  geom_histogram() +
  labs(title = "Accuracy for PCPS")


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


# 4. Attention Shifting task ----------------------------------------------

## 4.1 Trial-level exclusions ----

ggplot(dccs_raw, aes(RT)) +
  geom_histogram() 

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


# 5. Response bias -----------------------------------------------------------
# Calculate response bias as exclusion criterium

response_bias_ids <- list('dccs_clean', 'flanker_clean', 'pcps_clean') |> 
  map_df(function(x) {
    
      x |> 
      rlang::parse_expr() |>
      rlang::eval_tidy() |> 
      group_by(subj_idx) |> 
      summarise(response = sum(Response == 1)/n()*100) |> 
      ungroup() |> 
      mutate(
        response = ifelse(response < 50, 100 - response, response),
        task     = x
      )
  }) |> 
  group_by(subj_idx) |> 
  filter(response > 70) |> 
  mutate(n = n()) |> 
  filter(n >1) |>   
  distinct(subj_idx) |> 
  pull(subj_idx) 

exclusions$response_bias <- 
  tibble(
    response_bias = response_bias_ids |> 
      length()
  )
  



# 6. Final cleaning across all tasks --------------------------------------

lmt_clean <- lmt_clean |> 
  filter(!subj_idx %in% response_bias_ids)

flanker_clean<- flanker_clean|> 
  filter(!subj_idx %in% response_bias_ids)

pcps_clean <- pcps_clean |> 
  filter(!subj_idx %in% response_bias_ids) 

dccs_clean <- dccs_clean |> 
  filter(!subj_idx %in% response_bias_ids)

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




demographics$postcleaning_n <- 
  list(
    lmt     = lmt_clean |> pull(subj_idx) |> unique() |> length(),
    flanker = flanker_clean |> pull(subj_idx) |> unique() |> length(),
    dccs    = dccs_clean |> pull(subj_idx) |> unique() |> length(),
    pcps    = pcps_clean |> pull(subj_idx) |> unique() |> length(),

    # N participants with trial-level cognitive data
    full_n_trial = 
      unique(
        c(
          lmt_clean |> pull(subj_idx) |> unique(),
          flanker_clean |> pull(subj_idx) |> unique(),
          dccs_clean |> pull(subj_idx) |> unique(),
          pcps_clean |> pull(subj_idx) |> unique()
        )
      )  |> length(),

    # N participants with trial-level cognitive data on all tasks
    shared_n = 
      Reduce(intersect, 
             list(
               lmt_clean |> pull(subj_idx) |> unique(),
               flanker_clean |> pull(subj_idx) |> unique(),
               dccs_clean |> pull(subj_idx) |> unique(),
               pcps_clean|> pull(subj_idx) |> unique()
          
             )) |> length()
  )

demographics$postcleaning_n <- demographics$postcleaning_n |> 
  map(function(x) prettyNum(x, big.mark = ","))

# Subjects that have data available on all tasks
shared_ids <- 
  Reduce(intersect, 
         list(
           lmt_clean |> pull(subj_idx) |> unique(),
           flanker_clean |> pull(subj_idx) |> unique(),
           dccs_clean |> pull(subj_idx) |> unique(),
           pcps_clean |> pull(subj_idx) |> unique()
         ))



#lmt_clean <- lmt_clean |> filter(subj_idx %in% shared_ids)
#flanker_clean <- flanker_clean |> filter(subj_idx %in% shared_ids)
#dccs_clean <- dccs_clean |> filter(subj_idx %in% shared_ids)
#pcps_clean <- pcps_clean |> filter(subj_idx %in% shared_ids)



# Task-descriptives -------------------------------------------------------

demographics$task_descriptives <- 
  list(pcps_clean, flanker_clean, lmt_clean, dccs_clean) |> 
  map_df(function(x) {
    
      x |> 
        group_by(subj_idx, task) |> 
        summarise(
          rt = mean(RT, na.rm = T),
          acc     = sum(correct)/n() * 100,
        ) |> 
        group_by(task) |> 
        summarise(
          across(
            c(rt, acc),
            list(mean = mean, sd = sd, min = min, max = max)
          ))
  }) |> 
  select(-c(rt_min, rt_max)) |> 
  mutate(
    task = case_when(
      task == 1 ~ "Mental Rotation",
      task == 2 ~ "Flanker",
      task == 3 ~ "Attention Shifting",
      task == 4 ~ "Processing Speed"
    ),
    mean_rt = str_c(round(rt_mean, 2), " (", round(rt_sd, 2), ")"),
    mean_acc = str_c(round(acc_mean, 2), " (", round(acc_sd, 2), ")"),
    acc_min  = as.character(round(acc_min,2)),
    acc_max  = as.character(round(acc_max,2))
  ) |> 
  select(task, mean_rt, mean_acc, acc_min, acc_max)
  

# Round all exclusions to two decimals
exclusions <- exclusions |> map(function(x) x |> mutate(across(everything(), ~round(., 2) |> as.character())))

save(
  lmt_clean, 
  flanker_clean, 
  pcps_clean, 
  dccs_clean, 
  
  exclusions,
  demographics,
  
  file = glue('{data_folder}/tasks_clean.RData'))







