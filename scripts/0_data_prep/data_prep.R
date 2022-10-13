
# Libraries and functions -------------------------------------------------

library(tidyverse)
library(utils)
library(gert)
library(glue)
library(stringdist)


source('scripts/custom_functions/read-functions.R')
source('scripts/custom_functions/general-functions.R')

data_folder <- "closed_data"

# Read TBI data
tbi <- read_delim(file = "closed_data/abcd_tbi01.txt") |> 
  filter(eventname == "baseline_year_1_arm_1") |> 
  select(subjectkey, tbi_ss_worst_overall) |> 
  rename(subj_idx = subjectkey) |> 
  mutate(tbi_ss_worst_overall = as.numeric(tbi_ss_worst_overall))

# Read NIH summary scores for participant ID verification

nih_tb_sum <- readr::read_tsv(file = 'closed_data/abcd_tb_tlb01/abcd_tb_tlb01.txt') |> 
  filter(collection_id != "collection_id")

nih_ids <- nih_tb_sum |> filter(str_detect(eventname, "^baseline")) |>  pull(subjectkey) |> unique()

# 1. Little Man Task ------------------------------------------------------

## 1.1 File Preparations ----

# Only keep baseline measures
list.files(file.path(data_folder, "abcd_lmtlb01"), pattern = "2_year_follow_up", full.names = TRUE) |> 
  map(function(x) file.remove(x))

# Extract baseline measures
list.files(file.path(data_folder, "abcd_lmtlb01"), full.names = TRUE) |> 
  map(function(x) utils::unzip(x, exdir = data_folder))


## 1.2 Read files ----

# First, read a single file to develop parsing strategy
lmt <- read_csv(file = file.path(data_folder, "LMT/baseline_year_1_arm_1/NDAR_INV00BD7VDC_baseline_year_1_arm_1_lmt.csv"))

lmt <- lmt |> 
  select(subject, site, lmt_blockcode, lmt_trialnum, lmt_correct, lmt_latency) |> 
  filter(lmt_blockcode == "test") |> 
  rename(
    subj_idx = subject,
    response = lmt_correct,
    RT       = lmt_latency
  ) |> 
  mutate(RT = RT / 1000)

# Next, read files of all subjects
lmt <- read_csv(file = "list.files(file.path(data_folder, 'LMT/baseline_year_1_arm_1'), full.names = TRUE)") 

lmt <- lmt |> 
  map(function(x) {
    x |> mutate(lmt_response = as.character(lmt_response))
  }) |> 
  reduce(bind_rows) |> 
  select(subject, site, lmt_blockcode, lmt_trialnum, lmt_trialcode, lmt_correct, lmt_latency) |> 
  filter(lmt_blockcode == "test", !lmt_trialcode %in% c('instructions', 'homeBase')) |> 
  rename(
    subj_idx = subject,
    response = lmt_correct,
    RT       = lmt_latency
  ) |> 
  mutate(RT = RT / 1000) |> 
  left_join(tbi)

# data check: do all participants have 32 trials?
assertthat::assert_that(all(lmt |> group_by(subj_idx) |> count() |> pull() == 32))



## 1.3 Clean data ----

lmt_clean <- lmt |> 
  mutate(
    RT = ifelse(RT < 0, NA, RT),
    ex_missing_RT   = ifelse(is.na(RT), TRUE, FALSE),
    ex_fast_RT      = ifelse(RT < 0.3, TRUE, FALSE)
  ) |> 
  group_by(subj_idx) |> 
  mutate(ex_slow_RT = ifelse(scale(log(RT)) |> as.numeric() > 3, TRUE, FALSE)) |> 
  ungroup()

# summary list of trial-level exclusions
## - No missing responses
## - No responses < 300 ms
## - No log-transformed responses > 3 SD of the participant's mean RT
exclusions <- 
  list(
    lmt_trial = lmt_clean |> 
      summarise(
        ex_missing_RT = (sum(ex_missing_RT == TRUE) / n()) * 100,
        ex_fast_RT    = (sum(ex_fast_RT == TRUE, na.rm = T) / n()) * 100,
        ex_slow_RT    = (sum(ex_slow_RT == TRUE, na.rm = T) / n()) * 100
      )
  )

# Apply trial-level exclusions
lmt_clean <- lmt_clean |> 
  filter(if_all(c(ex_missing_RT, ex_fast_RT, ex_slow_RT), ~ . == FALSE)) |> 
  select(-starts_with('ex'))


# summary list of participant-level exclusions
## - No participants with fewer than 20 trials
## - No participants performing at chance level
exclusions$lmt_participant <- 
  lmt_clean |> 
  group_by(subj_idx) |> 
  summarise(
    ex_n_trials = ifelse(n() < 20, TRUE, FALSE),
    ex_chance   = ifelse((sum(response, na.rm=T)/n())*100 < gbinom(32, 0.5), TRUE, FALSE),
    ex_tbi      = ifelse(tbi_ss_worst_overall >= 3, TRUE, FALSE)
  ) |> 
  distinct() |> 
  ungroup() |> 
  summarise(
    ex_n_trials = sum(ex_n_trials),
    ex_chance   = sum(ex_chance),
    ex_tbi      = sum(ex_tbi, na.rm = T)
  )
  
 
# Apply participant-level exclusions
lmt_clean <- lmt_clean |> 
  group_by(subj_idx) |> 
  mutate(
    ex_n_trials = ifelse(n() < 20, TRUE, FALSE),
   # ex_chance   = ifelse((sum(response, na.rm=T)/n())*100 < gbinom(32, 0.5), TRUE, FALSE),
    ex_tbi      = ifelse(tbi_ss_worst_overall >= 3, TRUE, FALSE)  
  ) |> 
  filter(if_all(c(ex_n_trials, ex_tbi), ~ . == FALSE)) |> 
  select(-starts_with('ex'))


# 2. NIH Toolbox tasks -------------------------------------------------------

## 2.1 File preparations ----

## TODO: unzip files automatically

# Fix file names
list.files(glue("{data_folder}/abcd_tb_tlb01/NIHTB/NIHTB"), full.names = TRUE) |> 
  walk(function(x){
    
    new_name <- x |> 
      str_remove(pattern = ".csv") |> 
      str_c(".csv") |> 
      str_replace_all("\\s", "_")

    
    file.rename(from = x, to = new_name)
  })



## 2.2 Read files ----

# There are three file types with different data structures that have to be read in separately.

nih_p1 <- read_csv(file = "list.files(glue('{data_folder}/abcd_tb_tlb01/NIHTB/NIHTB'), full.names = TRUE) |> str_subset(pattern = 'Narrow_Structure')")
nih_p2 <- read_csv(file = "list.files(glue('{data_folder}/abcd_tb_tlb01/NIHTB/NIHTB'), full.names = TRUE) |> str_subset(pattern = '[0-9]_Assessment')")
nih_p3 <- read_csv(file = "list.files(glue('{data_folder}/abcd_tb_tlb01/NIHTB/NIHTB'), full.names = TRUE) |> str_subset(pattern = 'Registration')")



nih_p1_parsed <- nih_p1 |> 
  reduce(bind_rows)

plan("multisession", workers = 4)

nih_p2_parsed <- nih_p2 |> 
  future_map(function(x) {
    x |> mutate(across(everything(), as.character)) # Resolve combining issues by making all all variables character vectors.
  }, .options = furrr_options(seed = TRUE)) |> 
  future_map(function(x) {
    x |> 
      select(PIN, ItmOrdr, Inst, ItemID, Response, Score, Position, ResponseTime, AssessmentName = `Assessment Name`, InstStarted, InstEnded) |> 
      filter(str_detect(Inst, c("Pattern|Dimensional|Flanker|Picture Vocabulary"))) |> 
      filter(!str_detect(PIN, pattern = "Test|test|testing|Testing"))
  }) |> 
  reduce(bind_rows)

### 2.2.1 Flanker Task ----

nih_flanker_ids <- nih_tb_sum |> 
  filter(str_detect(eventname, "^baseline")) |>  
  filter(!is.na(nihtbx_flanker_uncorrected)) |> 
  pull(subjectkey) |> 
  unique() 



# Read data
flanker_raw <- 
  bind_rows(
    nih_p1_parsed |> 
      filter(str_detect(InstrumentTitle, "Flanker Inhibitory Control")) |> 
      filter(Key %in% c("Response", "Score", "ResponseTime", "Position", "AssessmentName", "InstrumentStartedTimestamp", "InstrumentEndedTimestamp")) |> 
      distinct() |> # Remove duplicate rows
      pivot_wider(names_from = "Key", values_from = "Value") |> 
      select(PIN, ItemID, Response, ResponseTime, Score, Position, AssessmentName, InstrumentStartedTimestamp, InstrumentEndedTimestamp) |> 
      rename(
        subj_idx = PIN,
        RT       = ResponseTime,
        correct  = Score
      ),
    nih_p2_parsed |> 
      filter(str_detect(Inst, "Flanker Inhibitory Control")) |> 
      select(PIN, ItemID, Response, Score, Position, ResponseTime, AssessmentName, InstStarted, InstEnded) |> 
      rename(
        subj_idx = PIN,
        RT       = ResponseTime,
        correct  = Score,
        InstrumentStartedTimestamp = InstStarted,
        InstrumentEndedTimestamp   = InstEnded
      )
  ) |> 
  mutate(across(c(Response, RT, correct, Position), as.numeric)) |> 
  filter(!str_detect(ItemID, "PRAC"), str_detect(ItemID, "ARROW")) |> 
  mutate(congruency = ifelse(str_detect(ItemID, "_CONGRUENT"), "congruent", "incongruent"))

#Both in sumset and trial data: 10548
#In trial data but not in sumset: 1104
#In sumset but not in trial data: 1175

# Collection of data manipulations to fix typos in subject IDs
flanker_raw <- flanker_raw |> 
  fix_subject_ids(data = _) |> # Get all subject IDs in the correct NDAR_INVXXXXX format
  fix_id_typos(data = _, reference = nih_ids) # Calculate Levenshtein distances between ids in this trial-level dataset compared to the summary score dataset in order to detect and fix typos.

# Before fixing typos:
#Both in sumset and trial data: 10674
#In trial data but not in sumset: 479
#In sumset but not in trial data: 1202

# After fixing typos:
#Both in sumset and trial data: 10700
#In trial data but not in sumset: 314
#In sumset but not in trial data: 1176

flanker_raw2 <- flanker_raw |> 
  # Now that IDs have been fixed, run another distinct() to remove duplicates
  distinct() |> 
  # Filter subjects that do not have a match in the summary score data
  filter(subj_idx %in% nih_ids) |>
  mutate(InstrumentStartedTimestamp = str_remove_all(InstrumentStartedTimestamp, "\\s.*")) |> 
  left_join(
    nih_tb_sum |> 
      select(src_subject_id, eventname, interview_date) |> 
      rename(subj_idx = src_subject_id, InstrumentStartedTimestamp = interview_date) |> 
      mutate(InstrumentStartedTimestamp = str_replace_all(InstrumentStartedTimestamp, "([0-9]+)/([0-9]+)/([0-9]+)", "\\3-\\1-\\2"))
      )
  # For subjects who have more than 1 assessment date, pick the earlier date
  filter_baseline(data = _) |> 
  # Filter assessments with an assessment in 2019, 2020 or 2021, as these were not baseline measures.
  filter(!str_detect(InstrumentStartedTimestamp, "^(2019|2020|2021)")) |> 
  group_by(subj_idx) |> 
  mutate(n=n())
  

flanker_raw3 |> 
  group_by(subj_idx) |> 
  mutate(n = n()) |> 
  filter(n>20) |> 
  filter(!str_detect(InstrumentStartedTimestamp, "^(2019|2020|2021)")) |> 
  View()




### 2.2.2 Processing Speed Task ----

pcps_raw <- 
  bind_rows(
    nih_p1_parsed |> 
      filter(str_detect(InstrumentTitle, "Pattern Comparison Processing Speed")) |> 
      filter(Key %in% c("Response", "Score", "ResponseTime", "Position", "InstrumentStartedTimestamp", "InstrumentEndedTimestamp")) |>
      distinct() |> # Remove duplicate rows
      pivot_wider(names_from = "Key", values_from = "Value") |> 
      select(PIN, ItemID, Response, ResponseTime, Score, Position) |> 
      rename(
        subj_idx = PIN,
        RT       = ResponseTime,
        correct  = Score
      ),
    nih_p2_parsed |> 
      filter(str_detect(Inst, "Pattern Comparison Processing Speed")) |> 
      select(PIN, ItemID, Response, Score, Position, ResponseTime) |> 
      rename(
        subj_idx = PIN,
        RT       = ResponseTime,
        correct  = Score
      )
  ) |> 
  mutate(across(c(Response, RT, correct, Position), as.numeric)) |> 
  filter(!ItemID %in% c("PSPAC_INSTR", "PSPAC_INTRO_INSTR", "PSPACPA[1-6]")) |> 
  fix_subject_ids(data = _) |> 
  distinct()


### 2.2.3 Dimensional Card Sorting Task ----

dccs_raw <- 
  bind_rows(
    nih_p1_parsed |> 
      filter(str_detect(InstrumentTitle, "Dimensional Change")) |> 
      filter(Key %in% c("Response", "Score", "ResponseTime", "Position", "InstrumentStartedTimestamp", "InstrumentEndedTimestamp")) |>
      distinct() |> # Remove duplicate rows
      pivot_wider(names_from = "Key", values_from = "Value") |> 
      select(PIN, ItemID, Response, ResponseTime, Score, Position) |> 
      rename(
        subj_idx = PIN,
        RT       = ResponseTime,
        correct  = Score
      ),
    nih_p2_parsed |> 
      filter(str_detect(Inst, "Dimensional Change")) |> 
      select(PIN, ItemID, Response, Score, Position, ResponseTime) |> 
      rename(
        subj_idx = PIN,
        RT       = ResponseTime,
        correct  = Score
      )
  ) |> 
  mutate(across(c(Response, RT, correct, Position), as.numeric)) |> 
  filter(!str_detect(ItemID, "PRAC")) |> 
  fix_subject_ids(data = _) |> 
  distinct()

### 2.2.4 Picture Vocabulary Task ----
picvoc_raw <- 
  bind_rows(
    nih_p1_parsed |> 
      filter(str_detect(InstrumentTitle, "Picture Vocabulary")) |> 
      filter(Key %in% c("Response", "Score", "ResponseTime", "Position", "InstrumentStartedTimestamp", "InstrumentEndedTimestamp")) |>
      distinct() |> # Remove duplicate rows
      pivot_wider(names_from = "Key", values_from = "Value") |> 
      select(PIN, ItemID, Response, ResponseTime, Score, Position) |> 
      rename(
        subj_idx = PIN,
        RT       = ResponseTime,
        correct  = Score
      ),
    nih_p2_parsed |> 
      filter(str_detect(Inst, "Picture Vocabulary")) |> 
      select(PIN, ItemID, Response, Score, Position, ResponseTime) |> 
      rename(
        subj_idx = PIN,
        RT       = ResponseTime,
        correct  = Score
      )
  ) |> 
  mutate(across(c(Response, RT, correct, Position), as.numeric)) |> 
  filter(str_detect(ItemID, "LAVOC")) |> 
  fix_subject_ids(data = _) |> 
  distinct()

# TODO: Add datachecks on number of trials, after filtering non-participants
# TODO: Cross-check ids with summary data and remove non-participants
# TODO: Check why some valid ids have more trials than they should (what about the time stamps?)

## 2.3 Clean data ----

### 2.3.1 Flanker Task ----

flanker_ids <- flanker_raw |> pull(subj_idx) |> unique()

flanker_ids[!flanker_ids %in% nih_ids]
nih_ids[!nih_ids %in% flanker_ids]

flanker_clean <- flanker_raw |> 
  mutate(
    ex_missing_RT   = ifelse(is.na(RT), TRUE, FALSE),
    ex_missing_acc  = ifelse(is.na(correct), TRUE, FALSE),
    ex_fast_RT      = ifelse(RT < 0.3, TRUE, FALSE)
  ) |> 
  group_by(subj_idx, congruency) |> 
  mutate(ex_slow_RT = ifelse(scale(log(RT)) |> as.numeric() > 3, TRUE, FALSE)) |> 
  ungroup()
  
  


### 2.3.2 Processing Speed Task ----

pcps_raw |> 
  group_by(subj_idx) |> 
  count() |> View()

### 2.3.3 Dimensional Card Sorting Task ----

dccs_raw |> 
  group_by(subj_idx) |> 
  count() |> View()

### 2.3.4 Picture Vocabulary Task ----

picvoc_raw |> 
  group_by(subj_idx) |> 
  count() |> View()
