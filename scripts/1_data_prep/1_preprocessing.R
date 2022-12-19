# Read NIH summary scores for participant ID verification
tryCatch(
  {
  nih_ref_ids <- read_delim('data/abcd_tbss01.txt') |> 
    filter(str_detect(eventname, 'baseline')) |> 
    filter(collection_id != 'collection_id') |> 
    distinct(subjectkey) |> 
    pull(subjectkey)
  },
  error=function(cond){ 
    message("Error: File not found. If you do not have access to the ABCD data, you can skip the scripts under `1_data_prep` and continue to the scripts under `2_analyses` using the synthetic data that are provided in the `data` folder. If you *do* have access to the ABCD data, make sure that you place the required data in the `data` folder. See the README file for more information.")
    }
)

exclusions <- list()
descriptives <- list()

# 1. Little Man Task ------------------------------------------------------

## 1.1 File Preparations ----

# Only keep baseline measures
list.files(file.path("data/abcd_lmtlb01"), pattern = "2_year_follow_up", full.names = TRUE) |> 
  map(function(x) file.remove(x))

# Extract baseline measures
list.files(file.path("data/abcd_lmtlb01"), full.names = TRUE) |> 
  map(function(x) utils::unzip(x, exdir = data_folder))


## 1.2 Read files ----

tryCatch(
  {
    lmt_raw <- read_csv(file = "list.files('data/LMT/baseline_year_1_arm_1', full.names = TRUE)") 
  },
  error=function(cond){ 
    message("Error: File not found. If you do not have access to the ABCD data, you can skip the scripts under `1_data_prep` and continue to the scripts under `2_analyses` using the synthetic data that are provided in the `data` folder. If you *do* have access to the ABCD data, make sure that you place the required data in the `data` folder. See the README file for more information.")
  }
)

lmt_raw <- lmt_raw |> 
  map(function(x) {
    x |> mutate(lmt_response = as.character(lmt_response))
  }) |> 
  reduce(bind_rows) |> 
  filter(lmt_blockcode == "test" | str_detect(lmt_values_stim, "Sorry"), !lmt_trialcode %in% c('instructions', 'homeBase')) |> 
  rename(
    subj_idx = subject,
    response = lmt_correct,
    RT       = lmt_latency
  ) |> 
  mutate(RT = RT / 1000) |> 
  # If RTs are missing because the response > 10 s, retain the missing value
  mutate(
    RT = ifelse(RT == -0.001 & str_detect(lmt_values_stim, ".png") & str_detect(lead(lmt_values_stim, 1), "Sorry"), 999, RT)
  ) |> 
  filter(RT > 0) |> 
  mutate(RT = ifelse(RT == 999, NA, RT))

# data check: do all participants have 32 trials?
assertthat::assert_that(all(lmt_raw |> group_by(subj_idx) |> count() |> pull() == 32))


# 2. NIH Toolbox tasks -------------------------------------------------------

## 2.1 File preparations ----

# Fix file names
list.files("data/abcd_tb_tlb01/NIHTB/NIHTB", full.names = TRUE) |> 
  walk(function(x){
    
    new_name <- x |> 
      str_remove(pattern = ".csv") |> 
      str_c(".csv") |> 
      str_replace_all("\\s", "_")

    
    file.rename(from = x, to = new_name)
  })


## 2.2 Read files ----

# There are three file types with different data structures that have to be read in separately.

tryCatch(
  {
    nih_p1 <- read_csv(file = "list.files('data/abcd_tb_tlb01/NIHTB/NIHTB'), full.names = TRUE) |> str_subset(pattern = 'Narrow_Structure')")
    nih_p2 <- read_csv(file = "list.files('data/abcd_tb_tlb01/NIHTB/NIHTB'), full.names = TRUE) |> str_subset(pattern = '[0-9]_Assessment')")
    nih_p3 <- read_csv(file = "list.files('data/abcd_tb_tlb01/NIHTB/NIHTB'), full.names = TRUE) |> str_subset(pattern = 'Registration')")
  },
  error=function(cond){ 
    message("Error: File not found. If you do not have access to the ABCD data, you can skip the scripts under `1_data_prep` and continue to the scripts under `2_analyses` using the synthetic data that are provided in the `data` folder. If you *do* have access to the ABCD data, make sure that you place the required data in the `data` folder. See the README file for more information.")
  }
)


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


# Collection of data manipulations to fix typos in subject IDs
flanker_raw <- flanker_raw |> 
  fix_subject_ids(data = _) |> # Get all subject IDs in the correct NDAR_INVXXXXX format
  fix_id_typos(data = _, reference = nih_ref_ids) # Calculate Levenshtein distances between ids in this trial-level dataset compared to the summary score dataset in order to detect and fix typos.


flanker_raw <- flanker_raw |> 
  # Filter subjects that do not have a match in the summary score data
  filter(subj_idx %in% nih_ref_ids) |>
  # Now that IDs have been fixed, remove duplicate rows from the data 
  distinct() |> 
  # For subjects who have more than 1 assessment date, pick the earlier date
  filter_baseline(data = _) |> 
  # Exclude participants who have 2 assessments on the same day.
  group_by(subj_idx) |> 
  mutate(n = n()) |> 
  filter(n == 20) |> 
  select(-n)


assertthat::assert_that(all((flanker_raw |> group_by(subj_idx) |> count() |> pull(n)) == 20))

glue("Pre-cleaning Flanker sample size: {flanker_raw |> pull(subj_idx) |> unique() |> length()} participants.")


### 2.2.2 Processing Speed Task ----

pcps_raw <- 
  bind_rows(
    nih_p1_parsed |> 
      filter(str_detect(InstrumentTitle, "Pattern Comparison Processing Speed")) |> 
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
      filter(str_detect(Inst, "Pattern Comparison Processing Speed")) |> 
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
  filter(str_detect(ItemID, "INSTR|TRANSITION|PSPACPA[1-6]", negate = T)) 

# Collection of data manipulations to fix typos in subject IDs
pcps_raw <- pcps_raw |> 
  fix_subject_ids(data = _) |> # Get all subject IDs in the correct NDAR_INVXXXXX format
  fix_id_typos(data = _, reference = nih_ref_ids) # Calculate Levenshtein distances between ids in this trial-level dataset compared to the summary score dataset in order to detect and fix typos.


pcps_raw <- pcps_raw |> 
  # Filter subjects that do not have a match in the summary score data
  filter(subj_idx %in% nih_ref_ids) |>
  # Now that IDs have been fixed, remove duplicate rows from the data 
  distinct() |> 
  # For subjects who have more than 1 assessment date, pick the earlier date
  filter_baseline(data = _) |> 
  # Exclude participants who have 2 assessments on the same day.
  group_by(subj_idx) |> 
  mutate(n = unique(InstrumentStartedTimestamp) |> length()) |> 
  filter(n == 1) |> 
  select(-n)

glue("Pre-cleaning PCPS sample size: {pcps_raw |> pull(subj_idx) |> unique() |> length()} participants.")


### 2.2.3 Dimensional Card Sorting Task ----

dccs_raw <- 
  bind_rows(
    nih_p1_parsed |> 
      filter(str_detect(InstrumentTitle, "Dimensional Change")) |> 
      filter(Key %in% c("Response", "Score", "ResponseTime", "AssessmentName", "Position", "InstrumentStartedTimestamp", "InstrumentEndedTimestamp")) |>
      distinct() |> # Remove duplicate rows
      pivot_wider(names_from = "Key", values_from = "Value") |> 
      select(PIN, ItemID, Response, ResponseTime, Score, Position, AssessmentName, InstrumentStartedTimestamp, InstrumentEndedTimestamp) |> 
      rename(
        subj_idx = PIN,
        RT       = ResponseTime,
        correct  = Score
      ),
    nih_p2_parsed |> 
      filter(str_detect(Inst, "Dimensional Change")) |> 
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
  filter(!str_detect(ItemID, "PRAC"), str_detect(ItemID, "DCCSMIXED")) |> 
  mutate(ItemID = ifelse(str_detect(ItemID, "REPEAT"), 'repeat', 'switch'))


# Collection of data manipulations to fix typos in subject IDs
dccs_raw <- dccs_raw |> 
  fix_subject_ids(data = _) |> # Get all subject IDs in the correct NDAR_INVXXXXX format
  fix_id_typos(data = _, reference = nih_ref_ids) # Calculate Levenshtein distances between ids in this trial-level dataset compared to the summary score dataset in order to detect and fix typos.


dccs_raw <- dccs_raw |> 
  # Filter subjects that do not have a match in the summary score data
  filter(subj_idx %in% nih_ref_ids) |>
  # Now that IDs have been fixed, remove duplicate rows from the data 
  distinct() |> 
  # For subjects who have more than 1 assessment date, pick the earlier date
  filter_baseline(data = _) |> 
# Exclude participants who have 2 assessments on the same day.
  group_by(subj_idx) |> 
  mutate(n = unique(InstrumentStartedTimestamp) |> length()) |> 
  filter(n == 1) |> 
  select(-n)

glue("Pre-cleaning DCCS sample size: {dccs_raw |> pull(subj_idx) |> unique() |> length()} participants.")


### 2.2.4 Picture Vocabulary Task ----
picvoc_raw <- 
  bind_rows(
    nih_p1_parsed |> 
      filter(str_detect(InstrumentTitle, "Picture Vocabulary")) |> 
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
      filter(str_detect(Inst, "Picture Vocabulary")) |> 
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
  filter(str_detect(ItemID, "LAVOC")) 




# Collection of data manipulations to fix typos in subject IDs
picvoc_raw <- picvoc_raw |> 
  fix_subject_ids(data = _) |> # Get all subject IDs in the correct NDAR_INVXXXXX format
  fix_id_typos(data = _, reference = nih_ref_ids) # Calculate Levenshtein distances between ids in this trial-level dataset compared to the summary score dataset in order to detect and fix typos.


picvoc_raw <- picvoc_raw |> 
  # Filter subjects that do not have a match in the summary score data
  filter(subj_idx %in% nih_ref_ids) |>
  # Now that IDs have been fixed, remove duplicate rows from the data 
  distinct() |> 
  # For subjects who have more than 1 assessment date, pick the earlier date
  filter_baseline(data = _) |> 
  # Exclude participants who have 2 assessments on the same day.
  group_by(subj_idx) |> 
  mutate(n = unique(InstrumentStartedTimestamp) |> length()) |> 
  filter(n == 1) |> 
  select(-n)

glue("Pre-cleaning PIC VOC sample size: {picvoc_raw |> pull(subj_idx) |> unique() |> length()} participants.")


# Remove subjects from lmt that do not have NIH TB data because of technical issues.
lmt_raw <- lmt_raw |> 
  filter(
    subj_idx %in% unique(
      c(
        flanker_raw |> pull(subj_idx) |> unique(),
        dccs_raw |> pull(subj_idx) |> unique(),
        pcps_raw |> pull(subj_idx) |> unique()
      )
    )
  )

save(lmt_raw, flanker_raw, dccs_raw, pcps_raw, nih_ref_ids, file = glue('data/tasks_raw.RData'))
