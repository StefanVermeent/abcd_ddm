
# Load data ---------------------------------------------------------------
nih_data <- read_delim("data/abcd_tbss01.txt")
test_set <- read_csv("data/test_set.csv")
iv_data <- read_csv("data/iv_data.csv")
lmt_clean <- read_csv("data/lmt_clean.csv")



# Prepare data ------------------------------------------------------------

lmt_sum <- lmt_clean |> 
  select(subj_idx, RT, correct) |> 
  group_by(subj_idx, correct) |> 
  mutate(mean_RT_correct = mean(RT, na.rm = T)) |> 
  group_by(subj_idx) |> 
  mutate(
    per_correct = sum(correct) / n() * 100,
  ) |> 
  ungroup() |> 
  filter(correct == 1) |> 
  select(-RT) |> 
  distinct() |> 
  mutate(lmt_raw = per_correct / mean_RT_correct, .keep = 'unused') |> 
  ungroup()

raw_scores <- nih_data |> 
  filter(str_detect(eventname, "baseline")) |> 
  select(
    subj_idx    = src_subject_id, subjectkey,
    flanker_raw = nihtbx_flanker_uncorrected ,
    dccs_raw    = nihtbx_cardsort_uncorrected,
    pcps_raw    = nihtbx_pattern_uncorrected
  ) |> 
  right_join(test_set) |> 
  left_join(iv_data) |> 
  left_join(lmt_sum) |> 
  mutate(across(ends_with("raw"), as.numeric))

ggplot(data = raw_scores, aes(flanker_raw)) + geom_histogram()
ggplot(data = raw_scores, aes(dccs_raw)) + geom_histogram()
ggplot(data = raw_scores, aes(lmt_raw)) + geom_histogram()
ggplot(data = raw_scores, aes(pcps_raw)) + geom_histogram()


# Exploratory analyses ----------------------------------------------------

raw_fit_flanker <- lm(data = raw_scores, flanker_raw ~ threat_mnlfa + dep_mnlfa + age_c + high_edu_c + eth_black + eth_hisp + eth_other + inr_c)
raw_fit_flanker_std <- parameters::standardise_parameters(raw_fit_flanker) |> 
  as_tibble() |> 
  left_join(raw_fit_flanker |> broom::tidy() |> rename(Parameter = term))

raw_fit_dccs <- lm(data = raw_scores, dccs_raw ~ threat_mnlfa + dep_mnlfa + age_c + high_edu_c + eth_black + eth_hisp + eth_other + inr_c)
raw_fit_dccs_std <- parameters::standardise_parameters(raw_fit_dccs) |> 
  as_tibble() |> 
  left_join(raw_fit_dccs |> broom::tidy() |> rename(Parameter = term))

raw_fit_lmt <- lm(data = raw_scores, lmt_raw ~ threat_mnlfa + dep_mnlfa + age_c + high_edu_c + eth_black + eth_hisp + eth_other + inr_c)
raw_fit_lmt_std <- parameters::standardise_parameters(raw_fit_lmt) |> 
  as_tibble() |> 
  left_join(raw_fit_lmt |> broom::tidy() |> rename(Parameter = term))

raw_fit_pcps <- lm(data = raw_scores, pcps_raw ~ threat_mnlfa + dep_mnlfa + age_c + high_edu_c + eth_black + eth_hisp + eth_other + inr_c)
raw_fit_pcps_std <- parameters::standardise_parameters(raw_fit_pcps) |> 
  as_tibble() |> 
  left_join(raw_fit_pcps |> broom::tidy() |> rename(Parameter = term))



# Save results ------------------------------------------------------------

save(raw_fit_flanker_std, raw_fit_dccs_std, raw_fit_lmt_std, raw_fit_pcps_std, file = "analysis_objects/exploratory_analysis.RData")


