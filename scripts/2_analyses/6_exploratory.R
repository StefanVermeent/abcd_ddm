

# Load data ---------------------------------------------------------------
nih_data <- read_delim("data/abcd_tbss01.txt")
test_set <- read_csv("data/test_set.csv")
iv_data <- read_csv("data/iv_data.csv")
lmt_clean <- read_csv("data/lmt_clean.csv")

ddm_data      <- read_csv('data/ddm_data.csv')
test_set      <- read_csv("data/test_set.csv")

load('analysis_objects/results_sem_test.RData')

test_data <- test_set |> 
  left_join(ddm_data) |> 
  left_join(iv_data) %>% 
  mutate(across(
    c(ends_with("_a"), ends_with("_v"), ends_with("_t"), dep_mnlfa, threat_mnlfa, inr_c, high_edu_c, age_c),
    ~scale(x = .) |> as.numeric(x = .)
  ))


# 2. Prepare data ---------------------------------------------------------

# Calculate the LMT traditional scores
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

# Combine data
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
  mutate(across(ends_with("raw"), as.numeric)) |> 
  mutate(across(
    c(ends_with("_raw"), dep_mnlfa, threat_mnlfa, inr_c, high_edu_c, age_c),
    ~scale(x = .) |> as.numeric(x = .)
  )) |> 
  filter(subj_idx %in% unique(test_data$subj_idx))
  



# Fit SEM model -----------------------------------------------------------

# Some children are clustered within families.
# We account for this using the lavaan.survey package

cluster_design <- survey::svydesign(ids=~rel_family_id, prob=~1, data = raw_scores) 

model_raw <- '
# MEASUREMENT MODEL
  # General latent factors
  general_raw         =~ 1*pcps_raw + dccs_raw + flanker_raw + lmt_raw

  # Task-specific variance
  flanker_raw_l       =~ 1*flanker_raw
  dccs_raw_l          =~ 1*dccs_raw
  lmt_raw_l           =~ 1*lmt_raw
  pcps_raw_l          =~ 1*pcps_raw
  
  flanker_raw         ~~ 0*flanker_raw
  dccs_raw            ~~ 0*dccs_raw
  lmt_raw             ~~ 0*lmt_raw
  pcps_raw            ~~ 0*pcps_raw
  
  dep_mnlfa           ~~ threat_mnlfa + age_c + inr_c + high_edu_c + eth_black + eth_hisp + eth_other + sex
  threat_mnlfa        ~~ age_c + inr_c + high_edu_c + eth_black + eth_hisp + eth_other + sex
  age_c               ~~ inr_c + high_edu_c + eth_black + eth_hisp + eth_other + sex
  inr_c               ~~ high_edu_c + eth_black + eth_hisp + eth_other + sex
  high_edu_c          ~~ eth_black + eth_hisp + eth_other + sex
  eth_black           ~~ eth_hisp + eth_other + sex
  eth_hisp            ~~ eth_other + sex
  eth_other           ~~ sex
 
  # STRUCTURAL MODEL
  
  # Task-general association
  general_raw         ~ dep_mnlfa + threat_mnlfa + age_c + inr_c + high_edu_c + eth_black + eth_hisp + eth_other + sex
  
  flanker_raw_l       ~ dep_mnlfa + threat_mnlfa + age_c + inr_c + high_edu_c + eth_black + eth_hisp + eth_other + sex
  dccs_raw_l          ~ dep_mnlfa + threat_mnlfa + age_c + inr_c + high_edu_c + eth_black + eth_hisp + eth_other + sex
  lmt_raw_l           ~ dep_mnlfa + threat_mnlfa + age_c + inr_c + high_edu_c + eth_black + eth_hisp + eth_other + sex
'

test_sem_full_expl <- 
  sem(model = model_raw, 
      data = raw_scores, 
      missing = 'ML', 
      orthogonal = TRUE, 
      auto.cov.lv.x = FALSE,
      auto.cov.y = FALSE, 
      std.lv = F)

test_sem_full_cluster_expl <- lavaan.survey::lavaan.survey(lavaan.fit = test_sem_full_expl, survey.design = cluster_design, estimator = "ML")

summary(test_sem_full_cluster_expl, fit.measures = TRUE, standardized = TRUE)

test_reg_coef_expl <- standardizedsolution(test_sem_full_cluster_expl) |> 
  as_tibble() |> 
  filter(op == "~", rhs %in% c("threat_mnlfa", "dep_mnlfa")) 

# Equivalence tests -------------------------------------------------------

equivalence_test_ml = function(sesoi, ml_effect, ml_se){
  p1 = 1 - pnorm( ml_effect, mean = -sesoi, sd = ml_se)                  
  p2 =     pnorm( ml_effect, mean =  sesoi, sd = ml_se)
  return(max(p1, p2))
}

equivalence_tests_expl <- standardizedSolution(test_sem_full_cluster_expl) |> 
  as_tibble() |> 
  filter(op == "~", rhs %in% c("dep_mnlfa", "threat_mnlfa")) |> 
  select(lhs, rhs, est.std, se) |> 
  mutate(
    eq_pvalue = pmap(list(lhs, rhs, est.std, se), function(lhs, rhs, est.std, se) {
      equivalence_test_ml(0.1, est.std, se)
    })
  ) |>
  unnest(eq_pvalue)

save(test_sem_full_cluster_expl, test_reg_coef_expl, equivalence_tests_expl, file = "analysis_objects/exploratory_results.RData")
