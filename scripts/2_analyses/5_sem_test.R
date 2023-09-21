ddm_data      <- readr::read_csv('data/ddm_data.csv')
iv_data       <- readr::read_csv(paste0("data/iv_data", data_suffix, ".csv"))
test_set      <- readr::read_csv(paste0("data/test_set", data_suffix, ".csv"))

test_data <- test_set |> 
  left_join(ddm_data) |> 
  left_join(iv_data) %>% 
  mutate(across(
    c(ends_with("_a"), ends_with("_v"), ends_with("_t"), dep_mnlfa, threat_mnlfa, inr_c, high_edu_c, age_c),
    ~scale(x = .) |> as.numeric(x = .)
  ))


# Specify Clustering Design -----------------------------------------------

# Some children are clustered within families.
# We account for this using the lavaan.survey package

cluster_design <- survey::svydesign(ids=~rel_family_id, prob=~1, data = test_data) 




# Model Fit ---------------------------------------------------------------

model_full <- '
# MEASUREMENT MODEL
  # General latent factors
  v_general             =~ 1*pcps_v + dccs_v + rotation_v + flanker_v  
  a_general             =~ 1*pcps_a + dccs_a + rotation_a + flanker_a 
  t_general             =~ 1*pcps_t + dccs_t + rotation_t + flanker_t
  
  
  # Task-specific variance
  flanker_v_l           =~ 1*flanker_v
  dccs_v_l              =~ 1*dccs_v
  rotation_v_l          =~ 1*rotation_v
  pcps_v_l              =~ 1*pcps_v
  
  flanker_a_l           =~ 1*flanker_a
  dccs_a_l              =~ 1*dccs_a
  rotation_a_l          =~ 1*rotation_a
  pcps_a_l              =~ 1*pcps_a
  
  flanker_t_l           =~ 1*flanker_t
  dccs_t_l              =~ 1*dccs_t
  rotation_t_l          =~ 1*rotation_t
  pcps_t_l              =~ 1*pcps_t
  
  flanker_v             ~~ 0*flanker_v
  flanker_a             ~~ 0*flanker_a
  flanker_t             ~~ 0*flanker_t
  dccs_v                ~~ 0*dccs_v
  dccs_a                ~~ 0*dccs_a
  dccs_t                ~~ 0*dccs_t
  rotation_v            ~~ 0*rotation_v
  rotation_a            ~~ 0*rotation_a
  rotation_t            ~~ 0*rotation_t
  pcps_v                ~~ 0*pcps_v
  pcps_a                ~~ 0*pcps_a
  pcps_t                ~~ 0*pcps_t
  

  # Only covariances between general latent factors and residual variances of the same task.
  v_general             ~~ a_general + t_general
  a_general             ~~ t_general
  
  flanker_v_l           ~~ flanker_a_l + flanker_t_l
  flanker_a_l           ~~ flanker_t_l 
  
  dccs_v_l              ~~ dccs_a_l + dccs_t_l 
  dccs_a_l              ~~ dccs_t_l 
  
  rotation_v_l          ~~ rotation_a_l + rotation_t_l 
  rotation_a_l          ~~ rotation_t_l 
  
  pcps_v_l              ~~ pcps_a_l + pcps_t_l
  pcps_a_l              ~~ pcps_t_l
  
  dep_mnlfa             ~~ threat_mnlfa + age_c + inr_c + high_edu_c + eth_black + eth_hisp + eth_other + sex
  threat_mnlfa          ~~ age_c + inr_c + high_edu_c + eth_black + eth_hisp + eth_other + sex
  age_c                 ~~ inr_c + high_edu_c + eth_black + eth_hisp + eth_other + sex
  inr_c                 ~~ high_edu_c + eth_black + eth_hisp + eth_other + sex
  high_edu_c            ~~ eth_black + eth_hisp + eth_other + sex
  eth_black             ~~ eth_hisp + eth_other + sex
  eth_hisp              ~~ eth_other + sex
  eth_other             ~~ sex
 

  # STRUCTURAL MODEL
  
  # Task-general association
  v_general             ~ dep_mnlfa + threat_mnlfa + age_c + inr_c + high_edu_c + eth_black + eth_hisp + eth_other + sex
  a_general             ~ dep_mnlfa + threat_mnlfa + age_c + inr_c + high_edu_c + eth_black + eth_hisp + eth_other + sex
  t_general             ~ dep_mnlfa + threat_mnlfa + age_c + inr_c + high_edu_c + eth_black + eth_hisp + eth_other + sex
  
  flanker_v_l           ~ dep_mnlfa + threat_mnlfa + age_c + inr_c + high_edu_c + eth_black + eth_hisp + eth_other + sex
  flanker_a_l           ~ dep_mnlfa + threat_mnlfa + age_c + inr_c + high_edu_c + eth_black + eth_hisp + eth_other + sex
  flanker_t_l           ~ dep_mnlfa + threat_mnlfa + age_c + inr_c + high_edu_c + eth_black + eth_hisp + eth_other + sex
  
  dccs_v_l              ~ dep_mnlfa + threat_mnlfa + age_c + inr_c + high_edu_c + eth_black + eth_hisp + eth_other + sex
  dccs_a_l              ~ dep_mnlfa + threat_mnlfa + age_c + inr_c + high_edu_c + eth_black + eth_hisp + eth_other + sex
  dccs_t_l              ~ dep_mnlfa + threat_mnlfa + age_c + inr_c + high_edu_c + eth_black + eth_hisp + eth_other + sex
  
  rotation_v_l          ~ dep_mnlfa + threat_mnlfa + age_c + inr_c + high_edu_c + eth_black + eth_hisp + eth_other + sex
  rotation_a_l          ~ dep_mnlfa + threat_mnlfa + age_c + inr_c + high_edu_c + eth_black + eth_hisp + eth_other + sex
  rotation_t_l          ~ dep_mnlfa + threat_mnlfa + age_c + inr_c + high_edu_c + eth_black + eth_hisp + eth_other + sex
'

test_sem_full <- 
  sem(model = model_full, 
      data = test_data, 
      missing = 'ML', 
      orthogonal = TRUE, 
      auto.cov.lv.x = FALSE,
      auto.cov.y = FALSE, 
      std.lv = F)

test_sem_full_cluster <- lavaan.survey::lavaan.survey(lavaan.fit = test_sem_full, survey.design = cluster_design, estimator = "ML")

summary(test_sem_full_cluster, fit.measures = TRUE, standardized = TRUE)

# Extract model results ---------------------------------------------------

test_factor_loadings <- parameterEstimates(test_sem_full_cluster, standardized = T) |> 
  filter(op == "=~", str_detect(lhs, "general"))

test_reg_coef <- standardizedsolution(test_sem_full_cluster) |> 
  as_tibble() |> 
  filter(op == "~", rhs %in% c("threat_mnlfa", "dep_mnlfa")) |> 
  mutate(
    ddm_parameter = case_when(
      lhs == "v_general" | str_detect(lhs, "_v_") ~ "Drift rate",
      lhs == "a_general" | str_detect(lhs, "_a_") ~ "Boundary separation",
      lhs == "t_general" | str_detect(lhs, "_t_") ~ "Non-decision time",
    )
  ) |> 
  group_by(ddm_parameter) |> 
  mutate(pvalue_adj = p.adjust(pvalue, method = "fdr")) |> 
  ungroup()



# Equivalence tests -------------------------------------------------------

equivalence_test_ml = function(sesoi, ml_effect, ml_se){
  p1 = 1 - pnorm( ml_effect, mean = -sesoi, sd = ml_se)                  
  p2 =     pnorm( ml_effect, mean =  sesoi, sd = ml_se)
  return(max(p1, p2))
}

equivalence_tests <- standardizedSolution(test_sem_full_cluster) |> 
  as_tibble() |> 
  filter(op == "~", rhs %in% c("dep_mnlfa", "threat_mnlfa")) |> 
  select(lhs, rhs, est.std, se) |> 
  mutate(
    eq_pvalue = pmap(list(lhs, rhs, est.std, se), function(lhs, rhs, est.std, se) {
      equivalence_test_ml(0.1, est.std, se)
    })
  ) |>
  unnest(eq_pvalue)

save(test_sem_full_cluster, test_factor_loadings, test_reg_coef, equivalence_tests, file = "analysis_objects/results_sem_test.RData")
