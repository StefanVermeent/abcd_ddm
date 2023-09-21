ddm_data      <- readr::read_csv('data/ddm_data.csv')
iv_data       <- readr::read_csv(paste0("data/iv_data", data_suffix, ".csv"))
training_set  <- readr::read_csv(paste0("data/training_set", data_suffix, ".csv"))

training_data <- training_set |> 
  left_join(ddm_data) |> 
  left_join(iv_data) %>% 
  mutate(across(
    c(ends_with("_a"), ends_with("_v"), ends_with("_t"), dep_mnlfa, threat_mnlfa, inr_c, high_edu_c, age_c),
    ~scale(x = .) |> as.numeric(x = .)
  ))



# Specify Clustering Design -----------------------------------------------

# Some children are clustered within families.
# We account for this using the lavaan.survey package

cluster_design <- survey::svydesign(ids=~rel_family_id, prob=~1, data = training_data) 



# 1. Measurement Model 1: Drift Rate --------------------------------------

model_sub_v <- '
  # MEASUREMENT MODEL
  ## General factor
  v_general        =~ 1*pcps_v + dccs_v + rotation_v + flanker_v
  
  # Task-specific variance
  flanker_v_l      =~ 1*flanker_v
  dccs_v_l         =~ 1*dccs_v
  rotation_v_l     =~ 1*rotation_v
  pcps_v_l         =~ 1*pcps_v
  
  # Covariance structure among unique variances
  flanker_v_l      ~~ 0*dccs_v_l + 0*rotation_v_l + 0*pcps_v_l
  dccs_v_l         ~~ 0*rotation_v_l + 0*pcps_v_l
  rotation_v_l     ~~ 0*pcps_v_l
  
  # No covariances between manifest variables
  flanker_v        ~~  0*flanker_v + 0*dccs_v + 0*rotation_v + 0*pcps_v
  dccs_v           ~~ 0*dccs_v + 0*rotation_v + 0*pcps_v
  rotation_v       ~~ 0*rotation_v + 0*pcps_v
  pcps_v           ~~ 0*pcps_v
  
  v_general        ~~ 0*flanker_v_l + 0*dccs_v_l + 0*rotation_v_l + 0*pcps_v_l
'

training_sem_sub_v <- sem(model = model_sub_v, data = training_data, missing = "ML")
training_sem_sub_v_cluster <- lavaan.survey::lavaan.survey(lavaan.fit = training_sem_sub_v, survey.design = cluster_design, estimator = "ML")

summary(training_sem_sub_v_cluster, fit.measures = TRUE, standardized = TRUE)

# 2. Measurement Model 2: Boundary separation --------------------------

model_sub_a <- '
  # MEASUREMENT MODEL
  ## General factor
  a_general        =~ 1*pcps_a + dccs_a + rotation_a + flanker_a
  
  # Task-specific variance
  flanker_a_l      =~ 1*flanker_a
  dccs_a_l         =~ 1*dccs_a
  rotation_a_l     =~ 1*rotation_a
  pcps_a_l         =~ 1*pcps_a
  
  # Covariance structure among unique variances
  flanker_a_l      ~~ 0*dccs_a_l + 0*rotation_a_l + 0*pcps_a_l
  dccs_a_l         ~~ 0*rotation_a_l + 0*pcps_a_l
  rotation_a_l     ~~ 0*pcps_a_l
  
  # No covariances between manifest variables
  flanker_a        ~~  0*flanker_a + 0*dccs_a + 0*rotation_a + 0*pcps_a
  dccs_a           ~~ 0*dccs_a + 0*rotation_a + 0*pcps_a
  rotation_a       ~~ 0*rotation_a + 0*pcps_a
  pcps_a           ~~ 0*pcps_a
  
  a_general        ~~ 0*flanker_a_l + 0*dccs_a_l + 0*rotation_a_l + 0*pcps_a_l
'

training_sem_sub_a <- sem(model = model_sub_a, data = training_data, missing = 'ML')
training_sem_sub_a_cluster <- lavaan.survey::lavaan.survey(lavaan.fit = training_sem_sub_a, survey.design = cluster_design, estimator = "ML")

summary(training_sem_sub_a_cluster, fit.measures = TRUE, standardized = TRUE)

# 3. Measurement Model 3: Non-Decision Time -------------------------------

model_sub_t <- '
  # MEASUREMENT MODEL
  ## General factor
  t_general        =~ 1*pcps_t + dccs_t + rotation_t + flanker_t
  
  # Task-specific variance
  flanker_t_l      =~ 1*flanker_t
  dccs_t_l         =~ 1*dccs_t
  rotation_t_l     =~ 1*rotation_t
  pcps_t_l         =~ 1*pcps_t
  
  # Covariance structure among unique variances
  flanker_t_l      ~~ 0*dccs_t_l + 0*rotation_t_l + 0*pcps_t_l
  dccs_t_l         ~~ 0*rotation_t_l + 0*pcps_t_l
  rotation_t_l     ~~ 0*pcps_t_l
  
  # No covariances between manifest variables
  flanker_t        ~~  0*flanker_t + 0*dccs_t + 0*rotation_t + 0*pcps_t
  dccs_t           ~~ 0*dccs_t + 0*rotation_t + 0*pcps_t
  rotation_t       ~~ 0*rotation_t + 0*pcps_t
  pcps_t           ~~ 0*pcps_t
  
  t_general        ~~ 0*flanker_t_l + 0*dccs_t_l + 0*rotation_t_l + 0*pcps_t_l
'

training_sem_sub_t <- sem(model = model_sub_t, data = training_data, missing = 'ML')
training_sem_sub_t_cluster <- lavaan.survey::lavaan.survey(lavaan.fit = training_sem_sub_t, survey.design = cluster_design, estimator = "ML")

summary(training_sem_sub_t_cluster, fit.measures = TRUE, standardized = TRUE)

# 4. Measurement Model 4: Combine DDM parameters --------------------------

model_sub_combn <- '
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
'

training_sem_sub_combn <- 
  sem(model = model_sub_combn, 
      data = training_data, 
      missing = 'ML', 
      orthogonal = TRUE, 
      auto.cov.lv.x = FALSE,
      auto.cov.y = FALSE, 
      std.lv = F)

training_sem_sub_combn_cluster <- lavaan.survey::lavaan.survey(lavaan.fit = training_sem_sub_combn, survey.design = cluster_design, estimator = "ML")

summary(training_sem_sub_combn_cluster, fit.measures = TRUE, standardized = TRUE)



# 5. Full Model -----------------------------------------------------------

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
  
  dep_mnlfa ~~ threat_mnlfa + age_c + sex + eth_black + eth_hisp + eth_other + inr_c + high_edu_c 
  threat_mnlfa ~~ age_c + sex + eth_black + eth_hisp + eth_other + inr_c + high_edu_c 
  age_c ~~ sex + eth_black + eth_hisp + eth_other + inr_c + high_edu_c
  sex ~~ eth_black + eth_hisp + eth_other + inr_c + high_edu_c 
  eth_black ~~ eth_hisp + eth_other + inr_c + high_edu_c 
  eth_hisp ~~ eth_other + inr_c + high_edu_c  
  eth_other ~~ inr_c + high_edu_c 
  inr_c ~~ high_edu_c
 

  # STRUCTURAL MODEL
  
  # Task-general association
  v_general             ~ dep_mnlfa + threat_mnlfa + age_c + sex + eth_black + eth_hisp + eth_other + inr_c + high_edu_c
  a_general             ~ dep_mnlfa + threat_mnlfa + age_c + sex + eth_black + eth_hisp + eth_other + inr_c + high_edu_c
  t_general             ~ dep_mnlfa + threat_mnlfa + age_c + sex + eth_black + eth_hisp + eth_other + inr_c + high_edu_c

  # Task-specific associations
  flanker_v_l           ~ dep_mnlfa + threat_mnlfa + age_c + sex + eth_black + eth_hisp + eth_other + inr_c + high_edu_c 
  flanker_a_l           ~ dep_mnlfa + threat_mnlfa + age_c + sex + eth_black + eth_hisp + eth_other + inr_c + high_edu_c 
  flanker_t_l           ~ dep_mnlfa + threat_mnlfa + age_c + sex + eth_black + eth_hisp + eth_other + inr_c + high_edu_c
  dccs_v_l              ~ dep_mnlfa + threat_mnlfa + age_c + sex + eth_black + eth_hisp + eth_other + inr_c + high_edu_c 
  dccs_a_l              ~ dep_mnlfa + threat_mnlfa + age_c + sex + eth_black + eth_hisp + eth_other + inr_c + high_edu_c 
  dccs_t_l              ~ dep_mnlfa + threat_mnlfa + age_c + sex + eth_black + eth_hisp + eth_other + inr_c + high_edu_c  
  rotation_v_l          ~ dep_mnlfa + threat_mnlfa + age_c + sex + eth_black + eth_hisp + eth_other + inr_c + high_edu_c 
  rotation_a_l          ~ dep_mnlfa + threat_mnlfa + age_c + sex + eth_black + eth_hisp + eth_other + inr_c + high_edu_c
  rotation_t_l          ~ dep_mnlfa + threat_mnlfa + age_c + sex + eth_black + eth_hisp + eth_other + inr_c + high_edu_c
'

training_sem_full <- 
  sem(model = model_full, 
      data = training_data, 
      missing = 'ML', 
      orthogonal = TRUE, 
      auto.cov.lv.x = FALSE,
      auto.cov.y = FALSE, 
      std.lv = F
      )

training_sem_full_cluster <- lavaan.survey::lavaan.survey(lavaan.fit = training_sem_full, survey.design = cluster_design, estimator = "ML")
                 
summary(training_sem_full_cluster, fit.measures = TRUE, standardized = TRUE)



# Save results ------------------------------------------------------------
save(training_sem_sub_v_cluster, training_sem_sub_a_cluster, training_sem_sub_t_cluster, training_sem_sub_combn_cluster, training_sem_full_cluster, file = "analysis_objects/results_sem_training.RData")


