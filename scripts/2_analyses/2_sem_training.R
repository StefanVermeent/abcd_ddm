load("closed_data/ddm_data.RData")
load("closed_data/iv_data.RData")
load("closed_data/training_set.RData")

training_data <- training_data |> 
  left_join(ddm_data) |> 
  left_join(iv_data)



# 1. Measurement Model 1: Drift Rate --------------------------------------

model_sub_v <- '
  # MEASUREMENT MODEL
  ## General factor
  v_general =~ 1*pcps_v + dccs_v + rotation_v + flanker_v 
  
  ## Task-specific variance
  flanker_v_l     =~ 1*flanker_v
  dccs_v_l        =~ 1*dccs_v
  rotation_v_l    =~ 1*rotation_v
  pcps_v_l        =~ 1*pcps_v
  
  # Covariance structure among unique variances
  flanker_v_l     ~~ 0*dccs_v_l + 0*rotation_v_l + 0*pcps_v_l
  dccs_v_l        ~~ 0*rotation_v_l + 0*pcps_v_l
  rotation_v_l    ~~ 0*pcps_v_l
  
  # No covariances between manifest variables
  flanker_v     ~~ 0*flanker_v + 0*dccs_v + 0*rotation_v + 0*pcps_v
  dccs_v        ~~ 0*dccs_v + 0*rotation_v + 0*pcps_v
  rotation_v    ~~ 0*rotation_v + 0*pcps_v
  pcps_v        ~~ 0*pcps_v
  
  v_general     ~~ 0*flanker_v_l + 0*dccs_v_l + 0*rotation_v_l + 0*pcps_v_l
  
'

model_sub_v_fit <- sem(model = model_sub_v, data = training_data)
summary(model_sub_v_fit, fit.measures = T)


# 2. Measurement Model 2: Boundary separation --------------------------

model_sub_a <- '
  # MEASUREMENT MODEL
  ## General factor
  a_general =~ 1*pcps_a + dccs_a + rotation_a + flanker_a
  
  # Task-specific variance
  flanker_a_l     =~ 1*flanker_a
  dccs_a_l        =~ 1*dccs_a
  rotation_a_l    =~ 1*rotation_a
  pcps_a_l        =~ 1*pcps_a
  
  # Covariance structure among unique variances
  flanker_a_l     ~~ 0*dccs_a_l + 0*rotation_a_l + 0*pcps_a_l
  dccs_a_l        ~~ 0*rotation_a_l + 0*pcps_a_l
  rotation_a_l    ~~ 0*pcps_a_l
  
  # No covariances between manifest variables
  flanker_a     ~~ 0*flanker_a + 0*dccs_a + 0*rotation_a + 0*pcps_a
  dccs_a        ~~ 0*dccs_a + 0*rotation_a + 0*pcps_a
  rotation_a    ~~ 0*rotation_a + 0*pcps_a
  pcps_a        ~~ 0*pcps_a
  
  a_general     ~~ 0*flanker_a_l + 0*dccs_a_l + 0*rotation_a_l + 0*pcps_a_l
  
'

model_sub_a_fit <- sem(model = model_sub_a, data = training_data)
summary(model_sub_a_fit, fit.measures = T)

# 3. Measurement Model 3: Non-Decision Time -------------------------------

model_sub_t0 <- '
  # MEASUREMENT MODEL
  ## General factor
  t0_general =~ 1*pcps_t0 + dccs_t0 + rotation_t0 + flanker_t0
  
  # Task-specific variance
  flanker_t0_l     =~ 1*flanker_t0
  dccs_t0_l        =~ 1*dccs_t0
  rotation_t0_l    =~ 1*rotation_t0
  pcps_t0_l        =~ 1*pcps_t0
  
  # Covariance structure among unique variances
  flanker_t0_l     ~~ 0*dccs_t0_l + 0*rotation_t0_l + 0*pcps_t0_l
  dccs_t0_l        ~~ 0*rotation_t0_l + 0*pcps_t0_l
  rotation_t0_l    ~~ 0*pcps_t0_l
  
  # No covariances between manifest variables
  flanker_t0     ~~ 0*flanker_t0 + 0*dccs_t0 + 0*rotation_t0 + 0*pcps_t0
  dccs_t0        ~~ 0*dccs_t0 + 0*rotation_t0 + 0*pcps_t0
  rotation_t0    ~~ 0*rotation_t0 + 0*pcps_t0
  pcps_t0       ~~ 0*pcps_t0
  
  t0_general     ~~ 0*flanker_t0_l + 0*dccs_t0_l + 0*rotation_t0_l + 0*pcps_t0_l
  
'

model_sub_t0_fit <- sem(model = model_sub_t0, data = training_data)
summary(model_sub_t0_fit, fit.measures = T)

# 4. Measurement Model 4: Combine DDM parameters --------------------------

model_sub_combn <- '

  # MEASUREMENT MODEL
  # General latent factors
  v_general  =~ 1*pcps_v + dccs_v + rotation_v + flanker_v 
  a_general  =~ 1*pcps_a + dccs_a + rotation_a + flanker_a 
  t0_general =~ 1*pcps_t0 + dccs_t0 + rotation_t0 + flanker_t0
  
  
  # Task-specific variance
  flanker_v_l     =~ 1*flanker_v
  dccs_v_l        =~ 1*dccs_v
  rotation_v_l    =~ 1*rotation_v
  pcps_v_l        =~ 1*pcps_v
  
  flanker_a_l     =~ 1*flanker_a
  dccs_a_l        =~ 1*dccs_a
  rotation_a_l    =~ 1*rotation_a
  pcps_a_l        =~ 1*pcps_a
  
  flanker_t0_l    =~ 1*flanker_t0
  dccs_t0_l       =~ 1*dccs_t0
  rotation_t0_l   =~ 1*rotation_t0
  pcps_t0_l       =~ 1*pcps_t0
  
  
  # Only covariances between general latent factors and residual variances of the same task.
  v_general       ~~ a_general + t0_general
  a_general       ~~ t0_general
  
  flanker_v_l     ~~ flanker_a_l + flanker_t0_l + 0*dccs_v_l + 0*dccs_a_l + 0*dccs_t0_l + 0*rotation_v_l + 0*rotation_a_l + 0*rotation_t0_l + 
    0*pcps_v_l + 0*pcps_a_l + 0*pcps_t0_l
  flanker_a_l     ~~ flanker_t0_l + 0*dccs_v_l + 0*dccs_a_l + 0*dccs_t0_l + 0*rotation_v_l + 0*rotation_a_l + 0*rotation_t0_l + 
    0*pcps_v_l + 0*pcps_a_l + 0*pcps_t0_l
  flanker_t0_l    ~~ 0*dccs_v_l + 0*dccs_a_l + 0*dccs_t0_l + 0*rotation_v_l + 0*rotation_a_l + 0*rotation_t0_l + 
    0*pcps_v_l + 0*pcps_a_l + 0*pcps_t0_l
  
  dccs_v_l        ~~ dccs_a_l + dccs_t0_l + 0*rotation_v_l + 0*rotation_a_l + 0*rotation_t0_l + 
    0*pcps_v_l + 0*pcps_a_l + 0*pcps_t0_l
  dccs_a_l        ~~ dccs_t0_l + 0*rotation_v_l + 0*rotation_a_l + 0*rotation_t0_l + 0*pcps_v_l + 0*pcps_a_l + 0*pcps_t0_l
  dccs_t0_l       ~~ 0*rotation_v_l + 0*rotation_a_l + 0*rotation_t0_l + 0*pcps_v_l + 0*pcps_a_l + 0*pcps_t0_l
  
  rotation_v_l    ~~ rotation_a_l + rotation_t0_l + 0*pcps_v_l + 0*pcps_a_l + 0*pcps_t0_l
  rotation_a_l    ~~ rotation_t0_l + 0*pcps_v_l + 0*pcps_a_l + 0*pcps_t0_l
  rotation_t0_l   ~~ 0*pcps_v_l + 0*pcps_a_l + 0*pcps_t0_l
  
  pcps_v_l        ~~ pcps_a_l + pcps_t0_l
  pcps_a_l        ~~ pcps_t0_l
  
  
  # No covariances between manifest variables
  flanker_v     ~~ 0*flanker_v + 0*flanker_a + 0*flanker_t0 + 0*dccs_v + 0*dccs_a + 0*dccs_t0 + 0*rotation_v + 0*rotation_a + 0*rotation_t0 + 0*pcps_v + 0*pcps_a + 0*pcps_t0
  flanker_a     ~~ 0*flanker_a + 0*flanker_t0 + 0*dccs_v + 0*dccs_a + 0*dccs_t0 + 0*rotation_v + 0*rotation_a + 0*rotation_t0 + 0*pcps_v + 0*pcps_a + 0*pcps_t0
  flanker_t0    ~~ 0*flanker_t0 + 0*dccs_v + 0*dccs_a + 0*dccs_t0 + 0*rotation_v + 0*rotation_a + 0*rotation_t0 + 0*pcps_v + 0*pcps_a + 0*pcps_t0
  dccs_v        ~~ 0*dccs_v + 0*dccs_a + 0*dccs_t0 + 0*rotation_v + 0*rotation_a + 0*rotation_t0 + 0*pcps_v + 0*pcps_a + 0*pcps_t0
  dccs_a        ~~ 0*dccs_a + 0*dccs_t0 + 0*rotation_v + 0*rotation_a + 0*rotation_t0 + 0*pcps_v + 0*pcps_a + 0*pcps_t0
  dccs_t0       ~~ 0*dccs_t0 + 0*rotation_v + 0*rotation_a + 0*rotation_t0 + 0*pcps_v + 0*pcps_a + 0*pcps_t0
  rotation_v    ~~ 0*rotation_v + 0*rotation_a + 0*rotation_t0 + 0*pcps_v + 0*pcps_a + 0*pcps_t0
  rotation_a    ~~ 0*rotation_a + 0*rotation_t0 + 0*pcps_v + 0*pcps_a + 0*pcps_t0
  rotation_t0   ~~ 0*rotation_t0 + 0*pcps_v + 0*pcps_a + 0*pcps_t0
  pcps_v        ~~ 0*pcps_v + 0*pcps_a + 0*pcps_t0
  pcps_a        ~~ 0*pcps_a + 0*pcps_t0
  pcps_t0       ~~ 0*pcps_t0
  
  # No covariances between general drift rate and residual variances
  v_general     ~~ 0*flanker_v_l + 0*flanker_a_l + 0*flanker_t0_l + 0*dccs_v_l + 0*dccs_a_l + 0*dccs_t0_l + 0*rotation_v_l + 0*rotation_a_l + 0*rotation_t0_l + 
    0*pcps_v_l + 0*pcps_a_l + 0*pcps_t0_l
  a_general     ~~ 0*flanker_v_l + 0*flanker_a_l + 0*flanker_t0_l + 0*dccs_v_l + 0*dccs_a_l + 0*dccs_t0_l + 0*rotation_v_l + 0*rotation_a_l + 0*rotation_t0_l + 
    0*pcps_v_l + 0*pcps_a_l + 0*pcps_t0_l
  t0_general    ~~ 0*flanker_v_l + 0*flanker_a_l + 0*flanker_t0_l + 0*dccs_v_l + 0*dccs_a_l + 0*dccs_t0_l + 0*rotation_v_l + 0*rotation_a_l + 0*rotation_t0_l + 
    0*pcps_v_l + 0*pcps_a_l + 0*pcps_t0_l                 
'

model_sub_combn_fit <- sem(model = model_sub_combn, data = training_data)
summary(model_sub_combn_fit, fit.measures = T)



# 5. Full Model -----------------------------------------------------------

model_full <- '
  # MEASUREMENT MODEL
  # General latent factors
  v_general  =~ 1*pcps_v + dccs_v + rotation_v + flanker_v 
  a_general  =~ 1*pcps_a + dccs_a + rotation_a + flanker_a 
  t0_general =~ 1*pcps_t0 + dccs_t0 + rotation_t0 + flanker_t0
  
  
  # Task-specific variance
  flanker_v_l     =~ 1*flanker_v
  dccs_v_l        =~ 1*dccs_v
  rotation_v_l    =~ 1*rotation_v
  pcps_v_l        =~ 1*pcps_v
  
  flanker_a_l     =~ 1*flanker_a
  dccs_a_l        =~ 1*dccs_a
  rotation_a_l    =~ 1*rotation_a
  pcps_a_l        =~ 1*pcps_a
  
  flanker_t0_l    =~ 1*flanker_t0
  dccs_t0_l       =~ 1*dccs_t0
  rotation_t0_l   =~ 1*rotation_t0
  pcps_t0_l       =~ 1*pcps_t0
  
  
  # Only covariances between general latent factors and residual variances of the same task.
  v_general       ~~ a_general + t0_general
  a_general       ~~ t0_general
  
  flanker_v_l     ~~ flanker_a_l + flanker_t0_l + 0*dccs_v_l + 0*dccs_a_l + 0*dccs_t0_l + 0*rotation_v_l + 0*rotation_a_l + 0*rotation_t0_l + 
    0*pcps_v_l + 0*pcps_a_l + 0*pcps_t0_l
  flanker_a_l     ~~ flanker_t0_l + 0*dccs_v_l + 0*dccs_a_l + 0*dccs_t0_l + 0*rotation_v_l + 0*rotation_a_l + 0*rotation_t0_l + 
    0*pcps_v_l + 0*pcps_a_l + 0*pcps_t0_l
  flanker_t0_l    ~~ 0*dccs_v_l + 0*dccs_a_l + 0*dccs_t0_l + 0*rotation_v_l + 0*rotation_a_l + 0*rotation_t0_l + 
    0*pcps_v_l + 0*pcps_a_l + 0*pcps_t0_l
  
  dccs_v_l        ~~ dccs_a_l + dccs_t0_l + 0*rotation_v_l + 0*rotation_a_l + 0*rotation_t0_l + 
    0*pcps_v_l + 0*pcps_a_l + 0*pcps_t0_l
  dccs_a_l        ~~ dccs_t0_l + 0*rotation_v_l + 0*rotation_a_l + 0*rotation_t0_l + 0*pcps_v_l + 0*pcps_a_l + 0*pcps_t0_l
  dccs_t0_l       ~~ 0*rotation_v_l + 0*rotation_a_l + 0*rotation_t0_l + 0*pcps_v_l + 0*pcps_a_l + 0*pcps_t0_l
  
  rotation_v_l    ~~ rotation_a_l + rotation_t0_l + 0*pcps_v_l + 0*pcps_a_l + 0*pcps_t0_l
  rotation_a_l    ~~ rotation_t0_l + 0*pcps_v_l + 0*pcps_a_l + 0*pcps_t0_l
  rotation_t0_l   ~~ 0*pcps_v_l + 0*pcps_a_l + 0*pcps_t0_l
  
  pcps_v_l        ~~ pcps_a_l + pcps_t0_l
  pcps_a_l        ~~ pcps_t0_l
  
  
  # No covariances between manifest variables
  flanker_v     ~~ 0*flanker_v + 0*flanker_a + 0*flanker_t0 + 0*dccs_v + 0*dccs_a + 0*dccs_t0 + 0*rotation_v + 0*rotation_a + 0*rotation_t0 + 0*pcps_v + 0*pcps_a + 0*pcps_t0
  flanker_a     ~~ 0*flanker_a + 0*flanker_t0 + 0*dccs_v + 0*dccs_a + 0*dccs_t0 + 0*rotation_v + 0*rotation_a + 0*rotation_t0 + 0*pcps_v + 0*pcps_a + 0*pcps_t0
  flanker_t0    ~~ 0*flanker_t0 + 0*dccs_v + 0*dccs_a + 0*dccs_t0 + 0*rotation_v + 0*rotation_a + 0*rotation_t0 + 0*pcps_v + 0*pcps_a + 0*pcps_t0
  dccs_v        ~~ 0*dccs_v + 0*dccs_a + 0*dccs_t0 + 0*rotation_v + 0*rotation_a + 0*rotation_t0 + 0*pcps_v + 0*pcps_a + 0*pcps_t0
  dccs_a        ~~ 0*dccs_a + 0*dccs_t0 + 0*rotation_v + 0*rotation_a + 0*rotation_t0 + 0*pcps_v + 0*pcps_a + 0*pcps_t0
  dccs_t0       ~~ 0*dccs_t0 + 0*rotation_v + 0*rotation_a + 0*rotation_t0 + 0*pcps_v + 0*pcps_a + 0*pcps_t0
  rotation_v    ~~ 0*rotation_v + 0*rotation_a + 0*rotation_t0 + 0*pcps_v + 0*pcps_a + 0*pcps_t0
  rotation_a    ~~ 0*rotation_a + 0*rotation_t0 + 0*pcps_v + 0*pcps_a + 0*pcps_t0
  rotation_t0   ~~ 0*rotation_t0 + 0*pcps_v + 0*pcps_a + 0*pcps_t0
  pcps_v        ~~ 0*pcps_v + 0*pcps_a + 0*pcps_t0
  pcps_a        ~~ 0*pcps_a + 0*pcps_t0
  pcps_t0       ~~ 0*pcps_t0
  
  # No covariances between general drift rate and residual variances
  v_general     ~~ 0*flanker_v_l + 0*flanker_a_l + 0*flanker_t0_l + 0*dccs_v_l + 0*dccs_a_l + 0*dccs_t0_l + 0*rotation_v_l + 0*rotation_a_l + 0*rotation_t0_l + 
    0*pcps_v_l + 0*pcps_a_l + 0*pcps_t0_l
  a_general     ~~ 0*flanker_v_l + 0*flanker_a_l + 0*flanker_t0_l + 0*dccs_v_l + 0*dccs_a_l + 0*dccs_t0_l + 0*rotation_v_l + 0*rotation_a_l + 0*rotation_t0_l + 
    0*pcps_v_l + 0*pcps_a_l + 0*pcps_t0_l
  t0_general    ~~ 0*flanker_v_l + 0*flanker_a_l + 0*flanker_t0_l + 0*dccs_v_l + 0*dccs_a_l + 0*dccs_t0_l + 0*rotation_v_l + 0*rotation_a_l + 0*rotation_t0_l + 
    0*pcps_v_l + 0*pcps_a_l + 0*pcps_t0_l     
    
    
  # STRUCTURAL MODEL
  
  # Task-general association
  threat     ~ v_general + a_general + t0_general
  
  # Task-specific associations
  threat     ~ flanker_v_l + flanker_a_l + flanker_t0_l
  threat     ~ dccs_v_l + dccs_a_l + dccs_t0_l
  threat     ~ rotation_v_l + rotation_a_l + rotation_t0_l
'
