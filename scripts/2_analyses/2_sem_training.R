load('analysis_objects/ddm_data.csv')
load('analysis_objects/iv_data.csv')
load('analysis_objects/training_set.csv')

training_data <- training_data |> 
  left_join(ddm_data) |> 
  left_join(iv_data)



# Specify Clustering Design -----------------------------------------------

# Some children are clustered within families.
# We account for this using the lavaan.survey package

cluster_design <- survey::svydesign(ids=~rel_family_id, prob=~1, data = training_data) 



# 1. Measurement Model 1: Drift Rate --------------------------------------

model_sub_v <- '
  # MEASUREMENT MODEL
  ## General factor
  v_general  =~ 1*pcps_v + dccs_sw_v + dccs_rep_v + rotation_v + flanker_con_v + flanker_incon_v 
  
  # Task-specific variance
  flanker_con_v_var     =~ 1*flanker_con_v
  flanker_incon_v_var   =~ 1*flanker_incon_v
  dccs_sw_v_var         =~ 1*dccs_sw_v
  dccs_rep_v_var        =~ 1*dccs_rep_v
  rotation_v_var        =~ 1*rotation_v
  pcps_v_var            =~ 1*pcps_v
  
  # Covariance structure among unique variances
  flanker_con_v_l   ~~ 0*dccs_sw_v_var + 0*dccs_rep_v_var + 0*rotation_v_var + 0*pcps_v_var
  flanker_incon_v_l ~~ 0*dccs_sw_v_var + 0*dccs_rep_v_var + 0*rotation_v_var + 0*pcps_v_var
  dccs_sw_v_l       ~~ 0*rotation_v_var + 0*pcps_v_var
  dccs_rep_v_l      ~~ 0*rotation_v_var + 0*pcps_v_var
  rotation_v_l      ~~ 0*pcps_v_var
  
  # No covariances between manifest variables
  flanker_con_v    ~~ 0*flanker_con_v + 0*flanker_incon_v + 0*dccs_sw_v + 0*dccs_rep_v + 0*rotation_v + 0*pcps_v
  flanker_incon_v  ~~ 0*flanker_incon_v + 0*dccs_sw_v + 0*dccs_rep_v + 0*rotation_v + 0*pcps_v
  dccs_sw_v        ~~ 0*dccs_sw_v + 0*dccs_rep_v + 0*rotation_v + 0*pcps_v
  dccs_rep_v       ~~ 0*dccs_rep_v + 0*rotation_v + 0*pcps_v
  rotation_v       ~~ 0*rotation_v + 0*pcps_v
  pcps_v           ~~ 0*pcps_v
  
  v_general       ~~ 0*flanker_con_v_var + 0*flanker_incon_v_var + 0*dccs_rep_v_var + 0*dccs_sw_v_var + 0*rotation_v_var + 0*pcps_v_var
'

model_sub_v <- sem(model = model_sub_v, data = training_data, missing = 'ML')
model_sub_v_fit <- lavaan.survey::lavaan.survey(lavaan.fit = model_sub_v, survey.design = cluster_design, estimator = "ML")


# 2. Measurement Model 2: Boundary separation --------------------------

model_sub_a <- '
  # MEASUREMENT MODEL
  ## General factor
  a_general  =~ 1*pcps_a + dccs_a + rotation_a + flanker_a 
  
  # Task-specific variance
  flanker_a_var     =~ 1*flanker_a
  dccs_a_var        =~ 1*dccs_a
  rotation_a_var    =~ 1*rotation_a
  pcps_a_var        =~ 1*pcps_a
  
  # Covariance structure among unique variances
  flanker_a_var     ~~ 0*dccs_a_var + 0*rotation_a_var + 0*pcps_a_var
  dccs_a_var        ~~ 0*rotation_a_var + 0*pcps_a_var
  rotation_a_var    ~~ 0*pcps_a_var
  
  # No covariances between manifest variables
  flanker_a        ~~ 0*flanker_a + 0*dccs_a + 0*rotation_a + 0*pcps_a
  dccs_a           ~~ 0*dccs_a + 0*rotation_a + 0*pcps_a
  rotation_a       ~~ 0*rotation_a + 0*pcps_a
  pcps_a           ~~ 0*pcps_a
  
  a_general       ~~ 0*flanker_a_var +  0*dccs_a_var + 0*rotation_a_var + 0*pcps_a_var
'

model_sub_a <- sem(model = model_sub_a, data = training_data, missing = 'ML')
model_sub_a_fit <- lavaan.survey::lavaan.survey(lavaan.fit = model_sub_a, survey.design = cluster_design, estimator = "ML")


# 3. Measurement Model 3: Non-Decision Time -------------------------------

model_sub_t0 <- '
  # MEASUREMENT MODEL
  ## General factor
  t0_general  =~ 1*pcps_t0 + dccs_sw_t0 + dccs_rep_t0 + rotation_t0 + flanker_con_t0 + flanker_incon_t0 
  
  # Task-specific variance
  flanker_con_t0_var     =~ 1*flanker_con_t0
  flanker_incon_t0_var   =~ 1*flanker_incon_t0
  dccs_sw_t0_var         =~ 1*dccs_sw_t0
  dccs_rep_t0_var        =~ 1*dccs_rep_t0
  rotation_t0_var        =~ 1*rotation_t0
  pcps_t0_var            =~ 1*pcps_t0
  
  # Covariance structure among unique variances
  flanker_con_t0_var     ~~ 0*dccs_sw_t0_var + 0*dccs_rep_t0_var + 0*rotation_t0_var + 0*pcps_t0_var
  flanker_incon_t0_var   ~~ 0*dccs_sw_t0_var + 0*dccs_rep_t0_var + 0*rotation_t0_var + 0*pcps_t0_var
  dccs_sw_t0_var         ~~ 0*rotation_t0_var + 0*pcps_t0_var
  dccs_rep_t0_var        ~~ 0*rotation_t0_var + 0*pcps_t0_var
  rotation_t0_var        ~~ 0*pcps_t0_var
  
  # No covariances between manifest variables
  flanker_con_t0         ~~ 0*flanker_con_t0 + 0*flanker_incon_t0 + 0*dccs_sw_t0 + 0*dccs_rep_t0 + 0*rotation_t0 + 0*pcps_t0
  flanker_incon_t0       ~~ 0*flanker_incon_t0 + 0*dccs_sw_t0 + 0*dccs_rep_t0 + 0*rotation_t0 + 0*pcps_t0
  dccs_sw_t0             ~~ 0*dccs_sw_t0 + 0*dccs_rep_t0 + 0*rotation_t0 + 0*pcps_t0
  dccs_rep_t0            ~~ 0*dccs_rep_t0 + 0*rotation_t0 + 0*pcps_t0
  rotation_t0            ~~ 0*rotation_t0 + 0*pcps_t0
  pcps_t0                ~~ 0*pcps_t0
        
  t0_general             ~~ 0*flanker_con_t0_var + 0*flanker_incon_t0_var + 0*dccs_rep_t0_var + 0*dccs_sw_t0_var + 0*rotation_t0_var + 0*pcps_t0_var
'

model_sub_t0 <- sem(model = model_sub_t0, data = training_data, missing = 'ML')
model_sub_t0_fit <- lavaan.survey::lavaan.survey(lavaan.fit = model_sub_t0, survey.design = cluster_design, estimator = "ML")


# 4. Measurement Model 4: Combine DDM parameters --------------------------

model_sub_combn <- '

# MEASUREMENT MODEL
  # General latent factors
  v_general  =~ 1*pcps_v + dccs_sw_v + dccs_rep_v + rotation_v + flanker_con_v + flanker_incon_v 
  a_general  =~ 1*pcps_a + dccs_sw_a + dccs_rep_a + rotation_a + flanker_con_a + flanker_incon_a  
  t0_general =~ 1*pcps_t0 + dccs_sw_t0 + dccs_rep_t0 + rotation_t0 + flanker_con_t0 + flanker_incon_t0
  
  
  # Task-specific variance
  flanker_con_v_var     =~ 1*flanker_con_v
  flanker_incon_v_var   =~ 1*flanker_incon_v
  dccs_sw_v_var         =~ 1*dccs_sw_v
  dccs_rep_v_var        =~ 1*dccs_rep_v
  rotation_v_var        =~ 1*rotation_v
  pcps_v_var            =~ 1*pcps_v
  
  flanker_con_a_var     =~ 1*flanker_con_a
  flanker_incon_a_var   =~ 1*flanker_incon_a
  dccs_sw_a_var         =~ 1*dccs_sw_a
  dccs_rep_a_var        =~ 1*dccs_rep_a
  rotation_a_var        =~ 1*rotation_a
  pcps_a_var            =~ 1*pcps_a
  
  flanker_con_t0_var    =~ 1*flanker_con_t0
  flanker_incon_t0_var  =~ 1*flanker_incon_t0
  dccs_sw_t0_var        =~ 1*dccs_sw_t0
  dccs_rep_t0_var       =~ 1*dccs_rep_t0
  rotation_t0_var       =~ 1*rotation_t0
  pcps_t0_var           =~ 1*pcps_t0
  
  
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

model_sub_combn <- sem(model = model_sub_combn, data = training_data, missing = 'ML')
model_sub_combn_fit <- lavaan.survey::lavaan.survey(lavaan.fit = model_sub_combn, survey.design = cluster_design, estimator = "ML")



# 5. Full Model -----------------------------------------------------------

model_full <- '
  # MEASUREMENT MODEL
  # General latent factors
  v_general  =~ 1*pcps_v + dccs_sw_v + dccs_rep_v + rotation_v + flanker_con_v + flanker_incon_v 
  a_general  =~ 1*pcps_a + dccs_sw_a + dccs_rep_a + rotation_a + flanker_con_a + flanker_incon_a  
  t0_general =~ 1*pcps_t0 + dccs_sw_t0 + dccs_rep_t0 + rotation_t0 + flanker_con_t0 + flanker_incon_t0
  
  
  # Task-specific variance
  flanker_con_v_var     =~ 1*flanker_con_v
  flanker_incon_v_var   =~ 1*flanker_incon_v
  dccs_sw_v_var         =~ 1*dccs_sw_v
  dccs_rep_v_var        =~ 1*dccs_rep_v
  rotation_v_var        =~ 1*rotation_v
  pcps_v_var            =~ 1*pcps_v
  
  flanker_con_a_var     =~ 1*flanker_con_a
  flanker_incon_a_var   =~ 1*flanker_incon_a
  dccs_sw_a_var         =~ 1*dccs_sw_a
  dccs_rep_a_var        =~ 1*dccs_rep_a
  rotation_a_var        =~ 1*rotation_a
  pcps_a_var            =~ 1*pcps_a
  
  flanker_con_t0_var    =~ 1*flanker_con_t0
  flanker_incon_t0_var  =~ 1*flanker_incon_t0
  dccs_sw_t0_var        =~ 1*dccs_sw_t0
  dccs_rep_t0_var       =~ 1*dccs_rep_t0
  rotation_t0_var       =~ 1*rotation_t0
  pcps_t0_var           =~ 1*pcps_t0
  
  
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
  v_general     ~ thr_home + inr + edu_high + race_dummy1 + race_dummy2 + race_dummy3 + sex_dummy + age_c
  a_general     ~ thr_home + inr + edu_high + race_dummy1 + race_dummy2 + race_dummy3 + sex_dummy + age_c
  t0_general    ~ thr_home + inr + edu_high + race_dummy1 + race_dummy2 + race_dummy3 + sex_dummy + age_c
  
  v_general     ~ mat_dep + inr + edu_high + race_dummy1 + race_dummy2 + race_dummy3 + sex_dummy + age_c 
  a_general     ~ mat_dep + inr + edu_high + race_dummy1 + race_dummy2 + race_dummy3 + sex_dummy + age_c
  t0_general    ~ mat_dep + inr + edu_high + race_dummy1 + race_dummy2 + race_dummy3 + sex_dummy + age_c

  # Task-specific associations
  flanker_v_l   ~ thr_home + inr + edu_high + race_dummy1 + race_dummy2 + race_dummy3 + sex_dummy + age_c
  flanker_a_  l ~ thr_home + inr + edu_high + race_dummy1 + race_dummy2 + race_dummy3 + sex_dummy + age_c
  flanker_t0_l  ~ thr_home + inr + edu_high + race_dummy1 + race_dummy2 + race_dummy3 + sex_dummy + age_c
  dccs_v_l      ~ thr_home + inr + edu_high + race_dummy1 + race_dummy2 + race_dummy3 + sex_dummy + age_c
  dccs_a_l      ~ thr_home + inr + edu_high + race_dummy1 + race_dummy2 + race_dummy3 + sex_dummy + age_c
  dccs_t0_l     ~ thr_home + inr + edu_high + race_dummy1 + race_dummy2 + race_dummy3 + sex_dummy + age_c
  rotation_v_l  ~ thr_home + inr + edu_high + race_dummy1 + race_dummy2 + race_dummy3 + sex_dummy + age_c
  rotation_a_l  ~ thr_home + inr + edu_high + race_dummy1 + race_dummy2 + race_dummy3 + sex_dummy + age_c
  rotation_t0_l ~ thr_home + inr + edu_high + race_dummy1 + race_dummy2 + race_dummy3 + sex_dummy + age_c
  
  flanker_v_l   ~ mat_dep + inr + edu_high + race_dummy1 + race_dummy2 + race_dummy3 + sex_dummy + age_c
  flanker_a_  l ~ mat_dep + inr + edu_high + race_dummy1 + race_dummy2 + race_dummy3 + sex_dummy + age_c
  flanker_t0_l  ~ mat_dep + inr + edu_high + race_dummy1 + race_dummy2 + race_dummy3 + sex_dummy + age_c
  dccs_v_l      ~ mat_dep + inr + edu_high + race_dummy1 + race_dummy2 + race_dummy3 + sex_dummy + age_c
  dccs_a_l      ~ mat_dep + inr + edu_high + race_dummy1 + race_dummy2 + race_dummy3 + sex_dummy + age_c
  dccs_t0_l     ~ mat_dep + inr + edu_high + race_dummy1 + race_dummy2 + race_dummy3 + sex_dummy + age_c
  rotation_v_l  ~ mat_dep + inr + edu_high + race_dummy1 + race_dummy2 + race_dummy3 + sex_dummy + age_c
  rotation_a_l  ~ mat_dep + inr + edu_high + race_dummy1 + race_dummy2 + race_dummy3 + sex_dummy + age_c
  rotation_t0_l ~ mat_dep + inr + edu_high + race_dummy1 + race_dummy2 + race_dummy3 + sex_dummy + age_c
'

model_full <- sem(model = model_full, data = training_data, missing = 'ML')
model_full_fit <- lavaan.survey::lavaan.survey(lavaan.fit = model_full, survey.design = cluster_design, estimator = "ML")




# Save results ------------------------------------------------------------
save(model_sub_v, model_sub_v_fit, model_sub_a, model_sub_a_fit, model_sub_t0, model_sub_t0_fit, model_combn, model_combn_fit, model_full, model_full_fit, file = "sem_training_results.RData")


