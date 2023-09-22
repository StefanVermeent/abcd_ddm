Scripts
================

# Data Processing Scripts

There are four types of R-scripts in this repository, each with a
separate folder.

- [`custom_functions/`](https://github.com/stefanvermeent/abcd_ddm/tree/main/scripts/custom_functions):
  Custom R-functions written for this project
- [`0_simulations/`](https://github.com/stefanvermeent/abcd_ddm/tree/main/scripts/0_simulations):
  Simulation scripts for power and DDM analyses
- [`1_data_prep/`](https://github.com/stefanvermeent/abcd_ddm/tree/main/scripts/1_data_prep):
  Data processing scripts
- [`2_analyses/`](https://github.com/stefanvermeent/abcd_ddm/tree/main/scripts/2_analyses):
  Primary and exploratory analysis scripts

Each script takes an input(s) and produces output(s). All outputs are
stored in
[analysis_objects/](https://github.com/stefanvermeent/abcd_ddm/tree/main/analysis_objects).
The tables below provides an overview of the inputs and outputs of each
script.

### Simulations

| script                   | input | output                                                                       |
|--------------------------|-------|------------------------------------------------------------------------------|
| ddm_missing_imputation.R |       | results_sim5.RData’<br> ddm_sim5_results.RData                               |
| ddm_trial_simulations.R  |       | ddm_sim1_results.RData<br> ddm_sim2_results.RData<br> ddm_sim3_results.RData |
| ddm_validity_checks.R    |       | ddm_sim4_results.RData                                                       |
| power_alternative.R      |       | power.RData’                                                                 |
| power_analysis.R         |       | power.RData’                                                                 |

### Data Prep

| script            | input           | output                                                                                         |
|-------------------|-----------------|------------------------------------------------------------------------------------------------|
| 1_preprocessing.R |                 | tasks_raw.RData’<br> descriptives.RData’                                                       |
| 2_clean_data.R    | tasks_raw.RData | lmt_clean.csv<br> flanker_clean.csv<br> pcps_clean.csv<br> dccs_clean.csv<br> exclusions.RData |
| 3_data_subsets.R  |                 | training_set.csv<br> test_set.csv                                                              |

### Analyses

| script                  | input                                                                                                                                                                                           | output                                                                                                                                                                                                                                                                   |
|-------------------------|-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| 1_ddm_fit.R             |                                                                                                                                                                                                 | ddm_lmt_mod1.RData’<br> ddm_lmt_mod2.RData’<br> ddm_flanker_mod1.RData’<br> ddm_flanker_mod2.RData’<br> ddm_pcps_mod1.RData’<br> ddm_pcps_mod1b.RData’<br> ddm_dccs_mod1.RData’<br> ddm_dccs_mod2.RData’                                                                 |
| 2_ivs.R                 |                                                                                                                                                                                                 | iv_data.csv                                                                                                                                                                                                                                                              |
| 3_ddm_extract_results.R | ddm_lmt_mod1.RData<br> ddm_lmt_mod2.RData<br> ddm_flanker_mod1.RData<br> ddm_flanker_mod2.RData<br> ddm_dccs_mod1.RData<br> ddm_dccs_mod2.RData<br> ddm_pcps_mod1.RData<br> ddm_pcps_mod2.RData | ddm_lmt_mod1_parsed.RData<br> ddm_lmt_mod2_parsed.RData<br> ddm_flanker_mod1_parsed.RData<br> ddm_flanker_mod2_parsed.RData<br> ddm_dccs_mod1_parsed.RData<br> ddm_dccs_mod2_parsed.RData<br> ddm_pcps_mod1_parsed.RData<br> ddm_pcps_mod1_parsed.RData<br> ddm_data.csv |
| 4_sem_training.R        |                                                                                                                                                                                                 | results_sem_training.RData                                                                                                                                                                                                                                               |
| 5_sem_test.R            |                                                                                                                                                                                                 | test_sem_full_clustertest_factor_input:<br> results_sem_test.RData                                                                                                                                                                                                       |
| 6_exploratory.R         |                                                                                                                                                                                                 | exploratory_analysis.RData                                                                                                                                                                                                                                               |
