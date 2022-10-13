library(tidyverse)
library(RWiener)
library(faux)


# Parameter settings

n_subj <- 100
n_trial <- 500
skill_mean <- c(0.2, 0.5, 0.8)
skill_sd <- skill_mean/10
skill_r <- c(0, 0.5, 0.7)



set.seed(4757343)
parm_grid <- 
  expand_grid(
    n_subj     = n_subj,
    n_trial    = n_trial,
    skill_mean = skill_mean,
    skill_sd   = skill_sd,
    skill_r    = skill_r
  ) %>%
  # Add simulated subject-level DDM parameters
  mutate(
    ddm_parms = pmap(function(){
      tibble(
        id     = 1:n_subj,
        a_sim  = rnorm(n = n_subj, 2, 0.5),
        t0_sim = rnorm(n = n_subj, 0.4, 0.1),
        z_sim  = 0.5,
        v_sim  = rnorm(n = n_subj, 2.5, 1),
      )
    })
  ) %>%
  # Generate RT/accuracy data based on DDM parameters
  mutate(
    raw_data
  )


# Generate initial DDM parameters and corresponding RTs and accuracy.
set.seed(4757343)
init_data <- tibble(
  id     = 1:n_subj,
  a_sim  = rnorm(n = n_subj, 2, 0.5),
  t0_sim = rnorm(n = n_subj, 0.4, 0.1),
  z_sim  = 0.5,
  v_sim  = rnorm(n = n_subj, 2.5, 1),
) %>%
  mutate(
    responses = pmap(., function(id,a_sim,t0_sim,z_sim,v_sim) {
      
      RWiener::rwiener(n=n_trial, alpha=a_sim, tau=t0_sim, beta=z_sim, delta=v_sim) %>%
        as_tibble() %>%
        rename(
          rt = q,
          correct = resp
        )
    })
  ) %>%
  unnest(responses) %>%
  group_by(id) %>%
  mutate(acc = sum(correct == "upper")/n()) %>%
  ungroup()

# Calculate an offset to t0 representing the cognitive skill of interest that is correlated to accuracy.
acc_data <- init_data %>%
  group_by(id) %>%
  summarise(acc = mean(acc)) %>%
  mutate(t0_offset = faux::rnorm_pre(x = acc, mu = skill_mean, sd = skill_sd, r = skill_cor))


# Recalculate RTs and accuracy based on updated t0 parameter
data <- init_data %>%
  select(id, a_sim, t0_sim, z_sim, v_sim) %>%
  distinct() %>%
  left_join(acc_data) %>%
  mutate(t0_sim = t0_sim + t0_offset) %>%
  select(-c(acc, t0_offset)) %>%
  mutate(
    responses = pmap(., function(id,a_sim,t0_sim,z_sim,v_sim) {
      
      RWiener::rwiener(n=n_trial, alpha=a_sim, tau=t0_sim, beta=z_sim, delta=v_sim) %>%
        as_tibble() %>%
        rename(
          rt = q,
          correct = resp
        )
    })
  ) %>%
  unnest(responses) %>%
  group_by(id) %>%
  mutate(acc = sum(correct == "upper")/n()) %>%
  ungroup() %>% 
  rename(subjID = id, choice = correct, RT = rt) %>%
  mutate(choice = ifelse(choice == "upper", 1, 0))

