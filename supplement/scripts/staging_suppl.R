
# 1. Load objects ------------------------------------------------------------
lmt_clean     <- readr::read_csv(paste0("data/lmt_clean", data_suffix, ".csv")) |> mutate(task = 1)
flanker_clean <- readr::read_csv(paste0("data/flanker_clean", data_suffix, ".csv")) |> mutate(task = 2)
dccs_clean    <- readr::read_csv(paste0("data/dccs_clean", data_suffix, ".csv")) |> mutate(task = 3)
pcps_clean    <- readr::read_csv(paste0("data/pcps_clean", data_suffix, ".csv")) |> mutate(task = 4)



load('analysis_objects/power.RData')
load('analysis_objects/ddm_sim1_results.RData')
load('analysis_objects/ddm_sim2_results.RData')
load('analysis_objects/ddm_sim3_results.RData')
load("analysis_objects/ddm_sim4_results.RData")
load("analysis_objects/ddm_sim5_results.RData")

load('analysis_objects/ddm_flanker_mod1_parsed.RData')
load('analysis_objects/ddm_flanker_mod2_parsed.RData')
load('analysis_objects/ddm_lmt_mod1_parsed.RData')
load('analysis_objects/ddm_lmt_mod2_parsed.RData')
load('analysis_objects/ddm_dccs_mod1_parsed.RData')
load('analysis_objects/ddm_dccs_mod2_parsed.RData')
load('analysis_objects/ddm_pcps_mod1_parsed.RData')
load('analysis_objects/ddm_pcps_mod2_parsed.RData')

load('analysis_objects/ddm_flanker_mod1_orig_parsed.RData')
load('analysis_objects/ddm_flanker_mod2_orig_parsed.RData')
load('analysis_objects/ddm_lmt_mod1_orig_parsed.RData')
load('analysis_objects/ddm_lmt_mod2_orig_parsed.RData')
load('analysis_objects/ddm_dccs_mod1_orig_parsed.RData')
load('analysis_objects/ddm_dccs_mod2_orig_parsed.RData')
load('analysis_objects/ddm_pcps_mod1_orig_parsed.RData')
load('analysis_objects/ddm_pcps_mod2_orig_parsed.RData')

load('analysis_objects/rhat_flanker_mod1.RData')
load('analysis_objects/rhat_flanker_mod2.RData')
load('analysis_objects/rhat_lmt_mod1.RData')
load('analysis_objects/rhat_lmt_mod2.RData')
load('analysis_objects/rhat_dccs_mod1.RData')
load('analysis_objects/rhat_dccs_mod2.RData')
load('analysis_objects/rhat_pcps_mod1.RData')
load('analysis_objects/rhat_pcps_mod2.RData')

load('analysis_objects/results_sem_training.RData')
load('analysis_objects/results_sem_test.RData')

# 2. Power analysis ----------------------------------------------------------

power_plot <- ggplot(power, aes(n, power, group = lhs, color = Type)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = 90, linetype = 'dashed') +
  facet_wrap(~paste0("\u03B2 = ", beta)) +
  scale_y_continuous(breaks = c(30, 40, 50, 60, 70, 80, 90, 95, 100)) +
  scale_x_continuous(breaks = c(1500, 2500, 3500, 4500, 5500, 6500, 7500, 8500)) +
  scale_color_uchicago() +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  labs(
    x = "\nSample size",
    y = "Power"
  )


# 3. DDM simulations ---------------------------------------------------------

## 3.1 Simulation 1 ------------------------------------------------------------

### Convergence ----

conv_sim1_data <- ddm_sim1_traces |> 
  rename(
    a  = muAlpha,
    t0 = muTau, 
    v  = muDelta
  ) |> 
  pivot_longer(-c(n, chains), names_to = 'parameter', values_to = 'value')

traces_plot_sim1 <- conv_sim1_data |> 
  ggplot(aes(n, value, color = factor(chains))) +
  geom_line() +
  facet_wrap(~parameter, scales = 'free') +
  theme_classic() +
  scale_color_uchicago() +
  labs(
    x = "",
    y = "",
    color = "Chain"
  )

distr_plot_sim1 <- conv_sim1_data |> 
  ggplot(aes(value, fill = parameter)) +
  geom_histogram() +
  facet_wrap(~parameter, scales = 'free') +
  theme_classic() +
  scale_fill_uchicago() +
  labs(
    x = "",
    y = "",
    color = "Chain"
  ) +
  guides(fill = 'none')

### Parameter Recovery ----
recov_plot_sim1 <- ddm_sim1_data |> 
  ggplot(aes(simulated, estimated)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0) +
  facet_wrap(~parameter, scales = 'free') +
  scale_color_uchicago() +
  theme_classic() +
  labs(
    x = "Simulated",
    y = "Recovered"
  )

ddm_sim1_cor <- ddm_sim1_cor |> 
  mutate(
    r = as.character(round(r,2)) |> str_remove(string = _, "^0")) |> 
  pivot_wider(names_from = "parameter", values_from = "r") |> 
  as.list()


## 3.2 Simulation 2 ------------------------------------------------------------

### Convergence ----

conv_sim2_data <- ddm_sim2_traces |> 
  rename(
    a_fixed = muAlpha,
    t0_1    = `muTau[1]`, 
    t0_2    = `muTau[2]`,
    v_1     = `muDelta[1]`,
    v_2     = `muDelta[2]`
  ) |> 
  pivot_longer(-c(n, chains), names_to = 'parameter', values_to = 'value') |> 
  separate(parameter, into = c('parameter', 'condition'), sep = "_")

traces_plot_sim2 <- conv_sim2_data |> 
  unite(col = "parameter", parameter,condition, sep = ", condition ") |> 
  ggplot(aes(n, value, color = factor(chains))) +
  geom_line() +
  facet_wrap(~parameter, scales = 'free') +
  theme_classic() +
  scale_color_uchicago() +
  labs(
    x = "",
    y = "",
    color = "Chain"
  )

distr_plot_sim2 <- conv_sim2_data |> 
  ggplot(aes(value, group = condition, fill = condition)) +
  geom_histogram() +
  facet_wrap(~parameter, scales = 'free') +
  theme_classic() +
  scale_fill_uchicago() +
  labs(
    x = "",
    y = "",
    fill = "Condition"
  ) 

### Parameter Recovery ----

recov_plot_sim2 <- ddm_sim2_data |> 
  mutate(condition = ifelse(is.na(condition), 'fixed', condition)) |> 
  ggplot(aes(simulated, estimated, color = condition)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0) +
  facet_wrap(~parameter, scales = 'free') +
  scale_color_uchicago() +
  theme_classic() +
  labs(
    x = "Simulated",
    y = "Recovered",
    color = "Condition"
  )

ddm_sim2_cor <- ddm_sim2_cor |> 
  mutate(
    r = as.character(round(r,2)) |> str_remove(string = _, "^0")) |> 
  pivot_wider(names_from = "parameter", values_from = "r") |> 
  as.list()



## 3.3 Simulation 3 ------------------------------------------------------------

### Convergence ----

conv_sim3_data <- ddm_sim3_traces |> 
  rename(
    a_fixed = muAlpha,
    t0_1    = `muTau[1]`, 
    t0_2    = `muTau[2]`,
    v_1     = `muDelta[1]`,
    v_2     = `muDelta[2]`
  ) |> 
  pivot_longer(-c(n, chains), names_to = 'parameter', values_to = 'value') |> 
  separate(parameter, into = c('parameter', 'condition'), sep = "_")

traces_plot_sim3 <- conv_sim3_data |> 
  unite(col = "parameter", parameter,condition, sep = ", condition ") |> 
  ggplot(aes(n, value, color = factor(chains))) +
  geom_line() +
  facet_wrap(~parameter, scales = 'free') +
  theme_classic() +
  scale_color_uchicago() +
  labs(
    x = "",
    y = "",
    color = "Chain"
  )

distr_plot_sim3 <- conv_sim3_data |> 
  ggplot(aes(value, group = condition, fill = condition)) +
  geom_histogram() +
  facet_wrap(~parameter, scales = 'free') +
  theme_classic() +
  scale_fill_uchicago() +
  labs(
    x = "",
    y = "",
    fill = "Condition"
  ) 

### Parameter Recovery ----

recov_plot_sim3 <- ddm_sim3_data |> 
  mutate(condition = ifelse(is.na(condition), 'fixed', condition)) |> 
  ggplot(aes(simulated, estimated, color = condition)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0) +
  facet_wrap(~parameter, scales = 'free') +
  scale_color_uchicago() +
  theme_classic() +
  labs(
    x = "Simulated",
    y = "Recovered",
    color = "Condition"
  )

ddm_sim3_cor <- ddm_sim3_cor |> 
  mutate(
    r = as.character(round(r,2)) |> str_remove(string = _, "^0")) |> 
  pivot_wider(names_from = "parameter", values_from = "r") |> 
  as.list()


## 3.4 Simulation 4 ----

shrink_hist_sim4 <- ddm_sim4_data |> 
  mutate(
    parameter = case_when(
      parameter == "v" ~ "Drift rate",
      parameter == "a" ~ "Boundary separation",
      parameter == "t" ~ "Non-decision time"
    ),
    parameter = factor(parameter, levels = c("Drift rate", "Non-decision time", "Boundary separation"))
  ) |> 
  pivot_longer(c(estimated,simulated), names_to = "type", values_to = "value") |> 
  ggplot(aes(value, fill = type, group = type)) +
  geom_histogram(alpha=0.5, position="identity") +
  facet_wrap(~parameter, scales = "free") +
  ggsci::scale_fill_uchicago() +
  theme_classic() +
  labs(
    y = "Frequency",
    x = "Parameter estimate",
    fill = ""
  )

ddm_sim4_cor <- ddm_sim4_data |>
  group_by(parameter) |> 
  summarise(r = cor(estimated, simulated))

dist_sim4 <- ddm_sim4_data |> 
  summarise(
    m_est = mean(estimated),
    m_sim = mean(simulated),
    sd_est = sd(estimated),
    sd_sim = sd(simulated)
  )

deviation_sim4 <- ddm_sim4_data |>
  mutate(
    parameter = case_when(
      parameter == "a" ~ "Boundary separation",
      parameter == "v" ~ "Drift rate",
      parameter == "t" ~ "Non-decision time"
    ),
parameter = factor(parameter, levels = c("Drift rate", "Non-decision time", "Boundary separation")),
adversity2 = adversity^2
) |> 
  ggplot(aes(adversity, deviation)) +
  facet_wrap(~parameter, scales = "free") +
  geom_point() +
  stat_smooth(aes(y = deviation),method = "lm", formula = y ~ x + I(x^2), size = 1) +
  stat_smooth(aes(y = deviation),method = "lm", formula = y ~ x + I(x^2), size = 1, color = "red") +
  geom_hline(yintercept = 0) +
  theme_classic() +
  labs(
    x = "Adversity",
    y = "Simulated - Estimated"
  )

# Is parameter recovery worse at high levels of adversity for each of the three DDM parameters?

mod_dev_v_sim4 <- ddm_sim4_data |> filter(parameter == 'v') |> lm(data = _, deviation ~ adversity) |> summary()
mod_dev_a_sim4 <- ddm_sim4_data |> filter(parameter == 'a') |> lm(data = _, deviation ~ adversity) |> summary()
mod_dev_t_sim4 <- ddm_sim4_data |> filter(parameter == 't') |> lm(data = _, deviation ~ adversity) |> summary()

# If worse recovery is due to a shrinkage effect at the tail, the effect might be quadratic (i.e., a non-linear increase of the deviation at the highest levels of adversity)

mod_dev_v2_sim4 <- ddm_sim4_data |> filter(parameter == 'v') |> mutate(adversity2 = adversity^2) |> lm(data = _, deviation ~ adversity + adversity2) |> summary()
mod_dev_a2_sim4 <- ddm_sim4_data |> filter(parameter == 'a') |> mutate(adversity2 = adversity^2) |> lm(data = _, deviation ~ adversity + adversity2) |> summary()
mod_dev_t2_sim4 <- ddm_sim4_data |> filter(parameter == 't') |> mutate(adversity2 = adversity^2) |> lm(data = _, deviation ~ adversity + adversity2) |> summary()

# Simulated and estimated effects
fit_sim4 <- ddm_sim4_data |> 
  filter(parameter == "v") |> 
  pivot_longer(c(estimated, simulated), names_to = 'type', values_to = 'value') |> 
  mutate(type = ifelse(type == "simulated", 0,1)) |> 
  lmerTest::lmer(data = _, value ~ type*adversity + (1|id)) 
  


ss2_sim4 <- sim_slopes(fit_sim4, pred = type, modx = adversity, modx.values = c(-1, 0, 1)) |> 
  broom.mixed::tidy() |> 
  select(modx.value, p.value) |> 
  mutate(
    p.value = paste0("p = ", formatC(p.value,  digits = 3, width = 3, flag = "0", format = 'f')),
    group = "Recovered",
    x = 0,
    y = c(3.3, 3.18, 3.07))
  
points_sim4 <- 
  ggpredict(fit_sim4, terms = c("adversity [-1,0,1]", "type [0,1]")) |> 
  as_tibble() |> 
  mutate(
    group = ifelse(group == 0, "Simulated", "Recovered"),
    group = factor(group, levels = c('Simulated', "Recovered")),
    x = case_when(
      x == -1 ~ "Low (-1SD)", 
      x == 0 ~ "Average",
      x == 1 ~ "High (+1SD)"),
    x = factor(x, levels = c("Low (-1SD)", "Average", "High (+1SD)"))
  ) |>  
  ggplot() +
  geom_point(aes(group, predicted, color = x, group = x)) +
  geom_line(aes(group, predicted, color = x, group = x)) +
  geom_text(data = ss2_sim4, aes(x = group, y = y, label = p.value), hjust = 2.5) +
  ggsci::scale_color_uchicago() +
  theme_classic() +
  ylim(2.8, 3.6) +
  labs(
    color = "Adversity",
    x = "\nDataset",
    y = "Predicted\n"
  )



## 3.5 Simulation 5 ----

data_compare_sim5 <- 
  list(
  sim_RT5_complete |> 
  group_by(subject) |> 
  summarise(
    rt_sim = mean(RT),
    acc_sim = sum(choice == 1)/n()*100,
    missing_sim = sum(RT > 5)/n()*100
  ) |> 
  ungroup() |> 
  summarise(
    rt_sim = mean(rt_sim),
    acc_sim = mean(acc_sim),
    missing_sim = mean(missing_sim)
  ),
  
  lmt_clean |> 
    group_by(subj_idx) |> 
    summarise(
      rt_lmt = mean(RT, na.rm = T),
      acc_lmt = sum(correct==1)/n()*100,
      missing_lmt = sum(is.na(RT))/n()*100
    ) |> 
    ungroup() |> 
    summarise(
      rt_lmt = mean(rt_lmt),
      acc_lmt = mean(acc_lmt),
      missing_lmt = mean(missing_lmt)
    )
  ) |> 
  reduce(bind_cols) |> 
  mutate(
    across(everything(),
           ~formatC(.,  digits = 2, width = 3, flag = "0", format = 'f'))
  )

cor_sim5 <- ddm_sim5_data |> 
  group_by(parameter) |> 
  summarise(
    r_miss_comp = cor(est_missing, est_complete),
    r_miss_sim = cor(est_missing, simulated),
    r_comp_sim = cor(est_complete, simulated)
    ) |> 
  mutate(
    across(
      starts_with('r'),
      ~ifelse(round(.,3) == 1, 
             formatC(.,  digits = 0, width = 1, flag = "0", format = 'f'),
             formatC(.,  digits = 3, width = 3, flag = "0", format = 'f'))
    )
  )
    

# 4. Descriptives ------------------------------------------------------------

descriptives <- list()

descriptives$task_descriptives_table <- 
  list(pcps_clean, flanker_clean, lmt_clean, dccs_clean) |> 
  map_df(function(x) {
    
    x |> 
      group_by(subj_idx, task) |> 
      summarise(
        rt = mean(RT, na.rm = T),
        acc     = sum(correct)/n() * 100,
      ) |> 
     group_by(task) |> 
      summarise(
        across(
          c(rt, acc),
          list(mean = mean, sd = sd, min = min, max = max)
        )) 
  }) |> 
  select(-c(rt_min, rt_max)) |> 
  mutate(
    task = case_when(
      task == 1 ~ "Mental Rotation",
      task == 2 ~ "Flanker",
      task == 3 ~ "Attention Shifting",
      task == 4 ~ "Processing Speed"
    ),
    mean_rt = str_c(round(rt_mean, 2), " (", round(rt_sd, 2), ")"),
    mean_acc = str_c(round(acc_mean, 2), " (", round(acc_sd, 2), ")"),
    acc_min  = as.character(round(acc_min,2)),
    acc_max  = as.character(round(acc_max,2))
  ) |> 
  select(task, mean_rt, mean_acc, acc_min, acc_max)



# 5. DDM model fit -----------------------------------------------------------

## 5.1 R^ values ----

rhat <- 
  tribble(
    ~task,                           ~rhat,
    "Flanker - Model 1",             max(rhat_flanker_mod1$`Point est.`, na.rm = T),
  #  "Flanker - Model 2",             max(rhat_flanker_mod2$`Point est.`, na.rm = T),
  #  "Mental Rotation - Model 1",     max(rhat_lmt_mod1$`Point est.`, na.rm = T),
  #  "Mental Rotation - Model 2",     max(rhat_lmt_mod2$`Point est.`, na.rm = T),
    "Attention Shifting - Model 1",  max(rhat_dccs_mod1$`Point est.`, na.rm = T),
    "Attention Shifting - Model 2",  max(rhat_dccs_mod2$`Point est.`, na.rm = T),
    "Processing Speed - Model 1",    max(rhat_pcps_mod1$`Point est.`, na.rm = T),
  #  "Processing Speed - Model 2",    max(rhat_pcps_mod2$`Point est.`, na.rm = T),
  ) |> 
  mutate(rhat = formatC(x = rhat, digits = 3, width = 3, flag = "0", format = 'f'))

## 5.2 Fit statistics ----  

ddm_fit_table <- bind_rows(
  flanker_mod1_fit |> 
    group_by(condition, percentile) |> 
    summarise(
      r_RT = cor(RT_sim, RT),
      r_acc = cor(acc_sim, acc)) |> 
    ungroup() |> 
    mutate(task = 'Flanker - Model 1'),
  flanker_mod2_fit |> 
    group_by(percentile) |> 
    summarise(
      r_RT = cor(RT_sim, RT),
      r_acc = cor(acc_sim, acc)) |> 
    ungroup() |> 
    mutate(task = 'Flanker - Model 2'),
  lmt_mod1_fit |> 
    group_by(percentile) |> 
    summarise(
      r_RT = cor(RT_sim, RT),
      r_acc = cor(acc_sim, acc)) |> 
    ungroup() |> 
    mutate(task = 'Mental Rotation - Model 1'),
  lmt_mod2_fit |> 
    group_by(percentile) |> 
    summarise(
      r_RT = cor(RT_sim, RT),
      r_acc = cor(acc_sim, acc)) |> 
    ungroup() |> 
    mutate(task = 'Mental Rotation - Model 2'),
  dccs_mod1_fit |> 
    group_by(condition, percentile) |> 
    summarise(
      r_RT = cor(RT_sim, RT),
      r_acc = cor(acc_sim, acc)) |> 
    ungroup() |> 
    mutate(task = 'Attention Shifting - Model 1'),
  dccs_mod2_fit |> 
    group_by(percentile) |> 
    summarise(
      r_RT = cor(RT_sim, RT),
      r_acc = cor(acc_sim, acc)) |> 
    ungroup() |> 
    mutate(task = 'Attention Shifting - Model 2'),
  pcps_mod1_fit |> 
    group_by(percentile) |> 
    summarise(
      r_RT = cor(RT_sim, RT),
      r_acc = cor(acc_sim, acc)) |> 
    ungroup() |> 
    mutate(task = 'Processing Speed - Model 1'),
  pcps_mod2_fit |> 
    group_by(percentile) |> 
    summarise(
      r_RT = cor(RT_sim, RT),
      r_acc = cor(acc_sim, acc)) |> 
    ungroup() |> 
    mutate(task = 'Processing Speed - Model 2'),
) |> 
  pivot_wider(names_from = 'percentile', values_from = c('r_RT', 'r_acc')) |> 
  select(task, condition, contains("RT_RT"), r_acc_RT_25) %>%
  mutate(across(-c(task, condition), ~formatC(x = ., digits = 2, width = 3, flag = "0", format = 'f'))) |> 
  left_join(rhat) |> 
  flextable::flextable() |> 
  flextable::autofit() |> 
  flextable::set_header_labels(
    task = "Task",
    condition = 'Condition',
    r_RT_RT_25 = "25th Percentile",
    r_RT_RT_50 = "50th Percentile",
    r_RT_RT_75 = "75th Percentile",
    r_acc_RT_25 = "Accuracy",
    rhat        = "R^"
  )

## 5.3 Fit statistics (original) ----

ddm_fit_orig_table <- bind_rows(
  flanker_mod1_fit_orig |> 
    group_by(condition, percentile) |> 
    summarise(
      r_RT = cor(RT_sim, RT),
      r_acc = cor(acc_sim, acc)) |> 
    ungroup() |> 
    mutate(task = 'Flanker - Model 1'),
  flanker_mod2_fit_orig |> 
    group_by(percentile) |> 
    summarise(
      r_RT = cor(RT_sim, RT),
      r_acc = cor(acc_sim, acc)) |> 
    ungroup() |> 
    mutate(task = 'Flanker - Model 2'),
  lmt_mod1_fit_orig |> 
    group_by(percentile) |> 
    summarise(
      r_RT = cor(RT_sim, RT),
      r_acc = cor(acc_sim, acc)) |> 
    ungroup() |> 
    mutate(task = 'Mental Rotation - Model 1'),
  dccs_mod1_fit_orig |> 
    group_by(condition, percentile) |> 
    summarise(
      r_RT = cor(RT_sim, RT),
      r_acc = cor(acc_sim, acc)) |> 
    ungroup() |> 
    mutate(task = 'Attention Shifting - Model 1'),
  dccs_mod2_fit_orig |> 
    group_by(percentile) |> 
    summarise(
      r_RT = cor(RT_sim, RT),
      r_acc = cor(acc_sim, acc)) |> 
    ungroup() |> 
    mutate(task = 'Attention Shifting - Model 2'),
  pcps_mod1_fit_orig |> 
    group_by(percentile) |> 
    summarise(
      r_RT = cor(RT_sim, RT),
      r_acc = cor(acc_sim, acc)) |> 
    ungroup() |> 
    mutate(task = 'Processing Speed - Model 1'),
  pcps_mod2_fit_orig |> 
    group_by(percentile) |> 
    summarise(
      r_RT = cor(RT_sim, RT),
      r_acc = cor(acc_sim, acc)) |> 
    ungroup() |> 
    mutate(task = 'Processing Speed - Model 2'),
) |> 
  pivot_wider(names_from = 'percentile', values_from = c('r_RT', 'r_acc')) |> 
  select(task, condition, contains("RT_RT"), r_acc_RT_25) %>%
  mutate(across(-c(task, condition), ~formatC(x = ., digits = 2, width = 3, flag = "0", format = 'f'))) |> 
  left_join(rhat) |> 
  flextable::flextable() |> 
  flextable::autofit() |> 
  flextable::set_header_labels(
    task = "Task",
    condition = 'Condition',
    r_RT_RT_25 = "25th Percentile",
    r_RT_RT_50 = "50th Percentile",
    r_RT_RT_75 = "75th Percentile",
    r_acc_RT_25 = "Accuracy",
    rhat        = "R^"
  )

## 5.4 Fit statistics at different levels op adversity ----

# Load IVs
mnlfa_ivs     <- readr::read_csv("data/iv_data.csv") |> 
  mutate(
    dep_mnlfa_c   = scale(dep_mnlfa),
    threat_mnlfa_c = scale(threat_mnlfa),
    dep_mnlfa_cat = case_when(
      dep_mnlfa_c < -1 ~ "< -1SD",
      dep_mnlfa_c > 1 ~ "> 1SD",
      dep_mnlfa_c > -1 & dep_mnlfa_c < 1 ~ "≥1*SD*≤"
    ),
    threat_mnlfa_cat = case_when(
      threat_mnlfa_c < -1 ~ "< -1SD",
      threat_mnlfa_c > 1 ~ "> 1SD",
      threat_mnlfa_c > -1 & threat_mnlfa_c < 1 ~ "≥1*SD*≤"
    ),
    across(c(dep_mnlfa_c, threat_mnlfa_c), as.numeric)
  )

ddm_fit_subgroup_threat_table <- bind_rows(
  flanker_mod2_fit |> 
    left_join(mnlfa_ivs) |> 
    group_by(percentile, threat_mnlfa_cat) |> 
    summarise(
      r_RT = cor(RT_sim, RT),
      r_acc = cor(acc_sim, acc)) |> 
    ungroup() |> 
    mutate(task = 'Flanker - Model 2'),
  lmt_mod1_fit |> 
    left_join(mnlfa_ivs) |> 
    group_by(percentile, threat_mnlfa_cat) |> 
    summarise(
      r_RT = cor(RT_sim, RT),
      r_acc = cor(acc_sim, acc)) |> 
    ungroup() |> 
    mutate(task = 'Mental Rotation - Model 1'),
  dccs_mod2_fit |> 
    left_join(mnlfa_ivs) |> 
    group_by(percentile, threat_mnlfa_cat) |> 
    summarise(
      r_RT = cor(RT_sim, RT),
      r_acc = cor(acc_sim, acc)) |> 
    ungroup() |> 
    mutate(task = 'Attention Shifting - Model 2'),
  pcps_mod2_fit |> 
    left_join(mnlfa_ivs) |> 
    group_by(percentile, threat_mnlfa_cat) |> 
    summarise(
      r_RT = cor(RT_sim, RT),
      r_acc = cor(acc_sim, acc)) |> 
    ungroup() |> 
    mutate(task = 'Processing Speed - Model 2'),
) |> 
  pivot_wider(names_from = 'percentile', values_from = c('r_RT', 'r_acc')) |> 
  select(task, threat_mnlfa_cat, contains("RT_RT"), r_acc_RT_25) %>%
  mutate(across(-c(task), ~formatC(x = ., digits = 2, width = 3, flag = "0", format = 'f'))) |> 
  filter(!str_detect(threat_mnlfa_cat, "NA")) |> 
  flextable::flextable() |> 
  flextable::autofit() |> 
  flextable::set_header_labels(
    task = "Task",
    threat_mnlfa_cat = "Household threat",
    r_RT_RT_25 = "25th Percentile",
    r_RT_RT_50 = "50th Percentile",
    r_RT_RT_75 = "75th Percentile",
    r_acc_RT_25 = "Accuracy",
    rhat        = "R^"
  )

ddm_fit_subgroup_dep_table <- bind_rows(
  flanker_mod2_fit |> 
    left_join(mnlfa_ivs) |> 
    group_by(percentile, dep_mnlfa_cat) |> 
    summarise(
      r_RT = cor(RT_sim, RT),
      r_acc = cor(acc_sim, acc)) |> 
    ungroup() |> 
    mutate(task = 'Flanker - Model 2'),
  lmt_mod1_fit |> 
    left_join(mnlfa_ivs) |> 
    group_by(percentile, dep_mnlfa_cat) |> 
    summarise(
      r_RT = cor(RT_sim, RT),
      r_acc = cor(acc_sim, acc)) |> 
    ungroup() |> 
    mutate(task = 'Mental Rotation - Model 1'),
  dccs_mod2_fit |> 
    left_join(mnlfa_ivs) |> 
    group_by(percentile, dep_mnlfa_cat) |> 
    summarise(
      r_RT = cor(RT_sim, RT),
      r_acc = cor(acc_sim, acc)) |> 
    ungroup() |> 
    mutate(task = 'Attention Shifting - Model 2'),
  pcps_mod2_fit |> 
    left_join(mnlfa_ivs) |> 
    group_by(percentile, dep_mnlfa_cat) |> 
    summarise(
      r_RT = cor(RT_sim, RT),
      r_acc = cor(acc_sim, acc)) |> 
    ungroup() |> 
    mutate(task = 'Processing Speed - Model 2'),
) |> 
  pivot_wider(names_from = 'percentile', values_from = c('r_RT', 'r_acc')) |> 
  select(task, dep_mnlfa_cat, contains("RT_RT"), r_acc_RT_25) %>%
  mutate(across(-c(task), ~formatC(x = ., digits = 2, width = 3, flag = "0", format = 'f'))) |> 
  filter(!str_detect(dep_mnlfa_cat, "NA")) |> 
  flextable::flextable() |> 
  flextable::autofit() |> 
  flextable::set_header_labels(
    task = "Task",
    dep_mnlfa_cat = "Material deprivation",
    r_RT_RT_25 = "25th Percentile",
    r_RT_RT_50 = "50th Percentile",
    r_RT_RT_75 = "75th Percentile",
    r_acc_RT_25 = "Accuracy",
    rhat        = "R^"
  )



## 5.5 Convergence Figures ----

ddm_conv_lmt_fig <- 
  lmt_mod1_traces |> 
  mutate(
    parameter = case_when(
      parameter == "a" ~ "Boundary separation",
      str_detect(parameter, "v") ~ "Drift rate",
      str_detect(parameter, "t0") ~ "Non-decision time"
    )
  ) |>  
  ggplot(aes(n, value, group = n, color = factor(chains))) +
  geom_line() +
  facet_wrap(~parameter, scales = 'free') + 
  theme_classic() +
  scale_color_uchicago() +
  labs(
    x = "",
    y = "",
    color = "Chain"
  ) +
  guides(fill = 'none')

ddm_conv_flanker_fig <- 
  bind_rows(
    flanker_mod1_traces |> mutate(model = "Model 1"),
    flanker_mod2_traces |> mutate(model = "Model 2")
  ) |> 
  mutate(
   parameter = case_when(
      parameter == "t01" ~ "t (con)",
      parameter == "t02" ~ "t (incon)",
      parameter == "t0" ~  "t",
      parameter == "v1" ~ "v (con)",
      parameter == "v2" ~ "v (incon)",
      parameter == "v" ~ "v",
      parameter == "a" ~ "a"
    )) |> 
  unite(col = "parameter", c(model, parameter), sep = ": ") |> 
  ggplot(aes(n, value, color = factor(chains))) +
  geom_line() +
  facet_wrap(~parameter, scales = 'free', ncol=4) +
  theme_classic() +
  scale_color_uchicago() +
  labs(
    x = "",
    y = "",
    color = "Chain"
  )

ddm_conv_dccs_fig <- 
  bind_rows(
    dccs_mod1_traces |> mutate(model = "Model 1"),
    dccs_mod2_traces |> mutate(model = "Model 2")
  ) |> 
  mutate(
    parameter = case_when(
      parameter == "t01" ~ "t (rep)",
      parameter == "t02" ~ "t (sw)",
      parameter == "t0" ~  "t",
      parameter == "v1" ~ "v (rep)",
      parameter == "v2" ~ "v (sw)",
      parameter == "v" ~ "v",
      parameter == "a" ~ "a"
    )) |> 
  unite(col = "parameter", c(model, parameter), sep = ": ") |> 
  ggplot(aes(n, value, color = factor(chains))) +
  geom_line() +
  facet_wrap(~parameter, scales = 'free', ncol=4) +
  theme_classic() +
  scale_color_uchicago() +
  labs(
    x = "",
    y = "",
    color = "Chain"
  )

ddm_conv_pcps_fig <- 
  bind_rows(
    pcps_mod1_traces |> mutate(model = "Model 1"),
    pcps_mod2_traces |> mutate(model = "Model 2")
  ) |> 
  mutate(
    parameter = case_when(
      parameter == "t0" ~  "t",
      parameter == "v" ~ "v",
      parameter == "a" ~ "a"
    )) |> 
  unite(col = "parameter", c(model, parameter), sep = ": ") |> 
  ggplot(aes(n, value, color = factor(chains))) +
  geom_line() +
  facet_wrap(~parameter, scales = 'free', ncol=3) +
  theme_classic() +
  scale_color_uchicago() +
  labs(
    x = "",
    y = "",
    color = "Chain"
  )



# 6. SEM fit --------------------------------------------------------------

## 6.1 Training set ----

summary(training_sem_sub_a_cluster, fit.measures = TRUE, standardized = TRUE)

## 6.2 Test set ----

test_factor_loadings |> 
  select(lhs, rhs, est, se, z, pvalue, std.all) |> 
  bind_rows(
    parameterEstimates(test_sem_full_cluster) |> 
      filter(op == "~~", str_detect(lhs, "_l$"), lhs == rhs) |> 
      select(lhs, rhs, est, se, z, pvalue)
  ) |> 
  as_tibble() |> 
  mutate(
    rhs = case_when(
      str_detect(rhs, "pcps_(v|a|t)") ~ "Processing Speed",
      str_detect(rhs, "dccs_(v|a|t)") ~ "Attention Shifting",
      str_detect(rhs, "flanker_(v|a|t)") ~ "Flanker",
      str_detect(rhs, "rotation_(v|a|t)") ~ "Mental Rotation",
    )
  ) |>
  add_row(.before = 1, rhs = "Factor loadings") |> 
  add_row(.before = 2, rhs = "Task-general drift rate") |> 
  add_row(.before = 6, rhs = "Task-general boundary separation") |> 
  add_row(.before = 11, rhs = "Task-general non-decision time") |> 
  add_row(.before = 16, rhs = "Residual variances") |> 
  add_row(.before = 17, rhs = "Task-specific drift rate") |> 
  add_row(.before = 21, rhs = "Task-specific boundary separation") |> 
  add_row(.before = 26, rhs = "Task-specific non-decision time") |> 
  select(-lhs) |> 
  mutate(
    z = as.numeric(z),
    std.all = as.numeric(std.all)) |> 
  mutate(across(c(est, se, z, std.all), ~formatC(x = ., digits = 2, width = 3, flag = "0", format = 'f'))) |> 
  mutate(across(c(pvalue), ~formatC(x = ., digits = 3, width = 3, flag = "0", format = 'f'))) |> 
  mutate(pvalue = ifelse(str_trim(pvalue) == "0", "<.001", pvalue)) |> 
  mutate(across(everything(), ~ifelse(str_detect(., " NA"), "", .))) |> 
  flextable::flextable() |> 
  flextable::autofit() |> 
  flextable::bold(i = c(1,2,  6, 11, 16, 17, 21, 26)) |> 
  flextable::merge_h(i = 1:6)
  
# 7. Stage all results -------------------------------------------------------

staged_sim_results_supp <- 
  list(
    power_plot                 = power_plot,
    
    ddm_sim1_convergence       = conv_sim1_data,
    ddm_sim1_traces_plot       = traces_plot_sim1,
    ddm_sim1_distr_plot        = distr_plot_sim1,
    ddm_sim1_cor               = ddm_sim1_cor,
    ddm_sim1_recov             = recov_plot_sim1,
    
    ddm_sim2_convergence       = conv_sim2_data,
    ddm_sim2_traces_plot       = traces_plot_sim2,
    ddm_sim2_distr_plot        = distr_plot_sim2,
    ddm_sim2_cor               = ddm_sim2_cor,
    ddm_sim2_recov             = recov_plot_sim2,
    
    ddm_sim3_convergence       = conv_sim3_data,
    ddm_sim3_traces_plot       = traces_plot_sim3,
    ddm_sim3_distr_plot        = distr_plot_sim3,
    ddm_sim3_cor               = ddm_sim3_cor,
    ddm_sim3_recov             = recov_plot_sim3,
    
    ddm_sim4_cor               = ddm_sim4_cor,
    ddm_sim4_hist              = shrink_hist_sim4,
    ddm_sim4_dist              = dist_sim4,
    ddm_sim4_dev_plot          = deviation_sim4,
    ddm_sim4_mod_dev_v         = mod_dev_v_sim4,
    ddm_sim4_mod_dev_a         = mod_dev_a_sim4,
    ddm_sim4_mod_dev_t         = mod_dev_t_sim4,
    ddm_sim4_mod_dev_v2        = mod_dev_v2_sim4,
    ddm_sim4_mod_dev_t2        = mod_dev_t2_sim4,
    ddm_sim4_mod_dev_a2        = mod_dev_a2_sim4,
    ddm_sim4_fit               = fit_sim4, 
    ddm_sim4_simslopes         = ss2_sim4, 
    ddm_sim4_points_plot       = points_sim4,
    
    ddm_sim5_cor               = cor_sim5,
    ddm_sim5_data_compare      = data_compare_sim5,
    
    descriptives               = descriptives$task_descriptives_table,
    
    ddm_fit_table              = ddm_fit_table,
    
    ddm_fit_subgroup_dep_table = ddm_fit_subgroup_dep_table,
    ddm_fit_subgroup_dep_table = ddm_fit_subgroup_dep_table,
    
    ddm_conv_flanker_fig       = ddm_conv_flanker_fig,
    ddm_conv_dccs_fig          = ddm_conv_dccs_fig,
    ddm_conv_lmt_fig           = ddm_conv_lmt_fig,
    ddm_conv_pcps_fig          = ddm_conv_pcps_fig
  )

save(staged_sim_results_supp, file = 'supplement/staged_sim_results_supp.RData')

