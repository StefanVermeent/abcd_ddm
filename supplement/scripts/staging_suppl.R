

# Load objects ------------------------------------------------------------
lmt_clean     <- readr::read_csv(paste0("data/lmt_clean", data_suffix, ".csv"))
flanker_clean <- readr::read_csv(paste0("data/flanker_clean", data_suffix, ".csv"))
pcps_clean    <- readr::read_csv(paste0("data/pcps_clean", data_suffix, ".csv"))
dccs_clean    <- readr::read_csv(paste0("data/dccs_clean", data_suffix, ".csv"))
load('analysis_objects/power.RData')
load('analysis_objects/ddm_sim1_results.RData')
load('analysis_objects/ddm_sim2_results.RData')
load('analysis_objects/ddm_sim3_results.RData')

# Power analysis ----------------------------------------------------------

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


# DDM simulations ---------------------------------------------------------

## Results of simulations with 6 trials ----
  

# Simulation 1 ------------------------------------------------------------

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

## Parameter Recovery ----

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


# Simulation 2 ------------------------------------------------------------

## Convergence ----

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

## Parameter Recovery ----

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



# Simulation 3 ------------------------------------------------------------

## Convergence ----

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

## Parameter Recovery ----

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

# Descriptives ------------------------------------------------------------

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



# Stage all results -------------------------------------------------------

staged_sim_results_supp <- 
  list(
    power_plot = power_plot,
    
    ddm_sim1_convergence = conv_sim1_data,
    ddm_sim1_traces_plot = traces_plot_sim1,
    ddm_sim1_distr_plot  = distr_plot_sim1,
    ddm_sim1_cor         = ddm_sim1_cor,
    ddm_sim1_recov       = recov_plot_sim1,
    
    ddm_sim2_convergence = conv_sim2_data,
    ddm_sim2_traces_plot = traces_plot_sim2,
    ddm_sim2_distr_plot  = distr_plot_sim2,
    ddm_sim2_cor         = ddm_sim2_cor,
    ddm_sim2_recov       = recov_plot_sim2,
    
    ddm_sim3_convergence = conv_sim3_data,
    ddm_sim3_traces_plot = traces_plot_sim3,
    ddm_sim3_distr_plot  = distr_plot_sim3,
    ddm_sim3_cor         = ddm_sim3_cor,
    ddm_sim3_recov       = recov_plot_sim3,
    
    descriptives         = descriptives$task_descriptives_table
  )

save(staged_sim_results_supp, file = 'supplement/staged_sim_results_supp.RData')

