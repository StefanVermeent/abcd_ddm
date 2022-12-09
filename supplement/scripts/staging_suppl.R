

# Load objects ------------------------------------------------------------
load('synthetic_data/power.RData')
load('synthetic_data/sim_DDM_6trials.RData')
load('synthetic_data/ddm_sim1_results.RData')
load('synthetic_data/ddm_sim2_results.RData')

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

traces_plot_sim2 <- conv_sim2_data |> 
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
  geom_point(shape = 1) +
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


# Stage all results -------------------------------------------------------

staged_sim_results_supp <- 
  list(
    power_plot = power_plot,
    
   # ddm_sim1_convergence = conv_sim1_data,
   # ddm_sim1_traces_plot = traces_plot_sim1,
   # ddm_sim1_distr_plot  = distr_plot_sim1,
   # ddm_sim1_cor         = ddm_sim1_cor,
   # ddm_sim1_recov       = recov_plot_sim2,
    
    ddm_sim2_convergence = conv_sim2_data,
    ddm_sim2_traces_plot = traces_plot_sim2,
    ddm_sim2_distr_plot  = distr_plot_sim2,
    ddm_sim2_cor         = ddm_sim2_cor,
    ddm_sim2_recov       = recov_plot_sim2
  )

save(staged_sim_results_supp, file = 'synthetic_data/staged_sim_results_supp.RData')

