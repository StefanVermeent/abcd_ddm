source('scripts/custom_functions/corr_plot.R')


load('data/tasks_raw.RData')
load("analysis_objects/descriptives.RData")
load("analysis_objects/exclusions.RData")

lmt_clean     <- readr::read_csv(paste0("data/lmt_clean", data_suffix, ".csv"))
flanker_clean <- readr::read_csv(paste0("data/flanker_clean", data_suffix, ".csv"))
pcps_clean    <- readr::read_csv(paste0("data/pcps_clean", data_suffix, ".csv"))
dccs_clean    <- readr::read_csv(paste0("data/dccs_clean", data_suffix, ".csv"))

ddm_data      <- readr::read_csv('data/ddm_data.csv')

load("analysis_objects/results_sem_training.RData")
load("analysis_objects/results_sem_test.RData")

# set up flextable for tables
set_flextable_defaults(
  font.family = "Times", 
  font.size = 10,
  font.color = "black",
  line_spacing = 1,
  padding.bottom = 1, 
  padding.top = 1,
  padding.left = 1,
  padding.right = 1
)

# Task-descriptives -------------------------------------------------------

get_descriptives <- function(data, conditions = FALSE) {
  
  if(!conditions) {
    return(
      data |> 
        group_by(subj_idx) |> 
        summarise(
          mean_RT = mean(RT, na.rm = T),
          sd_RT   = sd(RT, na.rm = T),
          acc     = sum(correct == 1) / n() * 100
        ) |> 
        ungroup() |> 
        summarise(
          mean_rt  = round(mean(mean_RT, na.rm = T), 2),
          sd_rt    = round(sd(mean_RT, na.rm = T), 2),
          mean_acc = round(mean(acc, na.rm = T), 2),
          sd_acc   = round(sd(acc, na.rm = T), 2)
        ) |> 
        as.list()
    )
  }
  
  if(conditions) {
    return(
      data |> 
        group_by(subj_idx, condition) |> 
        summarise(
          mean_RT = mean(RT, na.rm = T),
          sd_RT   = sd(RT, na.rm = T),
          acc     = sum(correct == 1) / n() * 100
        ) |> 
        group_by(condition) |> 
        summarise(
          mean_rt  = round(mean(mean_RT, na.rm = T), 2),
          sd_rt    = round(sd(mean_RT, na.rm = T), 2),
          mean_acc = round(mean(acc, na.rm = T), 2),
          sd_acc   = round(sd(acc, na.rm = T), 2)
        ) |> 
        pivot_longer(c(mean_rt, sd_rt, mean_acc, sd_acc), names_to = 'stat', values_to = 'value') |> 
        mutate(stat = paste0(stat, "_", condition)) |> 
        select(-condition) |> 
        pivot_wider(names_from = 'stat', values_from = 'value') |> 
        as.list()
    )
  }
}

descriptives$lmt <- 
  get_descriptives(lmt_clean)

descriptives$flanker <- 
  get_descriptives(flanker_clean, conditions = TRUE)

descriptives$pcps <- 
  get_descriptives(pcps_clean)

descriptives$dccs <- 
  get_descriptives(dccs_clean, conditions = TRUE)


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


# Results -----------------------------------------------------------------

## Training set ----

sem_fit_measures <- bind_rows(
  fitmeasures(training_sem_sub_v_cluster)[c('cfi', 'rmsea')] |> enframe() |> pivot_wider(names_from = 'name', values_from = 'value') |> mutate(model = "Training - Drift rate measurement model"), 
  fitmeasures(training_sem_sub_a_cluster)[c('cfi', 'rmsea')] |> enframe() |> pivot_wider(names_from = 'name', values_from = 'value') |> mutate(model = "Training - Boundary separation measurement model"), 
  fitmeasures(training_sem_sub_t_cluster)[c('cfi', 'rmsea')] |> enframe() |> pivot_wider(names_from = 'name', values_from = 'value') |> mutate(model = "Training - Non-decision time measurement model"), 
  fitmeasures(training_sem_sub_combn_cluster)[c('cfi', 'rmsea')] |> enframe() |> pivot_wider(names_from = 'name', values_from = 'value') |> mutate(model = "Training - Combined measurement model"), 
  fitmeasures(training_sem_full_cluster)[c('cfi', 'rmsea')] |> enframe() |> pivot_wider(names_from = 'name', values_from = 'value') |> mutate(model = "Training - Full model"),
  fitmeasures(test_sem_full_cluster)[c('cfi', 'rmsea')] |> enframe() |> pivot_wider(names_from = 'name', values_from = 'value') |> mutate(model = "Test - Full model")
)

## SEM model fit ----

table1 <- sem_fit_measures |> 
  select(Model = model, CFI = cfi, RMSEA = rmsea) %>%
  mutate(across(c(CFI, RMSEA), ~formatC(.,  digits = 3, width = 3, flag = "0", format = 'f'))) |> 
  flextable() |> 
  autofit() |> 
  border(i = 1, border.top = fp_border_default(style = "none", width = 0), part = "header") |> 
  add_header_row(
    values = " ",
    colwidths = 3
  ) |> 
  compose(
    i = 1, j = 1, 
    as_paragraph(as_b("Table 1. "), "SEM fit statistics for the training and test set."),
    part = "header"
  ) 

## Correlations between DDM parameters ----

table2 <- ddm_data |>
  left_join(iv_data |> select(subj_idx, dep_mnlfa, threat_mnlfa)) |> 
  select(
    flanker_v, dccs_v, rotation_v, pcps_v,
    flanker_a, dccs_a, rotation_a, pcps_a,
    flanker_t, dccs_t, rotation_t, pcps_t,
    dep_mnlfa, threat_mnlfa
  ) |> 
  corr_table(
    numbered = T,
    sample_size = F,
    c.names = c("Fl.", "Att. Sh.", "Men. Rot.", "Proc. Sp.",
                "Fl.", "Att. Sh.", "Men. Rot.", "Proc. Sp.",
                "Fl.", "Att. Sh.", "Men. Rot.", "Proc. Sp.",
                "Mat. Dep.", "Househ. Thr."
                ),
    stats = c("mean", "sd", "skew", "kurtosis")
  ) |> 
  add_row(.before = 1, Variable = "Drift Rate") |> 
  add_row(.after = 5, Variable = "Boundary Separation") |> 
  add_row(.after = 10, Variable = "Non-Decision Time") |>
  add_row(.after = 15, Variable = "Adversity") |> 
  flextable::flextable() |> 
  set_header_labels(Variable = "") |> 
  bold(i = c(1, 6, 11, 16)) |>
  border(i = 18, border.bottom = fp_border_default(), part = "body") |>
  border(i = 1, border.top = fp_border_default(style = "none", width = 0), part = "header") |> 
  add_header_row(
    values = " ",
    colwidths = 15
  ) |> 
  compose(
    i = 1, j = 1, 
    as_paragraph(as_b("Table 2. "), "Bivariate correlations between DDM parameters and measures of adversity."),
    part = "header"
  ) |>
  add_footer_row(
    values = " ",
    colwidths = 15
  ) |> 
  add_footer_row(
    values = " ",
    colwidths = 15
  ) |> 
  compose(
    i = 1, j = 1, 
    as_paragraph(as_i("Note: "), "Fl. = Flanker; Att. Sh. = Attention Shifting; Men. Rot. = Mental Rotation; Proc. Sp. = Processing Speed; Mat. Dep. = Material Deprivation; Househ. Thr. = Household Threat\n", as_i("* p "), "< .05, ", as_i("** p "), "< .01"), 
    part = "footer"
  ) |> 
  width(width = c(1.2,rep(0.5, 14)))



## Test set

test_reg_coef_list <- test_reg_coef |> 
  mutate(par_combi = paste0(lhs, "_", rhs)) |> 
  mutate(
    across(
      c(est.std, se, ci.lower, ci.upper), 
      ~formatC(.,  digits = 2, width = 3, flag = "0", format = 'f')
    ),
    pvalue_adj_chr = ifelse(pvalue_adj < .001, "<.001", 
                        formatC(pvalue_adj,  digits = 3, width = 3, flag = "0", format = 'f') |> str_remove("^0"))
  ) 

test_reg_coef_keys <- test_reg_coef_list |> group_keys(par_combi) |> pull()

test_reg_coef_list <- test_reg_coef_list |> 
  group_split(par_combi) |> 
  setNames(test_reg_coef_keys)

fig4 <- test_reg_coef |> 
  left_join(equivalence_tests |> select(lhs, rhs, eq_pvalue)) |> 
  ungroup() |> 
  add_row(
    lhs = "Filler1", 
    ddm_parameter = c("Drift rate","Drift rate", "Boundary separation","Boundary separation", "Non-decision time","Non-decision time"),
    rhs = c("dep_mnlfa","dep_mnlfa","dep_mnlfa", "threat_mnlfa", "threat_mnlfa", "threat_mnlfa"),
    ) |> 
  add_row(lhs = "Filler2", 
          ddm_parameter = c("Drift rate","Drift rate", "Boundary separation","Boundary separation", "Non-decision time","Non-decision time"),
          rhs = c("dep_mnlfa","dep_mnlfa","dep_mnlfa", "threat_mnlfa", "threat_mnlfa", "threat_mnlfa"),
  ) |> 
  mutate(
   eq_pvalue_discr = ifelse(eq_pvalue < .05, "eq", "non-eq"),
   lhs = str_remove(lhs, "^v_|^a_|^t_|(_(v|a|t)_l$)") |> str_to_title(),
   lhs = case_when(
     lhs == "General" ~ "Task-general",
     lhs == "Rotation" ~ "Mental Rotation",
     lhs == "Dccs" ~ "Attention Shifting",
     .default = lhs
   ),
   rhs = ifelse(rhs == "dep_mnlfa", "Material deprivation", "Household threat"),
   lhs = factor(lhs, levels = c("Filler1", "Task-general", "Flanker", "Attention Shifting", "Mental Rotation", "Filler2")),
   ddm_parameter = factor(ddm_parameter, levels = c("Drift rate", "Boundary separation", "Non-decision time")),
   dot_size = ifelse(lhs == "General", 8, 4),
   xmin = "Filler1",
   xmax = "Filler2",
   sig_star = case_when(
     pvalue_adj >= 0.05 ~ "",
     pvalue_adj < 0.05 & pvalue_adj >= 0.01 ~ "*",
     pvalue_adj < 0.01 & pvalue_adj >= 0.001 ~  "**",
     pvalue_adj < 0.001 ~ "***"
   ),
   sig_pos = ifelse(est.std < 0, ci.lower - 0.03, ci.upper + 0.01)
    ) |> 
  ggplot(aes(x = lhs, y = est.std)) +
  geom_rect(
    aes(
      xmin = xmin, xmax = xmax, ymin = -0.1, ymax = 0.1
    ),
    fill = "#F2F3F5",
    inherit.aes = T
  ) +
  geom_hline(aes(yintercept = 0), size = 0.7) +
  geom_vline(aes(xintercept = 2.5), linetype = "dashed") +
  geom_errorbar(aes(ymin = ci.lower, ymax = ci.upper), width = 0.2, size = 1) +
  geom_point(aes(color = lhs, shape = eq_pvalue_discr), fill = "white", size = 3, stroke = 1.5) +
  geom_text(
    aes(y = sig_pos, label = sig_star),
    color = "black"
  ) +
  facet_grid(ddm_parameter~rhs) +
  scale_shape_manual(values = c(16,21)) +
  coord_cartesian(xlim = c(2,5)) +
  scale_x_discrete(labels = c("", "Task-general", "Flanker", "Attention Shifting", "Mental Rotation", "")) +
  ggsci::scale_color_uchicago() +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.ticks.x = element_blank(),
    axis.text.y = element_text(size = 12),
    strip.background = element_rect(color = 'white'),
    strip.text.y = element_text(face = "bold", size = 9.5),
    strip.text.x = element_text(face = "bold", size = 13)
  ) +
  guides(color = 'none', shape = 'none', size = 'none') +
  labs(
    x = "",
    y = "Standardized regression coefficient"
  )
 

  



save(
  flanker_clean,
  dccs_clean,
  lmt_clean,
  pcps_clean,
  descriptives,
  exclusions,
  
  test_reg_coef_list,
  fig4,
  table1,
  table2,
  file = "registered_report/staged_results.RData"
)
