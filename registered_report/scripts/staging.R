data_folder <- 'closed_data'

load('data/tasks_raw.RData')

lmt_clean     <- readr::read_csv(paste0("data/lmt_clean", data_suffix, ".csv"))
flanker_clean <- readr::read_csv(paste0("data/flanker_clean", data_suffix, ".csv"))
pcps_clean    <- readr::read_csv(paste0("data/pcps_clean", data_suffix, ".csv"))
dccs_clean    <- readr::read_csv(paste0("data/dccs_clean", data_suffix, ".csv"))



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


save(
  flanker_clean,
  dccs_clean,
  lmt_clean,
  pcps_clean,
  descriptives,
  exclusions,
  file = "registered_report/staged_results.RData"
)
