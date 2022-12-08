load(glue('{data_folder}/tasks_raw.RData'))
load(glue('{data_folder}/tasks_clean.RData'))


# Sample Size Information -------------------------------------------------

# Sample size information for manuscript
descriptives$precleaning_n <- 
  list(
    lmt     = lmt_raw |> pull(subj_idx) |> unique() |> length(),
    flanker = flanker_raw |> pull(subj_idx) |> unique() |> length(),
    dccs    = dccs_raw |> pull(subj_idx) |> unique() |> length(),
    pcps    = pcps_raw |> pull(subj_idx) |> unique() |> length(),
    # picvoc  = picvoc_raw |> pull(subj_idx) |> unique() |> length(),
    
    # N participants with summary score cognitive data
    full_n_nihtb = nih_ref_ids |> length(),
    
    # N participants with trial-level cognitive data
    full_n_trial = 
      unique(
        c(
          lmt_raw |> pull(subj_idx) |> unique(),
          flanker_raw |> pull(subj_idx) |> unique(),
          dccs_raw |> pull(subj_idx) |> unique(),
          pcps_raw |> pull(subj_idx) |> unique()
          # picvoc_raw |> pull(subj_idx) |> unique()
        )
      )  |> length()
  )

descriptives$precleaning_n <- descriptives$precleaning_n |> 
  map(function(x) prettyNum(x, big.mark = ","))



descriptives$postcleaning_n <- 
  list(
    lmt     = lmt_clean |> pull(subj_idx) |> unique() |> length(),
    flanker = flanker_clean |> pull(subj_idx) |> unique() |> length(),
    dccs    = dccs_clean |> pull(subj_idx) |> unique() |> length(),
    pcps    = pcps_clean |> pull(subj_idx) |> unique() |> length(),
    
    # N participants with trial-level cognitive data
    full_n_trial = 
      unique(
        c(
          lmt_clean |> pull(subj_idx) |> unique(),
          flanker_clean |> pull(subj_idx) |> unique(),
          dccs_clean |> pull(subj_idx) |> unique(),
          pcps_clean |> pull(subj_idx) |> unique()
        )
      )  |> length(),
    
    # N participants with trial-level cognitive data on all tasks
    shared_n = 
      Reduce(intersect, 
             list(
               lmt_clean |> pull(subj_idx) |> unique(),
               flanker_clean |> pull(subj_idx) |> unique(),
               dccs_clean |> pull(subj_idx) |> unique(),
               pcps_clean|> pull(subj_idx) |> unique()
               
             )) |> length()
  )

descriptives$postcleaning_n <- descriptives$postcleaning_n |> 
  map(function(x) prettyNum(x, big.mark = ","))

# Subjects that have data available on all tasks
shared_ids <- 
  Reduce(intersect, 
         list(
           lmt_clean |> pull(subj_idx) |> unique(),
           flanker_clean |> pull(subj_idx) |> unique(),
           dccs_clean |> pull(subj_idx) |> unique(),
           pcps_clean |> pull(subj_idx) |> unique()
         ))


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