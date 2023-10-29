
extract_traces <- function(mcmc, chains = 3, iterations = 12000) {
  
  mcmc |> 
    select(starts_with("mu")) |> 
    mutate(
      n = rep(1:iterations, chains),
      chains = rep(1:chains, each = iterations))  |> 
    rename(
      a  = muAlpha,
      t0 = muTau, 
      v  = muDelta
    ) |> 
    pivot_longer(-c(n, chains), names_to = 'parameter', values_to = 'value')
}



extract_traces_2con <- function(mcmc, chains = 3, iterations = 1000) {
  
  mcmc |> 
    select(starts_with("mu")) |> 
    mutate(
      n = rep(1:iterations, chains),
      chains = rep(1:chains, each = iterations))  |> 
    rename(
      a   = muAlpha,
      t01 = `muTau[1]`, 
      t02 = `muTau[2]`,
      v1  = `muDelta[1]`,
      v2  = `muDelta[2]`
    ) |> 
    pivot_longer(-c(n, chains), names_to = 'parameter', values_to = 'value')
}

extract_traces_2con_expl <- function(mcmc, chains = 3, iterations = 1000) {
  
  mcmc |> 
    select(starts_with("mu")) |> 
    mutate(
      n = rep(1:iterations, chains),
      chains = rep(1:chains, each = iterations))  |> 
    rename(
      a1  = `muAlpha[1]`,
      a2  = `muAlpha[2]`,
      t01  = `muTau[1]`,
      t02 = `muTau[2]`,
      v1  = `muDelta[1]`,
      v2  = `muDelta[2]`
    ) |> 
    pivot_longer(-c(n, chains), names_to = 'parameter', values_to = 'value')
}


extract_ddm_estimates <- function(mcmc, task_prefix, id_matches) {
  
  mcmc |> 
    pivot_longer(everything(), names_to = "parameter", values_to = "estimated") |> 
    group_by(parameter) |> 
    summarise(estimated = mean(estimated, na.rm = T)) |> 
    filter(str_detect(parameter, pattern = 'deviance|^mu|^prec', negate = T)) |> 
    separate(col = parameter, into = c('parameter', 'subj_idx_num'), sep = "\\[") |> 
    mutate(
      subj_idx_num = str_remove(subj_idx_num, pattern = "\\]$")
    ) |>  
    mutate(
      subj_idx_num = as.numeric(subj_idx_num),
      parameter = case_when(
        parameter == 'alpha' ~ paste0(task_prefix, 'a'),
        parameter == 'tau' ~ paste0(task_prefix, 't'),
        parameter == 'delta' ~ paste0(task_prefix, 'v')
      )) |> 
    pivot_wider(names_from = "parameter", values_from = "estimated") |> 
    left_join(id_matches) |> 
    select(-subj_idx_num)
}

extract_ddm_estimates_2con <- function(mcmc, task_prefix, id_matches) {
  
  mcmc |> 
    pivot_longer(everything(), names_to = "parameter", values_to = "estimated") |> 
    group_by(parameter) |> 
    summarise(estimated = mean(estimated, na.rm = T)) |> 
    filter(str_detect(parameter, pattern = 'deviance|^mu|^prec', negate = T)) |> 
    separate(col = parameter, into = c('parameter', 'subj_idx_num'), sep = "\\[") |> 
    mutate(
      subj_idx_num = str_remove(subj_idx_num, pattern = "\\]$"),
      subj_idx_num = ifelse(parameter %in% c('delta', 'tau'),
             str_replace_all(subj_idx_num, "([0-9]*),([0-9]*)", "\\2,\\1"),
             subj_idx_num
      )
    ) |> 
    separate(subj_idx_num, into = c('subj_idx_num', 'condition')) |> 
    mutate(
      subj_idx_num = as.numeric(subj_idx_num),
      parameter = case_when(
        parameter == 'alpha' ~ paste0(task_prefix, 'a'),
        parameter == 'tau' & condition == 1 ~ paste0(task_prefix, 't1'),
        parameter == 'tau' & condition == 2 ~ paste0(task_prefix, 't2'),
        parameter == 'delta' & condition == 1 ~ paste0(task_prefix, 'v1'),
        parameter == 'delta' & condition == 2 ~ paste0(task_prefix, 'v2')
      )) |> 
    select(-condition) |> 
    pivot_wider(names_from = "parameter", values_from = "estimated") |> 
    left_join(id_matches) |> 
    select(-subj_idx_num)
}

extract_ddm_estimates_2con_expl <- function(mcmc, task_prefix, id_matches) {
  
  mcmc |> 
    pivot_longer(everything(), names_to = "parameter", values_to = "estimated") |> 
    group_by(parameter) |> 
    summarise(estimated = mean(estimated, na.rm = T)) |> 
    filter(str_detect(parameter, pattern = 'deviance|^mu|^prec', negate = T)) |> 
    separate(col = parameter, into = c('parameter', 'subj_idx_num'), sep = "\\[") |> 
    mutate(
      subj_idx_num = str_remove(subj_idx_num, pattern = "\\]$"),
      subj_idx_num = ifelse(parameter %in% c('delta', 'alpha', 'tau'),
                            str_replace_all(subj_idx_num, "([0-9]*),([0-9]*)", "\\2,\\1"),
                            subj_idx_num
      )
    ) |> 
    separate(subj_idx_num, into = c('subj_idx_num', 'condition')) |> 
    mutate(
      subj_idx_num = as.numeric(subj_idx_num),
      parameter = case_when(
        parameter == 'alpha' & condition == 1 ~ paste0(task_prefix, 'a1'),
        parameter == 'alpha' & condition == 2 ~ paste0(task_prefix, 'a2'),
        parameter == 'tau'   & condition == 1 ~ paste0(task_prefix, 't1'),
        parameter == 'tau'   & condition == 2 ~ paste0(task_prefix, 't2'),
        parameter == 'delta' & condition == 1 ~ paste0(task_prefix, 'v1'),
        parameter == 'delta' & condition == 2 ~ paste0(task_prefix, 'v2')
      )) |> 
    select(-condition) |> 
    pivot_wider(names_from = "parameter", values_from = "estimated") |> 
    left_join(id_matches) |> 
    select(-subj_idx_num)
}
