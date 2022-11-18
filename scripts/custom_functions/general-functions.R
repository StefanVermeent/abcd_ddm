fix_subject_ids <- function(data) {
  
  # Selection of regular expressions that fix general mistakes in subject identifiers.
  # Specifically, each subject ID should be preceded by 'NDAR_INV' followed by a unique code.
  
  data |> 
    filter(!str_detect(str_to_lower(subj_idx), "test|tes|practice")) |> 
    mutate(
      subj_idx = str_replace_all(str_to_lower(subj_idx), "^.*ndar", "ndar"),
      subj_idx = str_replace_all(str_to_lower(subj_idx), "ndar_inv\\s|^ndar.*inv|ndarinv|ndar[^[:alnum:]]|^inv|^dar_inv|^nadr_inv|bdar_inv|ndra_inv|ndr_inv|ndat_inv", ""),
      subj_idx = toupper(paste0("NDAR_INV", subj_idx)),
      subj_idx = str_remove_all(subj_idx, "(-|_)[0-9]$"),
      subj_idx = str_remove_all(subj_idx, "(-|\\s).*")
    )
}



fix_id_typos <- function(data, reference, id = "subj_idx", distance = 2) {
  
  # Function that tries to detect typos using the Levenshtein distance between unmatched ids and the reference list (i.e., the ids found in the summary score df)
  # The Levenshtein distance counds the number of deletions, insertions and substitutions necessary to turn one string into another.
  # Thus, a small Levensthein distance (e.g., 1 or 2) in subject IDs likely indicates a typo.
  
  data_ids <- data |> 
    pull(id) |> 
    unique()
  
  reference |> 
    as_tibble_col(column_name = "original")
  
  mismatches <- data_ids[!data_ids %in% reference] %>% 
    as_tibble_col(column_name = "subj_idx") %>%
    mutate(
      similar = pmap(.l = ., function(subj_idx) {
        
        reference |> 
          as_tibble_col(column_name = "ref") |> 
          mutate(original = subj_idx) |>  
          mutate(
            string_dist = stringdist::stringdist(a = original, b = ref, method = 'lv')
          ) |> 
          filter(string_dist == min(string_dist))
      })
    ) |> 
    unnest(similar) 
    
  mismatches |> select(subj_idx, string_dist) |> 
    distinct() |> 
    count(string_dist) |> 
    glue_data("the IDs of {n} subjects had a Levenshtein distance of {string_dist} from an ID in the curated set.
              
              ") |> print()
  
  mismatches <- mismatches |> 
  filter(
      string_dist == 1 |
      string_dist == 2 |
      string_dist == 3 & subj_idx %in% c("NDAR_INVIBVJ5JJZDPB", "NDAR_INVIVNYTX6A82H", "NDAR_INVMFOOX6V9", "NDAR_INV3RTY34Y", "NDAR_INV6MB1G")
    )
  
  glue("Only IDs with a Levenshtein distance of 1, 2 or 3 (in the latter case carefully curated) were changed to the reference ID
       
       Thus, a total of {nrow(mismatches)} IDs were judged to have a recoverable typo and were changed to the reference ID.
       ") |> print()
  
  data |> 
    left_join(mismatches) |> 
    mutate(subj_idx = ifelse(!is.na(string_dist), ref, subj_idx)) |> 
    select(-ref, -original, -string_dist)
}


filter_baseline <- function(data) {
  
  data |> 
    group_by(subj_idx) |> 
    nest(trials = -subj_idx) |> 
    mutate(
      trials = map(trials, function(x) {
        
        if(nrow(x) > 20) {
        
        dates <- x |> 
          pull(InstrumentStartedTimestamp) |> 
          unique() |> 
          str_extract(string = _, pattern = "^[0-9]*")
        
        smallest <- min(as.numeric(dates))
        
        glue('More than 1 year found: {paste(dates, collapse = ", ")}. {smallest} was selected as the baseline.')
        
        x |> 
          filter(str_detect(string = InstrumentStartedTimestamp, pattern = as.character(smallest)))
        
        } else {
          x
      }
      })
    ) |> 
    unnest(trials)
}
    
    
check_id_matches <- function(data) { 
  
  data_ids <- data |> pull(subj_idx) |> unique()

  glue("Both in sumset and trial data: {data_ids[data_ids %in% nih_ids] |> length()}") |> print()
  glue("In trial data but not in sumset: {data_ids[!data_ids %in% nih_ids] |> length()}") |> print()
  glue("In sumset but not in trial data: {nih_ids[!nih_ids %in% data_ids] |> length()}") |> print()
}




# DDM fitting helpers -----------------------------------------------------

fast_dm_settings <- function(task, model_version = "", method = "ml", precision = 3, zr = 0.5, d = 0, szr = 0, sv = 0, st0 = 0, p = 0, depend = "", format) {
  
  depend = str_c(depend, collapse = "\n")
  
  spec <- tribble(
    ~param,        ~setting,                                           ~ command,
    "method",      method,                                             "{param} {setting}",
    "precision",   as.character(precision),                            "{param} {setting}",
    "set zr",      as.character(zr),                                   "{param} {setting}",
    "set d",       as.character(d),                                    "{param} {setting}",
    "set szr",     as.character(szr),                                  "{param} {setting}",
    "set sv",      as.character(sv),                                   "{param} {setting}",
    "set st0",     as.character(st0),                                  "{param} {setting}",
    "set p",       as.character(p),                                    "{param} {setting}",
    "",            depend,                                             "{param} {setting}",
    "format",      format,                                             "{param} {setting}",
    "",            "load *.dat",                                       "{param} {setting}", 
    "",            glue("log ddm_results_{task}{model_version}.lst"),  "{param} {setting}"
  ) %>%
    filter(setting != "") %>%
    rowwise() %>%
    transmute(command = glue(command)) %>%
    glue_data("{command}")
  
  message("\nThe following model specification was written to the DDM folder:\n\n")
  print(spec)
  
  write_file(str_c(spec, collapse = "\n"), here(glue("{task}{model_version}_ml.ctl")))
}

write_DDM_files <- function(data, vars = c("rt", "correct"), task) {
  
  for (i in unique(data$id)) {
    # 1. Filter subject i and write individual data files to data folder
    change_DDM_i <- data %>%
      filter(id == i) %>%
      select(all_of(vars)) %>%
      write_delim(file = here(str_c(task, "_DDM_subject", i, ".dat")), col_names = FALSE)
  }
}


execute_fast_dm <- function(task, model_version = "") {
  # 2. Run DDM model
  path_to_files <- paste0("pushd ", here()) %>% str_replace_all(., pattern="/", replacement="\\\\")
  run_DDM <- str_glue(" && fast-dm.exe {task}{model_version}_ml.ctl")
  cmd <- paste0(path_to_files, run_DDM)
  
  shell(cmd)
  
  message("The following command was sent to the shell: ", cmd)
}


read_DDM <- function(task, model_version = "") {
  results <- read_table(here(str_glue("ddm_results_{task}{model_version}.lst"))) %>%
    mutate(dataset = str_replace_all(dataset, str_c("^", task, "_DDM_subject"), "") %>% as.numeric(),
           task = task) %>%
    rename(id = dataset)
  
  return(results)
}


remove_DDM_files <- function() {
  file.remove(list.files(here(), pattern = ".dat$|.lst$", full.names=TRUE))
  
  message("All individual data files were removed from the folder")
}



