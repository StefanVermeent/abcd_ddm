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




plot_RTs <- function(data, title = ''){
  data |> 
    ggplot(aes(RT, group = factor(correct), fill = factor(correct))) +
    geom_histogram(alpha = 0.6) +
    labs(
      title = title
    )
}

plot_meanRTs <- function(data, title = ''){
  data |> 
    group_by(subj_idx) |> 
    summarise(meanRT = mean(RT)) |> 
    ggplot(aes(meanRT)) +
    geom_histogram() +
    labs(
      title = title
    )
}

plot_meanAcc <- function(data, title = ''){
  data |> 
    group_by(subj_idx) |> 
    summarise(meanAcc = sum(correct == 1)/n()*100) |> 
    ggplot(aes(meanAcc)) +
    geom_histogram() +
    labs(
      title = title
    )
}

