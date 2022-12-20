lmt_clean <- readr::read_csv("data/lmt_clean.csv")
flanker_clean <- readr::read_csv("data/flanker_clean.csv")
pcps_clean <- readr::read_csv("data/pcps_clean.csv")
dccs_clean <- readr::read_csv("data/dccs_clean.csv")

final_sample_ids <- 
  unique(
    c(
      lmt_clean |> pull(subj_idx) |> unique(),
      flanker_clean |> pull(subj_idx) |> unique(),
      dccs_clean |> pull(subj_idx) |> unique(),
      pcps_clean |> pull(subj_idx) |> unique()
    )
  )

set.seed(4876945)

# Read Family IDs 

family_ids <- read_delim(file = 'data/acspsw03.txt', select_vars = c("subjectkey", "rel_family_id", "eventname")) |> 
  filter(str_detect(eventname, "baseline")) |> 
  filter(subjectkey %in% final_sample_ids) |> 
  select(-eventname)

# Check whether there are any missing family ids
any(is.na(family_ids$rel_family_id))

# Count the number of siblings per family, and randomly shuffle
full_set <- family_ids |> 
  count(rel_family_id, name = "n_siblings") |> 
  group_split(rel_family_id) |>
  sample() |> 
  bind_rows() |> 
  mutate(n_row = 1:n()) 

# In the shuffled set, assign first 1500 participants to training set and the rest to test set.
# This assures that children from the same family end up in the same set.
full_set <- full_set %>% 
  mutate(
    count = pmap_dbl(., function(rel_family_id, n_siblings, n_row){
      if(n_row == 1) {
        n_siblings
      } else {
        n_siblings + sum(full_set[1:(n_row-1), "n_siblings"])
      }
    })
  ) |> 
  mutate(set = ifelse(count <= 1500, "training", "test")) |> 
  select(rel_family_id, set)
  
# Create training and test set
training_set <- family_ids |> 
  right_join(full_set |> filter(set == "training")) |> 
  rename(subj_idx = subjectkey)
  
test_set <- family_ids |> 
  right_join(full_set |> filter(set == "test")) |> 
  rename(subj_idx = subjectkey)


# Verify that all siblings are in the same set
assertthat::assert_that(all(!training_set$rel_family_id %in% test_set$rel_family_id), msg = "Some siblings ended up in different sets!")

# Save both sets
write_csv(training_set, "data/training_set.csv")
write_csv(test_set, "data/test_set.csv")



