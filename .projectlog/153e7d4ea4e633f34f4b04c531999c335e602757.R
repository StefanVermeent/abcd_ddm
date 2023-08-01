### Date: 2022-12-08 09:34:04

### Description: Access family ids


### For more information on this commit, see the README file, or go to <anonymized repository>

### Below is the full code that was used to access the data:


readr::read_delim('closed_data/acspsw03.txt') |>
dplyr::select(subjectkey,rel_family_id) |>
dplyr::filter() |>
shuffle(data = _, shuffle_vars = 'NULL', long_format = FALSE, seed = 4858342)
