### Date: 2022-12-08 10:04:00

### Description: read family ids including info on waves.


### For more information on this commit, see the README file, or go to <anonymized repository>

### Below is the full code that was used to access the data:


readr::read_delim('closed_data/acspsw03.txt') |>
dplyr::select(subjectkey,rel_family_id,eventname) |>
dplyr::filter() |>
shuffle(data = _, shuffle_vars = 'NULL', long_format = FALSE, seed = 4858342)
