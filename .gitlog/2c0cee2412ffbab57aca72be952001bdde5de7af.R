### Date: 2022-09-28 08:49:57

### Description: Read TBI data for data exclusions.


### For more information on this commit, see the README file, or go to <anonymized repository>

### Below is the full code that was used to access the data:


readr::read_delim('closed_data/abcd_tbi01.txt') |>
dplyr::select(tidyr::everything()) |>
dplyr::filter() |>
shuffle(data = _, shuffle_vars = 'NULL', long_format = FALSE, seed = 4858342)
