### Date: 2022-09-22 10:01:58

### Description: read single LMT file to develop parsing strategy.


### For more information on this commit, see the README file, or go to <anonymized repository>

### Below is the full code that was used to access the data:


'closed_data/LMT/baseline_year_1_arm_1/NDAR_INV00BD7VDC_baseline_year_1_arm_1_lmt.csv' |>
map_df(function(x) readr::read_csv(x)) |>
dplyr::select(tidyr::everything()) |>
dplyr::filter() |>
shuffle(data = _, shuffle_vars = 'NULL', long_format = FALSE, seed = 4858342)
