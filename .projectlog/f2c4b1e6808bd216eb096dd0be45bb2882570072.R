### Date: 2022-09-26 10:45:23

### Description: read all Little Man Task data.


### For more information on this commit, see the README file, or go to <anonymized repository>

### Below is the full code that was used to access the data:


list.files(file.path(data_folder, 'LMT/baseline_year_1_arm_1'), full.names = TRUE) |>
map(function(x) readr::read_csv(x) |>
dplyr::select(tidyr::everything()) |>
dplyr::filter() |>
shuffle(data = _, shuffle_vars = 'NULL', long_format = FALSE, seed = 4858342))
