### Date: 2022-10-11 20:26:24

### Description: Read NIH Toolbox Part 2. Incl. files that got previously dropped by accident.


### For more information on this commit, see the README file, or go to <anonymized repository>

### Below is the full code that was used to access the data:


list.files(glue('{data_folder}/abcd_tb_tlb01/NIHTB/NIHTB'), full.names = TRUE) |>
str_subset(pattern = '[0-9]_Assessment') |>
future_map(function(x) readr::read_csv(x) |>
dplyr::select(tidyr::everything()) |>
dplyr::filter() |>
shuffle(data = _, shuffle_vars = 'NULL', long_format = FALSE, seed = 4858342))
