### Date: 2022-10-11 12:03:50

### Description: Read NIH Toolbox trial-level data part 1 -- incl. files that previously got dropped by accident.


### For more information on this commit, see the README file, or go to <anonymized repository>

### Below is the full code that was used to access the data:


list.files(glue('{data_folder}/abcd_tb_tlb01/NIHTB/NIHTB'), full.names = TRUE) |>
str_subset(pattern = 'Narrow_Structure') |>
map(function(x) readr::read_csv(x) |>
dplyr::select(tidyr::everything()) |>
dplyr::filter() |>
shuffle(data = _, shuffle_vars = 'NULL', long_format = FALSE, seed = 4858342))
