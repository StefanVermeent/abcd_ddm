### Date: 2022-09-29 12:33:42

### Description: Read first part of NIH Toolbox data.


### For more information on this commit, see the README file, or go to https://github.com/StefanVermeent/abcd_ddm/commit/d5245a2ffa01a2429a4ebd87624c345e20070a42

### Below is the full code that was used to access the data:


list.files(glue('{data_folder}/abcd_tb_tlb01/NIHTB/NIHTB'), full.names = TRUE) |>
str_subset(pattern = 'Narrow_Structure') |>
map(function(x) readr::read_csv(x) |>
dplyr::select(tidyr::everything()) |>
dplyr::filter() |>
shuffle(data = _, shuffle_vars = 'NULL', long_format = FALSE, seed = 4858342))
