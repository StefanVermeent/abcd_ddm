### Date: 2022-10-07 12:52:55

### Description: Read NIH Toolbox performance summary scores


### For more information on this commit, see the README file, or go to https://github.com/StefanVermeent/abcd_ddm/commit/ec19aea7b66d095174841088d52669177f90c793

### Below is the full code that was used to access the data:


readr::read_delim('closed_data/abcd_tbss01.txt') |>
dplyr::select(tidyr::everything()) |>
dplyr::filter() |>
shuffle(data = _, shuffle_vars = 'NULL', long_format = FALSE, seed = 4858342)
