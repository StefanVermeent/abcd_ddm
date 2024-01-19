### Date: 2022-12-08 10:04:00

### Description: read family ids including info on waves.


### For more information on this commit, see the README file, or go to https://github.com/StefanVermeent/abcd_ddm/commit/46e752f3255c245140d0bdf13c14fc956159c93b

### Below is the full code that was used to access the data:


readr::read_delim('closed_data/acspsw03.txt') |>
dplyr::select(subjectkey,rel_family_id,eventname) |>
dplyr::filter() |>
shuffle(data = _, shuffle_vars = 'NULL', long_format = FALSE, seed = 4858342)
