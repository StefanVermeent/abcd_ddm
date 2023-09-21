training_set  <- readr::read_csv(paste0("data/training_set", data_suffix, ".csv"))
test_set  <- readr::read_csv(paste0("data/test_set", data_suffix, ".csv"))

mnlfa_ivs <- read_csv("data/mnlfa_ivs.csv") |> 
  select(subj_idx  = SUBID, 
         dep_mnlfa = DepETA, 
         threat_mnlfa = ThreatETA,                
         age_c        = AGEC, 
         sex          = FEMALE, 
         age_m        = agemo,
         eth_white    = WHITE, 
         eth_black    = BLACK, 
         eth_hisp     = HISPANIC, 
         eth_other    = OTHER, 
         inr          = INR, 
         inr_c        = INRC,
         high_edu     = HIGHED, 
         high_edu_c   = HIGHEDC
  )

bind_rows(training_set, test_set) |> left_join(mnlfa_ivs) |> View()

write_csv(mnlfa_ivs, "data/iv_data.csv")
