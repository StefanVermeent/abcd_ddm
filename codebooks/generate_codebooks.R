library(sjlabelled)

extract_attrs <- function(data, attr_type){
  purrr::map_chr(data, function(x){
    
    # For variable labels
    if(attr_type == "label"){
      extracted_attr <- attr(x, which = attr_type)
      # If it's null, make it an empty string
      if (is.null(extracted_attr)) {
        extracted_attr <- ""
      }
      extracted_attr
    }
    
    # For variable values
    if(attr_type == "labels"){
      extracted_attr <- attr(x, which = attr_type)
      if (is.null(extracted_attr)) {
        extracted_attr <- ""
      }
      else {
        extracted_attr <- paste(names(extracted_attr),"=", extracted_attr, collapse = ", ")
      }
      extracted_attr
    }
    
    extracted_attr
  })
}

create_codebook <- function (data) {
  # create a lookup table for a labelled dataset
  lookup_data <- 
    dplyr::tibble(
      Variable = names(data), 
      Label    = extract_attrs(data, "label"), 
      Values   = extract_attrs(data, "labels") 
    )
  
  codebook <- lookup_data
  no_labels <- codebook %>% dplyr::filter(Label == "")
  no_values <- codebook %>% dplyr::filter(Values == "")
  if (nrow(no_labels) > 0) {
    message("---------The following variables have no labels:---------\n", 
            paste(no_labels %>% dplyr::pull(Variable), collapse = "\n"))
  }
  if (nrow(no_values) > 0) {
    message("---------The following variables have no value labels:---------\n", 
            paste(no_values %>% dplyr::pull(Variable), collapse = "\n"))
  }
  codebook
  
  
}

ddm_data      <- readr::read_csv('data/ddm_data.csv')
iv_data       <- readr::read_csv(paste0("data/iv_data", data_suffix, ".csv"))
test_set      <- readr::read_csv(paste0("data/test_set", data_suffix, ".csv"))


ddm_data <- ddm_data |> 
  sjlabelled::var_labels(
    subj_idx = "Unique subject identifier",
    dccs_a   = "Drift Diffusion Model, Boundary separations of the Attention Shifting Task",
    dccs_v   = "Drift Diffusion Model, Drift rates of the Attention Shifting Task",
    dccs_t   = "Drift Diffusion Model, Non-decision times of the Attention Shifting Task",
    flanker_a   = "Drift Diffusion Model, Boundary separations of the Inhibition Task",
    flanker_v   = "Drift Diffusion Model, Drift rates of the Inhibition Task",
    flanker_t   = "Drift Diffusion Model, Non-decision times of the Inhibition Task",
    pcps_a   = "Drift Diffusion Model, Boundary separations of the Processing Speed Task",
    pcps_v   = "Drift Diffusion Model, Drift rates of the Processing Speed Task",
    pcps_t   = "Drift Diffusion Model, Non-decision times of the Processing Speed Task",
    rotation_a   = "Drift Diffusion Model, Boundary separations of the Mental Rotation Task",
    rotation_v   = "Drift Diffusion Model, Drift rates of the Mental Rotation Task",
    rotation_t   = "Drift Diffusion Model, Non-decision times of the Mental Rotation Task"
)

iv_data <- iv_data |> 
  sjlabelled::var_labels(
    subj_idx = "Unique subject identifier",
    dep_mnlfa = "Summed Material deprivation score (MNFLA corrected)",
    threat_mnlfa = "Summed Household threat score (MNLFA corrected)",
    age_c = "Age in months (centered)",
    sex = "Child sex (0 = male, 1 = female)",
    age_m = "Age in months",
    eth_white = "Ethnicity Dummy variable - White",
    eth_black = "Ethnicity Dummy variable - Black",
    eth_hisp  = "Ethnicity Dummy variable - Hispanic",
    eth_other = "Ethnicity Dummy variable - Other",
    inr       = "Income-to-needs ratio",
    inr_c     = "Income-to-needs ratio (centered)",
    high_edu  = "Highest education of parents in years",
    high_edu_c = "Highest education of parents in years (centered)",
  )

test_set <- test_set |> 
  sjlabelled::var_labels(
    subj_idx = "Unique subject identifier",
    rel_family_id = "Unique family identifier",
    set = "Indicates whether child belongs to the training set or test set"
  )

ddm_data <- create_codebook(ddm_data)
iv_data <- create_codebook(iv_data)
test_set <- create_codebook(test_set)

write_csv(ddm_data, "codebooks/ddm_data.csv")
write_csv(iv_data, "codebooks/iv_data.csv")
write_csv(test_set, "codebooks/test_set.csv")

