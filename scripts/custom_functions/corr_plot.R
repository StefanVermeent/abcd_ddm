corr_table <- function (data, sample_size = T, use = "pairwise", method = "pearson", 
                        stats = NULL, c.names = names(data), change = F, numbered = F, 
                        flagged = T) {
  
  my_ifelse <- function(...) {
    suppressWarnings(ifelse(...))
  }
  
  descriptives <- psych::describe(data) %>% as.data.frame() %>% round(2) 
  corr_data <- psych::corr.test(x = data, use = use, method = method)
  ns <- corr_data$n
  rs <- corr_data$r %>% round(2) %>% formatC(digits = 2, width = 3, flag = '0', format = 'f')
  
  descriptives <- 
    descriptives %>% 
    select(-vars) %>% 
    rownames_to_column(var = "var") %>% 
    mutate(across(3:13, ~ formatC(.x, digits = 2, width = 3, flag = '0', format = 'f'))) %>% 
    pivot_longer(cols = c(-var), names_to = "stat", values_to = "value", values_transform = list(value = as.character)) %>% 
    pivot_wider(names_from = var, values_from = value)
  
  if (!is.null(stats)) {
    descriptives <- 
      descriptives %>% 
      filter(stat %in% stats) %>% 
      slice(match(stats, stat)) %>% 
      mutate(stat = str_to_title(stat), stat = ifelse(stat=="Sd","SD",stat))
  }
  if (flagged) {
    ps <- 
      corr_data$p %>% 
      as.data.frame() %>% 
      mutate(across(.cols = everything(), .fns = ~ my_ifelse(as.numeric(.x) <  .01, "**", as.character(.x)))) %>% 
      mutate(across(.cols = everything(), .fns = ~ my_ifelse(as.numeric(.x) <= .05 & !is.na(as.numeric(.x)), "*", .x))) %>% 
      mutate(across(.cols = everything(), .fns = ~ my_ifelse(as.numeric(.x) >  .05 & !is.na(as.numeric(.x)), " ", .x))) %>% 
      as.matrix()
    
    flagged.rs <- 
      paste(rs, ps, sep = "") %>% 
      matrix(nrow = nrow(rs), ncol = ncol(rs))
    
    rs[lower.tri(rs)] <- flagged.rs[lower.tri(flagged.rs)]
  }
  
  if (sample_size) {
    if (length(ns) > 1) {
      rs[upper.tri(rs)] <- ns[upper.tri(ns)]
    }
    else {
      rs[upper.tri(rs)] <- ns
    }
  }
  else {
    rs[upper.tri(rs)] <- NA
  }
  
  corrs <- 
    rs %>% 
    as.data.frame() %>% 
    rownames_to_column("stat") %>% 
    mutate(stat = paste(row_number(), ". ", c.names, sep = "")) %>% 
    mutate(across(2:ncol(.), ~ my_ifelse(.x == "1.00", "-", .x))) %>% 
    # add_row(.before = 1, stat = "*Correlations*") %>% 
    # add_row(stat = "*Descriptive Statistics*") %>% 
    mutate(across(2:ncol(.), ~my_ifelse(is.na(.x), "", .x))) %>% 
    mutate(across(2:ncol(.), ~case_when(str_detect(.x, "^-0\\.") ~ str_pad(.x, 7, "right"," "),
                                        str_detect(.x, "\\.\\d\\d\\*\\*$") ~ str_pad(.x, 7, "left"," "),
                                        str_detect(.x, "\\.", negate = T) ~ .x,
                                        T ~ str_pad(.x, 7, "both"," ")))) %>% 
    bind_rows(purrr::map(descriptives, as.character))
  
  if (change) {
    names(corrs) <- c("Variable", c.names)
  }
  if (numbered) {
    names(corrs) <- c("Variable", 1:length(c.names))
  }
  corrs
}
