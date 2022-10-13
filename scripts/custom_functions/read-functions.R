read_csv <- function(file, select_vars = NULL, filter_rows = NULL, shuffle_vars = NULL, long_format = FALSE, seed = 4858342, ...) {
  
  read_exprs <- enexprs(...)
  
  read_exprs_chr <- names(read_exprs) |> 
    map(function(x) {
      paste(x, "=", read_exprs[x] |> as.character())
    }) |> 
    paste(collapse = ", ")
  
  file <- ifelse(!str_detect(file, "list.files"), paste0("'", file, "'"), file)
  
  if(length(read_exprs_chr) == 0) {
    read_exprs_chr <- ")"
  } else {
    read_exprs_chr <- paste0(", ", read_exprs_chr, ")")
  }
  
  
  select_vars <- if(is.null(select_vars)) {"tidyr::everything()"}
  filter_rows <- if(is.null(filter_rows)) {""}
  shuffle_vars <- if(is.null(shuffle_vars)) {"NULL"}
  
  
  code <- create_code_list(file, type = "read_csv", read_exprs_chr, select_vars, filter_rows, shuffle_vars, long_format, seed)
  
  pipeline_chr <- code |>
    purrr::compact() |>
    paste(collapse = " |> ") |> 
    paste0(ifelse(str_detect(file, ",|list.files"), ")", ''))
  
  # Read data and apply manipulations
  data <- pipeline_chr |>
    glue::glue(.trim = F) |>
    rlang::parse_expr() |>
    rlang::eval_tidy()
  
  data_hash <- digest::digest(data)
  
  committed_hashes <-
    gert::git_log()$message |>
    grep(pattern = "\\[\\[data_access\\]\\]", x = _, value = TRUE) |>
    (\(.) regmatches(x = ., m = gregexpr("[a-z0-9]{32}", text = .)))() |>
    unlist()
  
  
 compare_and_commit(committed_hashes, data_hash, pipeline_chr = pipeline_chr)
  

  
  #  worcs::git_update(
  #    message = commit_message,
  #    files = ".gitlog/MD5"
  #  )
  
  return(data)
}



read_delim <- function(file, select_vars = NULL, filter_rows = NULL, shuffle_vars = NULL, long_format = FALSE, seed = 4858342, ...) {
  
  read_exprs <- enexprs(...)
  
  read_exprs_chr <- names(read_exprs) |> 
    map(function(x) {
      paste(x, "=", read_exprs[x] |> as.character())
    }) |> 
    paste(collapse = ", ")
  
  file <- ifelse(!str_detect(file, "list.files"), paste0("'", file, "'"), file)
  
  if(length(read_exprs_chr) == 0) {
    read_exprs_chr <- ")"
  } else {
    read_exprs_chr <- paste0(", ", read_exprs_chr, ")")
  }
  
  
  select_vars <- if(is.null(select_vars)) {"tidyr::everything()"}
  filter_rows <- if(is.null(filter_rows)) {""}
  shuffle_vars <- if(is.null(shuffle_vars)) {"NULL"}
  
  
  code <- create_code_list(file, type = "read_delim", read_exprs_chr, select_vars, filter_rows, shuffle_vars, long_format, seed)
  
  pipeline_chr <- code |>
    purrr::compact() |>
    paste(collapse = " |> ") |> 
    paste0(ifelse(str_detect(file, ",|list.files"), ")", ''))
  
  # Read data and apply manipulations
  data <- pipeline_chr |>
    glue::glue(.trim = F) |>
    rlang::parse_expr() |>
    rlang::eval_tidy()
  
  data_hash <- digest::digest(data)
  
  committed_hashes <-
    gert::git_log()$message |>
    grep(pattern = "\\[\\[data_access\\]\\]", x = _, value = TRUE) |>
    (\(.) regmatches(x = ., m = gregexpr("[a-z0-9]{32}", text = .)))() |>
    unlist()
  
  
  compare_and_commit(committed_hashes, data_hash)
    
  

    return(data)
  }
  



shuffle <- function(data, shuffle_vars, long_format, seed = seed) {
  
  if(is.null(shuffle_vars)) {
    return(data)
  }
  
  if(long_format) {
    row_nums <- data |> group_by_at(shuffle_vars[[1]]) |> summarise(n = n()) |> dplyr::pull(n)
  } else {
    row_nums <- rep(1, nrow(data))
  }
  
  set.seed(seed)
  
  data <- shuffle_vars |>
    map_dfc(function(x){
      data |>
        select(matches(x)) |>
        mutate(rows = rep(1:length(row_nums), row_nums)) |>
        group_split(rows) |>
        sample() |>
        bind_rows() |>
        select(-rows)
    }) |>
    bind_cols(
      data |>
        select(-matches(shuffle_vars))
    ) |>
    select(names(data)) |>
    arrange(across(matches(shuffle_vars[[1]])))
  
  data
}

create_code_list <- function(file, type, read_exprs_chr, select_vars, filter_rows, shuffle_vars, long_format, seed) {
  
  # Construct pipeline
  code <- list()
  
  code$read <-
    if(str_detect(file, ",|list.files")) {
      paste0(file, " |> map(function(x) readr::", type, "(x", read_exprs_chr)
    } else {
      paste0("readr::", type, "(", file, ")")
    }
  
  code$select <-
    paste0("dplyr::select(", select_vars, ")")
  
  code$filter <-
    paste0("dplyr::filter(", filter_rows, ")")
  
  code$shuffle <-
    paste0("shuffle(data = _, shuffle_vars = '", shuffle_vars, "', long_format = ", long_format, ", seed = ", seed, ")")
  
  code
}


compare_and_commit <- function(committed_hashes, data_hash, pipeline_chr) {
  
  message(length(committed_hashes), " previously committed data files found.")
  
  if(!data_hash %in% committed_hashes) {
    
    response = FALSE
    
    while(response != 'Y' & response != "n") {
      response <- readline(prompt = "NEW DATA FILE DETECTED. This will trigger an automatic commit to GitHub. Are you sure you want to continue? [Y/n]:")
    }
    
    if(response == "n") {
      return(message("Reading the data file was aborted. Use the glance_* functions to get a summary report prior to reading in the full dataset."))
    }
    
    message <- readline(prompt = "If you want, you can type a short commit message about the data file. Press enter for a default message: ")
  
  
  # Construct commit message
  commit_code <- glue::glue("code {pipeline_chr}")
  commit_hash <- glue::glue("object_hash {data_hash}")
  commit_message <- glue::glue("[[data_access]] {message}\n{commit_hash}\n{commit_code}")
  
  readr::read_file(".gitlog/MD5") |>
    paste(data_hash, sep = "\n") |>
    readr::write_file(".gitlog/MD5")
  
  gert::git_add('.gitlog/MD5')
  gert::git_commit(commit_message)
  gert::git_push()
  }
}



gbinom <- function(n, p, low=0, high=n,scale = F) {
  
  sd <- sqrt(n * p * (1 - p))
  if(scale && (n > 10)) {
    low <- max(0, round(n * p - 4 * sd))
    high <- min(n, round(n * p + 4 * sd))
  }
  values <- low:high
  probs <- dbinom(values, n, p)
  
  max_chance_performance <- max(values[which(probs > 0.025)])
  
  
  
  return((max_chance_performance/n)*100)
}

