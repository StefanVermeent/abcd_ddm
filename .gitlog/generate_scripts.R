gert::git_log() |> 
  select(commit, author, time, message) |>
  (function(.) filter(., str_detect(string = .$message, pattern = "^\\[\\[")))() |> 
  separate(message, into = c("message", "hash", 'code'), sep = "\n") |> 
  separate(message, into = c("milestone", "description"), sep = "\\]\\]\\s") |> 
  mutate(milestone = str_remove(milestone, "\\[\\[")) |> 
  pwalk(function(commit,author,time,message,hash,milestone,description, code){
    
    if(milestone == "data_access"){
      code <- code |> 
        str_replace_all(string = _, "\\|> ", "\\|>\n") |> 
        str_remove(string = _, "^code ") 
      
      url <- gert::git_remote_list()$url |> 
        str_remove(string = _, "\\.git$") |> 
        paste0(paste0("/commit/", commit))
      
      script <-
        glue(
          "---
          title: 'Data access event'
          output: github_document
          ---\n\n
          ### Date: {time}\n
          ### Description: {description}\n\n
          ### For more information on this commit, go to {url}\n\n\n
          ### Below is the full code that was used to access the data:\n\n
          {code}"
        )
      
      writeLines(script, con = paste0(".gitlog/", commit, ".Rmd"))
    }
  })




  
  mutate(
    Milestone = map_chr(Milestone, function(Milestone) {
      Milestone |> 
        str_remove("\\[\\[") |>
        str_replace("_", " ") |> 
        str_to_title()
    })
  ) |> 
  select(Time=time, Milestone, Description, Code, `Data Hash`) |> 
  mutate(
    Code = str_replace_all(Code, "\\|\\>", "\\|\\>\\\n")
  ) |>
  glue_data("  - *{Time}*, {Milestone}, {Description}")