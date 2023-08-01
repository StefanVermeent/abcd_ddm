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
      
    #  url <- gert::git_remote_list()$url |> 
    #    str_remove(string = _, "\\.git$") |> 
    #    paste0(paste0("/commit/", commit))
      
      url <- "<anonymized repository>"
      
      script <-
        glue(
          "
          ### Date: {time}\n
          ### Description: {description}\n\n
          ### For more information on this commit, see the README file, or go to {url}\n
          ### Below is the full code that was used to access the data:\n\n
          {code}"
        )
      
      writeLines(script, con = paste0(".gitlog/", commit, ".R"))
    }
  })


