video_url <- function(title, channel = "https://www.youtube.com/channel/UC_R5smHVXRYGhZYDJsnXTwg") {
  # fixes for mismatches
  title <- gsub("Applications 2", "Application 2", title, ignore.case = TRUE)
  title <- gsub("epidemiology 2", "Epidemology 2", title, ignore.case = TRUE)
  title <- gsub("Multivariate", "Multivariate data", title, ignore.case = TRUE)
  title <- gsub("Switching", "Switch", title, ignore.case = TRUE)
  title <- gsub("Open science, ", "", title, ignore.case = TRUE)
  # explicit set of lightning talks
  lightning <- c(
    "Biostatistics & epidemiology",
    "Open science, education & community",
    "Spatial & time series",
    "Text mining",
    "Workflow & development",
    "Bioinformatics & biostatistics",
    "Methods & applications",
    "Models & methods",
    "Shiny & web",
    "Switching to R"
  )
  lightning <- grepl(paste0("^", lightning, "$", collapse = "|"), title, ignore.case = TRUE)
  sprintf(
    '%s/search?query=toulouse+2019+"%sTalk+%s"', 
    channel,
    ifelse(lightning, "Lightning+", ""),
    curl::curl_escape(title)
  )
}
# browseURL(video_url("###  Applications 1"))

insert_video_links <- function(file = "README.md") {
  
  content <- readLines(file)
  pattern <- "^(###.*)\\s+(-.*)?$"
  is_header_for_videos <- grepl(pattern, content) & !grepl("Keynote", content)
  # remove trailing spaces and existing links 
  headers <- sub("^(###.*)\\s+(-.*)?$", "\\1", content[is_header_for_videos])
  content[is_header_for_videos] <- 
    sprintf("%s - [videos](%s)", headers, video_url(sub("###\\s+", "", headers)))
  
  writeLines(content, file)
  
}

