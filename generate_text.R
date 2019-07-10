# first part copied from:
# https://raw.githubusercontent.com/mine-cetinkaya-rundel/user2019-schedule/master/data-scrape.R
# by Mine CetinkayaRundel (https://twitter.com/minebocek)

library(tidyverse)
library(rvest)
library(glue)
library(stringr)

page <- read_html("http://www.user2019.fr/talk_schedule/")

tabs <- page %>%
  html_table("td", header = TRUE)

process_schedule <- function(day_tab, day_name){
  
  raw <- day_tab %>% select(-2, -Slides)
  
  nested <- raw %>%
    slice(seq(1, nrow(raw), by = 2)) %>%
    select(Session, Title, Speaker) %>% 
    group_nest(Session)
  
  for (session in 1:nrow(nested)) {
    cat('### ', nested[session, "Session", drop = TRUE], '\n\n')
    
    unnested <- nested[session, "data"] %>% tidyr::unnest()
    for (talk in 1:nrow(unnested)) {
      cat('- ', unnested[talk, "Title", drop = TRUE], '  \n')
      cat(unnested[talk, "Speaker", drop = TRUE], '\n\n')
    }
    
    cat('---\n\n')
    
  }
  
  nested$Session %>% 
    purrr::map_chr(., 
      ~glue("[{.x}](#{tolower(.x) %>% 
      str_replace_all(' ', '-') %>% 
      str_remove('/') %>% 
      str_remove(',') %>% 
      str_remove('&-')
        })")
    ) %>% 
    glue_collapse(sep = ", ", last = " and ")
  
}

# purrr::walk2(1:3, c("wed", "thu", "fri"), ~process_schedule(tabs[[.x]], .y))

process_schedule(tabs[[1]], "wed")
process_schedule(tabs[[2]], "thu")
process_schedule(tabs[[3]], "fri")
