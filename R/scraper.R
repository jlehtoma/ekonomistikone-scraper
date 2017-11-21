library(rvest)
library(tidyverse)

# Helper functions --------------------------------------------------------

process_answer <- function(url) {
  
  message("Processing ", url)
  
  question_page <- read_html(url)
  question_number <- unlist(strsplit(url, "-"))[2]
  
  answers <- question_page %>% 
    rvest::html_nodes("table") %>%
    rvest::html_table() %>% 
    purrr::pluck(1) %>% 
    dplyr::mutate(numero = question_number) %>% 
    dplyr::select(numero, dplyr::everything())
  
  return(answers)
}

process_question <- function(url) {
  
  message("Processing ", url)
  
  question_page <- read_html(url)
  
  question <- question_page %>% 
    rvest::html_nodes("h2") %>%
    rvest::html_text() %>% 
    gsub("Kysymys: ", "", .)
  
  question_number <- unlist(strsplit(url, "-"))[2]
  
  return(data.frame(numero = question_number,
                    kysymys = question))
}

# Scrape data -------------------------------------------------------------

base_url <- "http://www.ekonomistikone.fi/kysymys-"
question_range <- 10:22

question_urls <- paste0(base_url, question_range)

all_questions <- purrr::map(question_urls, process_question) %>% 
  dplyr::bind_rows()

all_answers <- purrr::map(question_urls, process_answer) %>% 
  dplyr::bind_rows()


# Write out data ----------------------------------------------------------

readr::write_csv(all_questions, "data/kysymykset.csv")
readr::write_csv(all_answers, "data/vastaukset.csv")
