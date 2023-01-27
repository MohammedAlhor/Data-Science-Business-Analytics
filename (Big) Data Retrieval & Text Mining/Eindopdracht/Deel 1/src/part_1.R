library(tidyverse)

#' Convert Dutch date to proper R Date
#' 
#' You can use this for a single date, or for a vector of dates (e.g., with mutate())
#' Use it within the retrieve_member_activity() function.
#'
#' @param dates A character vector of one or more dates in short Dutch notation
#'
#' @return A vector of R Dates
as_date <- function(dates){
  months_dutch <- setNames(
    as.character(1:12),
    c("januari","februari","maart","april","mei","juni","juli","augustus","september","oktober","november","december")
  )
  tryCatch({
    str_replace_all(dates, months_dutch) %>% lubridate::dmy()
  }, error = function(e){
    lubridate::as_date(NA_integer_)
  })
}

#' Retrieve members of parliament
#'
#' Side effect: the members info is saved in the clean_data folder
#' 
#' @return A tibble with the columns as specified
retrieve_members <- function(){
  url <- "https://www.tweedekamer.nl/kamerleden_en_commissies/alle_kamerleden"
  
  #
  # Maak de functie af, sla je data op in members_df zodat de regels hieronder goed werken
  #
  
  
  #
  # Einde van jouw code
  #
  
  saveRDS(members_df, file = "clean_data/members.Rds")
  return(members_df)
}

#' Retrieve activity of a single member of parliament
#'
#' @param member_url The url to the personal page of a member
#'
#' @return A tibble with the columns as specified
retrieve_member_activity <- function(member_url){
  url <- str_c("https://www.tweedekamer.nl", member_url)
  print(url)
  
  # Retrieve all urls to the pages that contain this member's activities
  all_activity_page_links <- str_c(url, "/meer?activityType=", c("schriftelijk","mondeling","amendementen","moties"), "&name=&page=0") %>%
    map(function(page_url){
      links <- httr::GET(page_url) %>% httr::content() %>% rvest::html_nodes(".m-pager__page") %>% rvest::html_attr("href") 
      if (length(links) == 0) {
        return(page_url)
      } else {
        return(str_c(page_url, 0:(links %>% readr::parse_number() %>% max())))
      }
    }) %>% unlist() 
  
  #
  # Maak de functie af, sla je data op in activity zodat de regels hieronder goed werken
  #

  
  
  #
  # Einde van jouw code
  #
  
  print(str_c(nrow(activity), " activities found"))
  return(activity)
}


  

#' Retrieve and save activity for the given members
#'
#' Side effect: the activity is saved in the clean_data folder
#'
#' @param members_df The tibble of members from retrieve_members() function
#'
#' @return A tibble with all activity for all the given members
save_all_activity <- function(members_df){
  member_activity <- purrr::map_df(members_df$url, retrieve_member_activity)
  saveRDS(member_activity, file =  "clean_data/member_activity.Rds")
}
  

#' Function to test your work and put all the necessary files in one zip file you can upload to Canvas.
#'
#' It will generate errors, warnings and OK messages. Please correct all problems before submitting.
#' If there are no crashing errors, a zip file will be created that will contain all the necessary files.
#' If there are no remaining problems, you can upload the zip on Canvas to hand in your work.
#'
#' @return Nothing. Hopefully a zip file will be created. 
hand_in <- function(){
  # we use the zip package instead of the built-in zip function because it is most likely to work on different systems
  if (!"zip" %in% installed.packages()){
    install.packages("zip")
  }
  if ("part1.zip" %in% list.files()){
    file.remove("part1.zip")
  }
  
  # run the checker
  source("src/part_1_test.R")
  tester()
  
  # find the name of the current folder
  # make sure you don't change the working directory after opening the project or else this won't work
  if (sum(str_detect(list.files(), ".Rproj")) == 1){
    # the working directory is still the project root folder
    zip::zipr("part1.zip",list.files(all.files = TRUE, no.. = TRUE))
    print("Je opdracht is ingepakt in part1.zip en staat klaar in je project folder. Corrigeer eventuele overgebleven 'Failed' meldingen voordat je het inlevert!")
  } else {
    stop("Je hebt de working directory aangepast met setwd(). Het makkelijkst is om je project te sluiten en opnieuw te openen. Dan staat dit weer goed.")
  }
  
}
