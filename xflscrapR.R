library(glue)
library(RSelenium)
library(XML)
library(tidyverse)
library(lubridate)

# Open Web Browser
driver <- rsDriver(browser = "firefox")
remDr <- driver[["client"]]

# Function that navigates to each game web page and scrapes the play by play
retrieve_games <- function(game_num){
  # Navigate to XFL Stats Page
  remDr$navigate(glue("https://stats.xfl.com/{game_num}"))
  
  # Click on play list tab
  remDr$findElement("xpath", "//h3[@id = 'pgPlayList']")$clickElement()
  
  # Create function that returns a list of webElements containing each row value in a column
  retrieve_column <- function(column_name){
    if(column_name == "rPlayDesc"){
      remDr$findElements("xpath", glue("//div[@class = 'table totalPlaylist']\\
                                      //child::div[@class = 'body']\\
                                      //child::div[@class = '{column_name}']\\
                                      //child::div[contains(@data-bind, 'PlayDescription')]"))
    } else {
      remDr$findElements("xpath", glue("//div[@class = 'table totalPlaylist']\\
                                      //child::div[@class = 'body']\\
                                      //child::div[@class = '{column_name}']"))
    }
  }
  
  # Store a list of webElement lists for each column we want to retrieve
  columns <- lapply(c("rQtr", "rStart", "rPoss", "rPossDown", "rPlayDesc", "rPlays hideMobile scored",
                      "rYards hideMobile scored", "rTime hideMobile scored", "rVisitor hideMobile scored",
                      "rHome hideMobile scored"), 
                    function(x){
                      retrieve_column(x)
                    })
  
  # Find the minimum rows for each column
  plays <- min(sapply(1:length(columns), function(x){
    length(columns[[x]])
  }))
  
  # Scrape a game's play-by-play for the relevant columns
  game_pbp <- map_dfr(1:plays, function(x){
    play_info <- tibble(Game = game_num)
    play_info$PlayID <- 5*x
    play_info$Qtr <- unlist(columns[[1]][[x]]$getElementText())
    play_info$Time <- unlist(columns[[2]][[x]]$getElementText())
    play_info$Off <- unlist(columns[[3]][[x]]$getElementText())
    play_info$Situation <- unlist(columns[[4]][[x]]$getElementText())
    play_info$Description <- unlist(columns[[5]][[x]]$getElementText())
    play_info$DrivePlays <- unlist(columns[[6]][[x]]$getElementText())
    play_info$DriveYards <- unlist(columns[[7]][[x]]$getElementText())
    play_info$DriveTime <- unlist(columns[[8]][[x]]$getElementText())
    play_info$AwayScoreAfterDrive <- unlist(columns[[9]][[x]]$getElementText())
    play_info$HomeScoreAfterDrive <- unlist(columns[[10]][[x]]$getElementText())
    return(play_info)
  })
  
  return(game_pbp)
}

lapply(1:4, retrieve_games)

tibble(Game = 1:40) %>%
  mutate(Week = ceiling(game_num/4),
         GameID = if_else(game_num%%4 %in% c(1, 2),
                          str_remove_all(glue("{ymd(20200208) + (Week-1)*7}"), "-"),
                          str_remove_all(glue("{ymd(20200209) + (Week-1)*7}"), "-")),
         GameID = if_else(game_num%%2 == 1,
                          as.character(glue("{GameID}00")),
                          as.character(glue("{GameID}01"))))