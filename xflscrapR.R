library(glue)
library(RSelenium)
library(XML)
library(tidyverse)
library(lubridate)
library(magrittr)
library(stringi)

# Function that navigates to each game web page and scrapes the play by play
retrieve_games <- function(game_num){
  
  # Navigate to XFL Stats Page
  remDr$navigate(glue("https://stats.xfl.com/{game_num}"))
  
  Sys.sleep(4)
  # Click on play list tab
  remDr$findElement("xpath", "//h3[@id = 'pgPlayList']")$clickElement()
  
  # Retrieve Home and Away Team Codes
  away <- unlist(remDr$findElement("xpath", "//div[@data-bind = 'text: awayClubCode']")$getElementText())
  home <- unlist(remDr$findElement("xpath", "//div[@data-bind = 'text: homeClubCode']")$getElementText())
  
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
    play_info$HomeTeam <- home
    play_info$AwayTeam <- away
    return(play_info)
  })
  
  return(game_pbp)
}

# Function that reads in pbp data and cleans/parses it
clean_data <- function(df){
  pbp1 <- df %>%
    select(-c(15:ncol(df))) %>%
    mutate(Week = ceiling(Game/4),
           # Create GameIDs in NFL's GSIS format
           GameID = if_else(Game%%4 %in% c(1, 2),
                            str_remove_all(glue("{ymd(20200208) + (Week-1)*7}"), "-"),
                            str_remove_all(glue("{ymd(20200209) + (Week-1)*7}"), "-")),
           GameID = if_else(Game%%2 == 1,
                            as.character(glue("{GameID}00")),
                            as.character(glue("{GameID}01"))),
           # GameTime
           QuarterSecondsRemaining = unlist(lapply(Time, mins_to_seconds)),
           HalfSecondsRemaining = if_else(Qtr %in% c("2","4"), QuarterSecondsRemaining, QuarterSecondsRemaining + 900),
           GameSecondsRemaining = QuarterSecondsRemaining + ((4 - as.numeric(Qtr)) * 900),
           # Extract Down and Distance
           Down = str_extract(Situation, "^[1-4]"),
           Distance = str_extract(Situation, "(?<=\\& )[0-9]+"),
           # Convert Field Position to Yards from the end zone
           Yardline_100 = case_when(str_detect(Situation, "50$") ~ 50,
                                    str_extract(Situation, "[A-Z]+(?= [0-9]{1,2}$)") == Off ~ 
                                      100 - as.numeric(str_extract(Situation, "[0-9]{1,2}$")),
                                    TRUE ~ as.numeric(str_extract(Situation, "[0-9]{1,2}$"))),
           # Is it goal to go?
           GoalToGo = if_else(Distance == Yardline_100, 1, 0),
           # Get Play Types
           PlayType = case_when(str_detect(Description, "(rush)|(kneel)") ~ "run",
                                str_detect(Description, "(pass)|(scramble)|(sack)|(spike)") ~ "dropback",
                                str_detect(Description, "punt") ~ "punt",
                                str_detect(Description, "kickoff") ~ "kickoff",
                                str_detect(str_to_lower(Description), "field goal") ~ "field goal",
                                str_detect(Description, "[1-3]pt attempt") ~ "extra point",
                                str_detect(Description, "Aborted") ~ "aborted snap",
                                TRUE ~ "no play"),
           # Is pass a spike?
           Spike = if_else(str_detect(Description, "spike"), 1, 0),
           # Is pass a QB Kneel
           QBKneel = if_else(str_detect(Description, "kneel"), 1, 0),
           # Was the QB sacked?
           Sack = if_else(str_detect(Description, "sack"), 1, 0),
           # Did the QB Scramble?
           Scramble = if_else(str_detect(Description, "scramble"), 1, 0),
           # Was there a pass attempt?
           PassAttempt = if_else(Sack == 0 & Scramble == 0 & Spike == 0 & QBKneel == 0 & PlayType == "dropback", 1, 0),
           # Was the pass intercepted?
           Interception = if_else(str_detect(str_to_lower(Description), "intercepted"), 1, 0),
           # Was the pass completed?
           Complete = if_else(PassAttempt == 1 & !str_detect(Description, "incomplete") &
                                  Interception != 1, 1, 0),
           # Get yards gained on play
           YardsGained = case_when((PassAttempt == 1 & Complete == 0) | Spike == 1 | QBKneel == 1 ~ 0,
                                   str_detect(Description, "sacked to") ~
                                       as.numeric(str_extract(Description, "(?<=\\sfor\\s)\\-?\\[0-9]{1,2}")), # I believe this never triggers and the bottom case deals with sacks
                                   PlayType %in% c("run","dropback") ~ 
                                       as.numeric(stri_extract_last_regex(gsub("PENALTY.*","",Description),pattern=c("\\-*\\d+\\.*\\d*")))),
           # Is this a touchdown?
           Touchdown = if_else(str_detect(str_to_lower(Description), "touchdown"), 1, 0),
           # Extra point succesful?
           ExtraPointConverted = case_when(PlayType == "extra point" ~ if_else(str_detect(Description,"unsuccessful"),0,1)
                                           ),
           # Is this a penalty?
           Penalty = if_else(str_detect(Description,"PENALTY"),1,0),
           # On who is the penalty?
           PenaltyTeam = case_when(Penalty == 1 ~ gsub(".*PENALTY","",Description) %>% str_split(" ") %>% sapply( "[[", 3) %>% 
                                     gsub(pattern = "[.]", replacement = "")
                                  ),
           # What kind of penalty?
           PenaltyType = case_when(Penalty == 1 ~ gsub(".*PENALTY","",Description) %>% stri_extract_first_regex(pattern=c("[.].*,")) %>%
                                     str_split(",") %>% sapply( "[[", 1) %>% gsub(pattern="([.] )",replacement="")
                                   ),
           # How many yards lost/gained?
           PenaltyYards = case_when(Penalty == 1 & PenaltyTeam == Off ~ -as.numeric(stri_extract_first_regex(gsub(".*PENALTY","",Description),pattern=c("\\-*\\d+\\.*\\d*"))),
                                    Penalty == 1 & PenaltyTeam != Off ~ as.numeric(stri_extract_first_regex(gsub(".*PENALTY","",Description),pattern=c("\\-*\\d+\\.*\\d*")))
           ),
           # Extract passer name
           PasserName = case_when(PassAttempt == 1 ~ stri_extract_first_regex(Description,pattern=c("[A-Z][.]([a-zA-Z]+)"))),
           # Extract receiver name
           ReceiverName = case_when(PassAttempt == 1 & Interception == 0 ~
                                      stri_extract_last_regex(Description,pattern=c(" [A-Z][.]([a-zA-Z]+)")) %>% str_trim(),
                                    PassAttempt == 1 & Interception == 1 ~
                                      stri_extract_last_regex(gsub("INTERCEPTED.*","",Description),pattern=c("[A-Z][.]([a-zA-Z]+)"))
                                    ),
           # Extract rusher name
           RunnerName = case_when(PlayType == "run" ~ stri_extract_first_regex(Description,pattern=c("[A-Z][.]([a-zA-Z]+)")) )
           )
  
  return(pbp1)
}

# Function that can read in the most recently updated version of xflscrapR's pbp data, and will update
# automatically with the most recent data if more games have been played
xfl_scrapR <- function(){
  pbp <- read_csv(url("https://raw.githubusercontent.com/keegan-abdoo/xflscrapR/master/play_by_play_data/regular_season/reg_pbp_2020.csv"))
  
  most_recent_game <- if_else(wday(as_date(Sys.time())) == 7, 2, 4) + 
    4*floor((as.numeric(as_date(Sys.time())) - as.numeric(ymd(20200208)))/7)
  
  last_game <- max(pbp$Game)
  
  if(most_recent_game > last_game){
    # Open Web Browser
    driver <- rsDriver(browser = "firefox")
    remDr <- driver[["client"]]
    
    # Scrape new games
    pbp1 <- map_dfr(last_game:most_recent_game, retrieve_games)
    
    # Clean Data
    pbp1 %<>% clean_data(pbp1)
    
    # Join to old data
    pbp <<- bind_rows(pbp, pbp1)
    
    # Close Browser when done
    remDr$close()
    driver[["server"]]$stop()
  }
  # Return up-to-date and cleaned play-by-play data
  return(pbp)
}

pbp <- xfl_scrapR()
#pbp2 <- add_nflscrapR_epa(pbp1)



###### nflscrapR variables
add_nflscrapR_epa <- function(df){
  library(nflscrapR)
  cat("WARNING: this relies on the nflscrapR model built exclusively on NFL data, not XFL data. When using these numbers, keep in mind that it will fail to capture differences between the two leagues. This should only be used until a XFL-specific EPA model is available.")
  df_ep <- nflscrapR::calculate_expected_points(df,"HalfSecondsRemaining","Yardline_100","Down","Distance","GoalToGo") %>%
    mutate(epa = if_else(Off==lead(Off,1),lead(ep,1) - ep, -lead(ep,1) - ep))
  return(df_ep)
}

