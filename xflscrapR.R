library(glue)
library(RSelenium)
library(XML)
library(tidyverse)
library(hms)
library(lubridate)
library(magrittr)
library(stringi)
library(gsheet)
source("helpers.R")

# Function that navigates to each game web page and scrapes the play by play
retrieve_games <- function(game_num,driver){
  
  # Navigate to XFL Stats Page
  driver$navigate(glue("https://stats.xfl.com/{game_num}"))
  
  Sys.sleep(4)
  # Click on play list tab
  driver$findElement("xpath", "//h3[@id = 'pgPlayList']")$clickElement()
  
  # Retrieve Home and Away Team Codes
  away <- unlist(driver$findElement("xpath", "//div[@data-bind = 'text: awayClubCode']")$getElementText())
  home <- unlist(driver$findElement("xpath", "//div[@data-bind = 'text: homeClubCode']")$getElementText())
  
  # Create function that returns a list of webElements containing each row value in a column
  retrieve_column <- function(column_name){
    if(column_name == "rPlayDesc"){
      driver$findElements("xpath", glue("//div[@class = 'table totalPlaylist']\\
                                      //child::div[@class = 'body']\\
                                      //child::div[@class = '{column_name}']\\
                                      //child::div[contains(@data-bind, 'PlayDescription')]"))
    } else {
      driver$findElements("xpath", glue("//div[@class = 'table totalPlaylist']\\
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
    play_info$Qtr <- as.integer(unlist(columns[[1]][[x]]$getElementText()))
    play_info$Time <- as_hms(glue("00:{unlist(columns[[2]][[x]]$getElementText())}"))
    play_info$Off <- unlist(columns[[3]][[x]]$getElementText())
    play_info$Situation <- unlist(columns[[4]][[x]]$getElementText())
    play_info$Description <- unlist(columns[[5]][[x]]$getElementText())
    play_info$DrivePlays <- as.integer(unlist(columns[[6]][[x]]$getElementText()))
    play_info$DriveYards <- as.integer(unlist(columns[[7]][[x]]$getElementText()))
    play_info$DriveTime <- as_hms(unlist(columns[[8]][[x]]$getElementText()))
    play_info$AwayScoreAfterDrive <- as.numeric(unlist(columns[[9]][[x]]$getElementText()))
    play_info$HomeScoreAfterDrive <- as.numeric(unlist(columns[[10]][[x]]$getElementText()))
    play_info$HomeTeam <- home
    play_info$AwayTeam <- away
    return(play_info)
  })
  
  return(game_pbp)
}

# Function that reads in pbp data and cleans/parses it
clean_data <- function(df){
  c_names <- c("Game", "PlayID", "Qtr", "Time", "Off", "Situation", "Description",
               "DrivePlays", "DriveYards", "DriveTime", "AwayScoreAfterDrive", 
               "HomeScoreAfterDrive", "HomeTeam", "AwayTeam")
  
  pbp1 <- df %>%
    select(c_names) %>%
    mutate(# Fix Game 7 issue where one drive has the wrong Offense
           Off = if_else(Game == 7 & Qtr == 4 & between(PlayID, 655, 690), "DAL", Off),
           # Add Defensive Team
           Def = if_else(Off == HomeTeam, AwayTeam, HomeTeam),
           Week = ceiling(Game/4),
           # Create GameIDs in NFL's GSIS format
           GameID = if_else(Game%%4 %in% c(1, 2),
                            str_remove_all(glue("{ymd(20200208) + (Week-1)*7}"), "-"),
                            str_remove_all(glue("{ymd(20200209) + (Week-1)*7}"), "-")),
           GameID = if_else(Game%%2 == 1,
                            as.character(glue("{GameID}00")),
                            as.character(glue("{GameID}01"))),
           GameID = as.numeric(GameID),
           # GameTime
           QuarterSecondsRemaining = as.numeric(str_extract(as.character(Time), "(?<=:)[0-9]{2}")) * 60 +
             as.numeric(str_extract(as.character(Time), "[0-9]{2}$")),
           HalfSecondsRemaining = if_else(Qtr %in% c("2","4"), QuarterSecondsRemaining, QuarterSecondsRemaining + 900),
           GameSecondsRemaining = QuarterSecondsRemaining + ((4 - as.numeric(Qtr)) * 900),
           # Extract Down and Distance
           Down = str_extract(Situation, "^[1-4]"),
           Distance = as.numeric(str_extract(Situation, "(?<=\\& )[0-9]+")),
           # Convert Field Position to Yards from the end zone
           Yardline_100 = yl_100(Situation, Off),
           # Is it goal to go?
           GoalToGo = if_else(Distance == Yardline_100, 1, 0),
           # Get Play Types
           PlayType = case_when(str_detect(Description, "[1-3]pt attempt") ~ "extra point",
                                str_detect(Description, "(rush)|(kneel)") ~ "run",
                                str_detect(Description, "(pass)|(scramble)|(sack)|(spike)") ~ "dropback",
                                str_detect(Description, "punt") ~ "punt",
                                str_detect(Description, "kickoff") ~ "kickoff",
                                str_detect(str_to_lower(Description), "field goal") ~ "field goal",
                                str_detect(Description, "Aborted") ~ "aborted snap",
                                TRUE ~ "no play"),
           # Did the QB align in shotgun?
           Shotgun = case_when(PlayType %in% c("run", "dropback") ~ if_else(str_detect(Description, "Shotgun"), 1, 0)),
           # Did the offense Huddle?
           NoHuddle = case_when(PlayType %in% c("run", "dropback") ~ if_else(str_detect(Description, "No Huddle"), 1, 0)),
           # Is pass a spike?
           Spike = case_when(PlayType == "dropback" ~ if_else(str_detect(Description, "spike"), 1, 0)),
           # Is pass a QB Kneel
           QBKneel = case_when(PlayType == "run" ~ if_else(str_detect(Description, "kneel"), 1, 0)),
           # Was the QB sacked?
           Sack = case_when(PlayType == "dropback" ~ if_else(str_detect(Description, "sack"), 1, 0)),
           # Did the QB Scramble?
           Scramble = case_when(PlayType == "dropback" ~ if_else(str_detect(Description, "scramble"), 1, 0)),
           # Was there a pass attempt?
           PassAttempt = case_when(PlayType == "dropback" ~ if_else(Sack == 0 & Scramble == 0, 1, 0)),
           # Was the pass intercepted?
           Interception = case_when(PassAttempt == 1 ~ 
                                        if_else(str_detect(str_to_lower(Description), "intercepted"), 1, 0)),
           # Was the pass completed?
           Complete = case_when(PassAttempt == 1 ~ 
                                    if_else(str_detect(Description, "incomplete") |
                                                !str_detect(Description, "[0-9]") |
                                                Interception == 1, 0, 1)),
           # Was the pass thrown away?
           Throwaway = case_when(PassAttempt == 1 ~ ifelse(str_detect(Description, "incomplete\\."), 1, 0)),
           # Pass Depth and Direction buckets
           PassDepth = str_extract(Description, "(?<=pass (incomplete )?)(short)|(deep)"),
           PassDirection = str_extract(Description, "(?<=pass (incomplete )?((short)|(deep)) )(left)|(middle)|(right)"),
           # Run Gap
           RunDirection = str_extract(Description, "(?<=rush )([A-z]+\\s[A-z]+)+(?=\\s((to)|(out)|(for)))"),
           # Did the play result in a touchdown?
           Touchdown = if_else(str_detect(str_to_lower(Description), "touchdown"), 1, 0),
           # Was there a fumble?
           Fumble = if_else(str_detect(str_to_lower(Description), "fumble"), 1, 0),
           # Yards Gained from Scrimmage
           YardsGained = case_when(Spike == 1 | Complete == 0 ~ 0,
                                   PlayType %in% c("run", "dropback") & Fumble == 1 & 
                                       !str_detect(Description, "Aborted") ~ Yardline_100 - 
                                       yl_100(str_extract(Description, "(?<= to )[A-Z]{2,3}\\s[0-9]{1,2}"), Off),
                                   PlayType == "dropback" & str_detect(Description, "Aborted") ~ Yardline_100 - 
                                       yl_100(str_extract(Description, "(?<= to )[A-Z]{2,3}\\s[0-9]{1,2}"), Off),
                                   PlayType == "run" & str_detect(Description, "Aborted") ~ Yardline_100 - 
                                       yl_100(str_extract(Description, "(?<= at the )[A-Z]{2,3}\\s[0-9]{1,2}"), Off),
                                   PlayType %in% c("run","dropback") ~ 
                                       as.numeric(str_extract(Description, "\\-?[0-9]{1,2}(?= yards)"))),
           #as.numeric(stri_extract_last_regex(Description,pattern=c("\\-*\\d+\\.*\\d*")))),
           # Did the play result in a first down? # is.na() handles first downs before penalties, might cause false positives
           FirstDown = if_else((Off == lead(Off) | is.na(lead(Off))) & GameID == lead(GameID) & PlayType %in% c("run", "dropback") &
                                   (lead(Down) == 1 | is.na(lead(Down))) & YardsGained >= Distance, 1, 0),
           # Did the offense turn the ball over?
           Turnover = case_when(((Off != lead(Off) & GameID == lead(GameID) & 
                                    Qtr == lead(Qtr)) | Interception == 1) &
                                  !(PlayType %in% c("punt", "field goal", "extra point")) ~ 1,
                                TRUE ~ 0),
           TurnoverType = case_when(Turnover == 1 & Fumble == 1 ~ "Fumble",
                                    Turnover == 1 & Interception == 1 ~ "Interception",
                                    Turnover == 1 & Down == 4 ~ "Downs"),
           # Did the defense recover the fumble?
           FumbleLost = if_else(Fumble == 1 & Turnover == 1, 1, 0),
           # Did the Offense go for it on fourth down?
           FourthDownDecision = case_when(Down == 4 ~ if_else(PlayType %in% c("dropback", "run"), 1, 0)),
           # What Type of Extra Point did the offense attempt?
           ExtraPointType = as.numeric(if_else(PlayType == "extra point", 
                                               str_extract(Description, "^[1-3]"), "NA")),
           # Extra point succesful?
           ExtraPointConverted = case_when(PlayType == "extra point" ~ 
                                               if_else(str_detect(Description, " successful"), 1, 0)),
           # Is this a penalty?
           Penalty = if_else(str_detect(Description,"PENALTY"),1,0),
           # Fix missing down, distance, yardline, and goal to go for penalties
           Down = case_when(Penalty == 1 & lag(FirstDown) == 1 ~ "1",
                            Penalty == 1 & lag(FirstDown) != 1 & lag(Down) != "4" ~ as.character(as.numeric(lag(Down,1)) + 1), # need better fix to avoid 5th down
                            TRUE ~ Down),
           Distance = case_when(Penalty == 1 & lag(FirstDown) == 1 ~ 10,
                                Penalty == 1 & lag(FirstDown) != 1 ~ lag(Distance) - lag(YardsGained),
                                TRUE ~ Distance),
           Yardline_100 = case_when(Penalty == 1 ~ lag(Yardline_100) - lag(YardsGained),
                                    TRUE ~ Yardline_100),
           GoalToGo = if_else(Distance == Yardline_100, 1, 0),
           # # On who is the penalty?
           # PenaltyTeam = case_when(Penalty == 1 ~ gsub(".*PENALTY","",Description) %>% str_split(" ") %>% sapply( "[[", 3) %>% 
           #                           gsub(pattern = "[.]", replacement = "")
           #                        ),
           # # What kind of penalty?
           # PenaltyType = case_when(Penalty == 1 ~ gsub(".*PENALTY","",Description) %>% stri_extract_first_regex(pattern=c("[.].*,")) %>%
           #                           str_split(",") %>% sapply( "[[", 1) %>% gsub(pattern="([.] )",replacement="")
           #                         ),
           # # How many yards lost/gained?
           # PenaltyYards = case_when(Penalty == 1 & PenaltyTeam == Off ~ -as.numeric(stri_extract_first_regex(gsub(".*PENALTY","",Description),pattern=c("\\-*\\d+\\.*\\d*"))),
           #                          Penalty == 1 & PenaltyTeam != Off ~ as.numeric(stri_extract_first_regex(gsub(".*PENALTY","",Description),pattern=c("\\-*\\d+\\.*\\d*")))
           # ),
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

add_air_yards <- function(df){
  # load in data from Moose Jester, @CFB_Moose on Twitter
  # process to match xflscrapR data
  air_yards <- gsheet2tbl('https://docs.google.com/spreadsheets/d/1Esh5R0zJFXlislSUngzm3GeLNDPLF4uckUknWFIoVZ0/edit#gid=0') %>%
    filter(WEEK %in% unique(df$Week)) %>%
    mutate(START = as_hms(glue("00:{substr(as.character(START),1,5)}")),
           PassAttempt = 1) %>%
    dplyr::select(GAMEID,WEEK,QTR,START,PassAttempt,`AIR YARDS`) %>%
    rename(Game = GAMEID, Week = WEEK, Qtr = QTR, Time = START, AirYards = `AIR YARDS`)
  
  # join air yards column
  df <- df %>%
    left_join(air_yards,by=c("Game","Week","Qtr","Time","PassAttempt"))
  
  # return finished df
  return(df)
}

# Function that can read in the most recently updated version of xflscrapR's pbp data, and will update
# automatically with the most recent data if more games have been played
xfl_scrapR <- function(browser_port=NA){
  pbp <- read_csv(url("https://raw.githubusercontent.com/keegan-abdoo/xflscrapR/master/play_by_play_data/regular_season/reg_pbp_2020.csv")) %>%
      clean_data() %>%
      add_air_yards()

  most_recent_game <- if_else(wday(as_date(Sys.time())) == 7, 2, 4) +
    4*floor((as.numeric(as_date(Sys.time())) - as.numeric(ymd(20200208)))/7)

  last_game <- max(pbp$Game)
  
  if(most_recent_game > last_game){
    # Open Web Browser
    if (is.na(browser_port)){
      try_browser<-try(driver <- rsDriver(browser = "firefox"),silent = TRUE)
    } else {
      try_browser<-try(driver <- rsDriver(browser = "firefox",port=as.integer(browser_port)),silent = TRUE)
    }
    
    # Catch error on browser launch
    tries <- 0
    port_range <- as.integer(seq(4567,10000))
    while (class(try_browser) =="try-error") {
      tries <- tries + 1
      # timeout if too many tries
      if (tries >= 10){
        cat(try_browser[1])
        stop("Selenium failed to launch after 10 retries. The final error is printed above. \n If this is a port already in use error, specify an open port through the browser_port parameter.")
      }
      
      # port in use error
      if (str_detect(try_browser[1],"already in use")){
        cat("Re-trying to launch Selenium with different port.\n")
        port <- sample(port_range,1)
        try_browser<-try(driver <- rsDriver(browser = "firefox",port=port),silent = TRUE)
      }

      # TO-DO: handle other errors
    }
    
    # assign client driver
    remDr <- driver[["client"]]
  
    # Scrape new games
    pbp1 <- map_dfr((last_game+1):most_recent_game, retrieve_games, driver=remDr)
    
    # Clean Data
    pbp1 %<>% clean_data()
    
    # Add air yards
    pbp1 %<>% add_air_yards()
    
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
pbp2 <- add_nflscrapR_epa(pbp)


###### nflscrapR variables
add_nflscrapR_epa <- function(df){
  library(nflscrapR)
  cat("WARNING: this relies on the nflscrapR model built exclusively on NFL data, not XFL data. When using these numbers, keep in mind that it will fail to capture differences between the two leagues. This should only be used until a XFL-specific EPA model is available.")
  df_ep <- nflscrapR::calculate_expected_points(df_pens,"HalfSecondsRemaining","Yardline_100","Down","Distance","GoalToGo") %>%
    mutate(epa = case_when(Touchdown == 1 & Turnover == 0 ~ 7 - ep,
                           Touchdown == 1 & Turnover == 1 ~ -7 - ep,
                           TRUE ~ if_else(Off==lead(Off),lead(ep) - ep, -lead(ep) - ep)
                  )
           )
  return(df_ep)
}

