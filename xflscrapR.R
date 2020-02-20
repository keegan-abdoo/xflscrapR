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
    play_info$play_id <- 5*x
    play_info$qtr <- as.integer(unlist(columns[[1]][[x]]$getElementText()))
    play_info$time <- as_hms(glue("00:{unlist(columns[[2]][[x]]$getElementText())}"))
    play_info$posteam <- unlist(columns[[3]][[x]]$getElementText())
    play_info$Situation <- unlist(columns[[4]][[x]]$getElementText())
    play_info$desc <- unlist(columns[[5]][[x]]$getElementText())
    play_info$DrivePlays <- as.integer(unlist(columns[[6]][[x]]$getElementText()))
    play_info$DriveYards <- as.integer(unlist(columns[[7]][[x]]$getElementText()))
    play_info$DriveTime <- as_hms(unlist(columns[[8]][[x]]$getElementText()))
    play_info$AwayScoreAfterDrive <- as.numeric(unlist(columns[[9]][[x]]$getElementText()))
    play_info$HomeScoreAfterDrive <- as.numeric(unlist(columns[[10]][[x]]$getElementText()))
    play_info$home_team <- home
    play_info$away_team <- away
    return(play_info)
  })
  
  return(game_pbp)
}

# Function that reads in pbp data and cleans/parses it
clean_data <- function(df){
  c_names <- c("Game", "play_id", "qtr", "time", "posteam", "Situation", "desc",
               "DrivePlays", "DriveYards", "DriveTime", "AwayScoreAfterDrive", 
               "HomeScoreAfterDrive", "home_team", "away_team")
  
  pbp1 <- df %>%
    select(c_names) %>%
    mutate(# Fix Game 7 issue where one drive has the wrong Offense
           posteam = if_else(Game == 7 & qtr == 4 & between(play_id, 655, 690), "DAL", posteam),
           # Add Defensive Team
           defteam = if_else(posteam == home_team, away_team, home_team),
           Week = ceiling(Game/4),
           # Create GameIDs in NFL's GSIS format
           game_id = if_else(Game%%4 %in% c(1, 2),
                            str_remove_all(glue("{ymd(20200208) + (Week-1)*7}"), "-"),
                            str_remove_all(glue("{ymd(20200209) + (Week-1)*7}"), "-")),
           game_id = if_else(Game%%2 == 1,
                            as.character(glue("{game_id}00")),
                            as.character(glue("{game_id}01"))),
           game_id = as.numeric(game_id),
           # GameTime
           quarter_seconds_remaining = as.numeric(str_extract(as.character(time), "(?<=:)[0-9]{2}")) * 60 +
             as.numeric(str_extract(as.character(time), "[0-9]{2}$")),
           half_seconds_remaining = if_else(qtr %in% c("2","4"), quarter_seconds_remaining, quarter_seconds_remaining + 900),
           game_seconds_remaining = quarter_seconds_remaining + ((4 - as.numeric(qtr)) * 900),
           # Extract Down and Distance
           down = str_extract(Situation, "^[1-4]"),
           ydstogo = as.numeric(str_extract(Situation, "(?<=\\& )[0-9]+")),
           # Convert Field Position to Yards from the end zone
           yardline_100 = yl_100(Situation, posteam),
           # Is it goal to go?
           goal_to_go = if_else(ydstogo == yardline_100, 1, 0),
           # Get Play Types
           play_type = case_when(str_detect(desc, "[1-3]pt attempt") ~ "extra point",
                                str_detect(desc, "(rush)|(kneel)") ~ "run",
                                str_detect(desc, "(pass)|(scramble)|(sack)|(spike)") ~ "dropback",
                                str_detect(desc, "punt") ~ "punt",
                                str_detect(desc, "kickoff") ~ "kickoff",
                                str_detect(str_to_lower(desc), "field goal") ~ "field goal",
                                str_detect(desc, "Aborted") ~ "aborted snap",
                                TRUE ~ "no play"),
           # Did the QB align in shotgun?
           shotgun = case_when(play_type %in% c("run", "dropback") ~ if_else(str_detect(desc, "Shotgun"), 1, 0)),
           # Did the offense Huddle?
           no_huddle = case_when(play_type %in% c("run", "dropback") ~ if_else(str_detect(desc, "No Huddle"), 1, 0)),
           # Was the pass a spike?
           qb_spike = case_when(play_type == "dropback" ~ if_else(str_detect(desc, "spike"), 1, 0)),
           # Was the play a QB Kneel?
           qb_kneel = case_when(play_type == "run" ~ if_else(str_detect(desc, "kneel"), 1, 0)),
           # Was the QB sacked?
           sack = case_when(play_type == "dropback" ~ if_else(str_detect(desc, "sack"), 1, 0)),
           # Did the QB Scramble?
           qb_scramble = case_when(play_type == "dropback" ~ if_else(str_detect(desc, "scramble"), 1, 0)),
           # Was there a pass attempt?
           pass_attempt = case_when(play_type == "dropback" ~ if_else(sack == 0 & qb_scramble == 0, 1, 0)),
           # Was the pass intercepted?
           interception = case_when(pass_attempt == 1 ~ 
                                        if_else(str_detect(str_to_lower(desc), "intercepted"), 1, 0)),
           # Was the pass completed?
           complete_pass = case_when(pass_attempt == 1 ~ 
                                    if_else(str_detect(desc, "incomplete") |
                                                !str_detect(desc, "[0-9]") |
                                                interception == 1, 0, 1)),
           # Was the pass thrown away?
           throwaway = case_when(pass_attempt == 1 ~ ifelse(str_detect(desc, "incomplete\\."), 1, 0)),
           # Pass Depth and Direction buckets
           pass_length = str_extract(desc, "(?<=pass (incomplete )?)(short)|(deep)"),
           pass_location = str_extract(desc, "(?<=pass (incomplete )?((short)|(deep)) )(left)|(middle)|(right)"),
           # Run Direction to get location and gap
           run_direction = str_extract(desc, "(?<=rush )([A-z]+\\s[A-z]+)+(?=\\s((to)|(out)|(for)))"),
           run_location = str_extract(run_direction, "(left)|(right)|(middle)"),
           run_gap = str_extract(run_direction, "(end)|(tackle)|(guard)|(middle)"),
           # Did the play result in a touchdown?
           touchdown = if_else(str_detect(str_to_lower(desc), "touchdown"), 1, 0),
           # Was there a fumble?
           fumble = if_else(str_detect(str_to_lower(desc), "fumble"), 1, 0),
           # Yards Gained from Scrimmage
           yards_gained = case_when(qb_spike == 1 | complete_pass == 0 ~ 0,
                                   play_type %in% c("run", "dropback") & fumble == 1 & 
                                       !str_detect(desc, "Aborted") ~ yardline_100 - 
                                       yl_100(str_extract(desc, "(?<= to )[A-Z]{2,3}\\s[0-9]{1,2}"), posteam),
                                   play_type == "dropback" & str_detect(desc, "Aborted") ~ yardline_100 - 
                                       yl_100(str_extract(desc, "(?<= to )[A-Z]{2,3}\\s[0-9]{1,2}"), posteam),
                                   play_type == "run" & str_detect(desc, "Aborted") ~ yardline_100 - 
                                       yl_100(str_extract(desc, "(?<= at the )[A-Z]{2,3}\\s[0-9]{1,2}"), posteam),
                                   play_type %in% c("run","dropback") ~ 
                                       as.numeric(str_extract(desc, "\\-?[0-9]{1,2}(?= yards)"))),
           # Did the play result in a first down? # is.na() handles first downs before penalties, might cause false positives
           first_down = if_else((posteam == lead(posteam) | is.na(lead(posteam))) & game_id == lead(game_id) & 
                                 play_type %in% c("run", "dropback") & (lead(down) == 1 | is.na(lead(down))) & 
                                 yards_gained >= ydstogo, 1, 0),
           # Did the offense turn the ball over?
           turnover = if_else(((posteam != lead(posteam) & game_id == lead(game_id) & 
                                    qtr == lead(qtr)) | interception == 1) &
                                  !(play_type %in% c("punt", "field goal", "extra point")), 1, 0),
           turnover_type = case_when(turnover == 1 & fumble == 1 ~ "Fumble",
                                    turnover == 1 & interception == 1 ~ "Interception",
                                    turnover == 1 & down == 4 ~ "Downs"),
           # Did the defense recover the fumble?
           fumble_lost = if_else(fumble == 1 & turnover == 1, 1, 0),
           # Did the Offense go for it on fourth down?
           fourth_down_decision = case_when(down == 4 ~ if_else(play_type %in% c("dropback", "run"), 1, 0)),
           # What Type of Extra Point did the offense attempt?
           extra_point_type = as.numeric(if_else(play_type == "extra point", 
                                                 str_extract(desc, "^[1-3]"), "NA")),
           # Extra point succesful?
           extra_point_conversion = case_when(play_type == "extra point" ~ 
                                               if_else(str_detect(desc, " successful"), 1, 0)),
           # Is this a penalty?
           penalty = if_else(str_detect(desc, "PENALTY"),1,0),
           # Fix missing down, distance, yardline, and goal to go for penalties
           down = case_when(penalty == 1 & lag(first_down) == 1 ~ "1",
                            penalty == 1 & lag(first_down) != 1 & lag(down) != "4" ~ as.character(as.numeric(lag(down,1)) + 1), # need better fix to avoid 5th down
                            TRUE ~ down),
           ydstogo = case_when(penalty == 1 & lag(first_down) == 1 ~ 10,
                                penalty == 1 & lag(first_down) != 1 ~ lag(ydstogo) - lag(yards_gained),
                                TRUE ~ ydstogo),
           yardline_100 = case_when(penalty == 1 ~ lag(yardline_100) - lag(yards_gained),
                                    TRUE ~ yardline_100),
           goal_to_go = if_else(ydstogo == yardline_100, 1, 0),
           # Tackle information
           solo_tackle = case_when(str_detect(desc, "[A-Z]\\.[A-z]+(\\'|\\-)?[A-z]+\\)") ~
                                     if_else(str_detect(desc, "\\([A-Z]\\.[A-z]+(\\'|\\-)?[A-z]+\\)"), 1, 0)),
           assist_tackle = case_when(str_detect(desc, "[A-Z]\\.[A-z]+(\\'|\\-)?[A-z]+\\)") ~
                                       if_else(str_detect(desc, ";") |
                                                 str_detect(desc, ", [A-Z]\\.[A-z]+(\\'|\\-)?[A-z]+\\)"), 1, 0)),
           tackle_for_loss = case_when(solo_tackle == 1 | assist_tackle == 1 ~ if_else(yards_gained < 0, 1, 0)),
           # # On who is the penalty?
           # penalty_team = case_when(penalty == 1 ~ gsub(".*PENALTY","",desc) %>% str_split(" ") %>% sapply( "[[", 3) %>% 
           #                           gsub(pattern = "[.]", replacement = "")
           #                        ),
           # # What kind of penalty?
           # penalty_type = case_when(penalty == 1 ~ gsub(".*PENALTY","",desc) %>% stri_extract_first_regex(pattern=c("[.].*,")) %>%
           #                           str_split(",") %>% sapply( "[[", 1) %>% gsub(pattern="([.] )",replacement="")
           #                         ),
           # # How many yards lost/gained?
           # PenaltyYards = case_when(penalty == 1 & penalty_team == posteam ~ -as.numeric(stri_extract_first_regex(gsub(".*PENALTY","",desc),pattern=c("\\-*\\d+\\.*\\d*"))),
           #                          penalty == 1 & penalty_team != posteam ~ as.numeric(stri_extract_first_regex(gsub(".*PENALTY","",desc),pattern=c("\\-*\\d+\\.*\\d*")))
           # ),
           # Extract passer name
           passer_player_name = case_when(play_type == "dropback" & qb_scramble != 1 ~ 
                                    str_extract(desc, "[A-Z]\\.[A-z]+(\\'|\\-)?[A-z]+")),
           # Extract receiver name
           receiver_player_name = case_when(play_type == "dropback" & qb_scramble != 1 ~
                                      str_extract(desc, "(?<=((to)|(for)) )[A-Z]\\.[A-z]+(\\'|\\-)?[A-z]+")),
           # Extract rusher name
           rusher_player_name = case_when(play_type == "run" | qb_scramble == 1 ~ 
                                    str_extract(desc, "[A-Z]\\.[A-z]+(\\'|\\-)?[A-z]+")),
           # Extract interceptor name
           interception_player_name = case_when(interception == 1 ~
                                                  str_extract(desc, "(?<=INTERCEPTED by )[A-Z]\\.[A-z]+(\\'|\\-)?[A-z]+")),
           # Extract Solo Tackle player name
           solo_tackle_player_name = case_when(solo_tackle == 1 ~
                                                  str_extract(desc, "(?<=\\()[A-Z]\\.[A-z]+(\\'|\\-)?[A-z]+")),
           # Extract Assist Tackle player names
           assist_tackle_1_player_name = case_when(assist_tackle == 1 ~
                                                     str_extract(desc, "(?<=\\()[A-Z]\\.[A-z]+(\\'|\\-)?[A-z]+")),
           assist_tackle_2_player_name = case_when(assist_tackle == 1 ~
                                                     str_extract(desc, "(?<=(;|,) )[A-Z]\\.[A-z]+(\\'|\\-)?[A-z]+(?=\\))")))
  
  return(pbp1)
}

add_air_yards <- function(df){
  # load in data from Moose Jester, @CFB_Moose on Twitter
  # process to match xflscrapR data
  air_yards <- gsheet2tbl('https://docs.google.com/spreadsheets/d/1Esh5R0zJFXlislSUngzm3GeLNDPLF4uckUknWFIoVZ0/edit#gid=0') %>%
    filter(WEEK %in% unique(df$Week)) %>%
    mutate(START = as_hms(glue("00:{substr(as.character(START),1,5)}")),
           pass_attempt = 1) %>%
    dplyr::select(GAMEID,WEEK,QTR,START,pass_attempt,`AIR YARDS`) %>%
    rename(Game = GAMEID, Week = WEEK, qtr = QTR, time = START, air_yards = `AIR YARDS`)
  
  # join air yards column
  df <- df %>%
    left_join(air_yards,by=c("Game","Week","qtr","time","pass_attempt"))  %>%
    mutate(yards_after_catch = case_when(complete_pass == 1 ~ yards_gained - air_yards))
  
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
        stop(glue("Selenium failed to launch after 10 retries. The final error is printed above. 
                   If this is a port already in use error, specify an open port through the browser_port parameter."))
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
#pbp2 <- add_nflscrapR_epa(pbp)


###### nflscrapR variables
add_nflscrapR_epa <- function(df){
  library(nflscrapR)
  cat(glue("WARNING: this relies on the nflscrapR model built exclusively on NFL data, not XFL data. When using these \\
            numbers, keep in mind that it will fail to capture differences between the two leagues. This should only be \\
            used until a XFL-specific EPA model is available."))
  df_ep <- nflscrapR::calculate_expected_points(df, "half_seconds_remaining", "yardline_100", "down", "ydstogo", "goal_to_go",
                                                td_value = 6.5) %>%
    mutate(epa = case_when(Touchdown == 1 & Turnover == 0 ~ 6.5 - ep,
                           Touchdown == 1 & Turnover == 1 ~ -6.5 - ep,
                           PlayType == "field goal" & FieldGoalMade == 1 ~ 3 - ep,
                           TRUE ~ if_else(Off == lead(Off), lead(ep) - ep, -lead(ep) - ep)
                  )
           )
  return(df_ep)
}

