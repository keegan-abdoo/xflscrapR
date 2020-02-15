# Scraping and Cleaning XFL Play-By-Play Data with RSelenium

This is my second tutorial on how to scrape football data with RSelenium (find the first one on scraping weekly Next Gen Stats data [here](https://github.com/keegan-abdoo/Public-Next-Gen-Stats-Weekly-Data/blob/master/ngspublicscraping.md)). Unlike the first tutorial, the data is not stored in a HTML table, so this is a good example getting more hands-on with your scraper.

To reiterate, the RSelenium package allows you to open a web browser, interact with webpages, and extract data from the underlying html that builds the websites all through your code. Again, I suggest following along with this [RSelenium Baics Primer](https://rpubs.com/johndharrison/RSelenium-Basics).

Currently, the [XFL Stats play-by-play page](https://stats.xfl.com/1) only contains situational information and a play description, although the XFL's director of Football Ops, **Sam Schwartzstein** [@schwartzsteins] (https://twitter.com/schwartzsteins), indicated an [interest in potentially adding expected points](https://twitter.com/benbbaldwin/status/1227962146044153859?s=20) to the website eventually.  Anthony Reinhard (@reinhurdler)[https://twitter.com/reinhurdler] has done some work to manipulate the data into variables that can be used with the [nflscrapR EPA calculator](https://github.com/ajreinhard/XFL-public/blob/master/epa/xfl%20parse%20plays.R), but I'm going to leave that up to you if you want to use that.  Personally, I'm skeptical about the usefulness of applying an expected points model built off historical NFL data to a completely different league, but I digress.

Anyway, let's dive into the scraper. I have put in a function that will read in the existing play-by-play and update it with cleaned data based on the current date. If you don't feel like learning RSelenium and just want to get your hands on the XFL data, feel free to just call in the play-by-play data and use the xflscrapR Rscript to update it if need be.

-- Keegan Abdoo, [@KeeganAbdoo](https://twitter.com/KeeganAbdoo)

## Load Packages

If you haven't already installed any of these packages on your computer, you will need to run the R function install.packages("packagename") before you can load them from your library.

``` r
library(glue)
library(RSelenium)
library(XML)
library(tidyverse)
library(lubridate)
library(magrittr)
```

## Remote Drivers and Web Elements

Before we get into the actual code, I'm going to briefly explain the idea of remote drivers and web elements.

The function rsDriver opens up a specified browser on your computer (I use firefox here) and starts a selenium server.  It returns a list (which we name "driver") that contains the server and a client.  We can then isolate the client and create a object of class remoteDriver (which we name "remDr"). Oversimplifying this, the remoteDriver allows us to interact with and control the browser from our R console.

```r
driver <- rsDriver(browser = c("firefox"))
remDr <- driver[["client"]]
```
The first step of our function will be to navigate to the supplied url.  It does this by using a method from the remoteDriver object.  Methods are basically the functions that you attach (with a "$") to a remoteDriver to interact with the browser. They include being able to navigate to specific urls, move our mouse to a specified location, screenshot the page, change the window size, etc.  

The most useful methods in my experience, are those that allow you to interact with elements on the page such as filters and search boxes.  Elements are basically just different HTML objects, and we can search for them specifically using xpaths within a findElement() method (here's a great [tutorial for using xpath in a Selenium](https://www.guru99.com/xpath-selenium.html)). What is returned is an object of class webElement (or list of webElements if there are multiple elements with the same xpath, in which case you use the plural findElement**s**()).

webElement objects, like remoteDriver objects, can be appended by methods. The webElement methods can be used to scrape data (i.e. getElementAttribute()) and also to click and select on filters. For instance, this will be how we navigate to the playlist tab on the xfl stats page in our scraper function:

```r
# Navigate to XFL Stats Page
remDr$navigate(glue("https://stats.xfl.com/{game_num}"))
# Click on play list tab
remDr$findElement("xpath", "//h3[@id = 'pgPlayList']")$clickElement()
```
To close a Selenium browser, simply use the close method on the remoteDriver, and then stop the server with a method on the driver.

```r
remDr$close()
driver[["server"]]$stop()
```

## Scraping Function

The first step of my code is to construct the scraping function.  This function will navigate to the stats page for a given game, and extract the play-by-play data, iterating by row. It's nested within the data-loading function later, as it will scrape the webpage only if the most recent games' data doesn't exist on the existing play-by-play file in the github repository.

Let's take this chunk by chunk:

The first line of code, for those unfamilar with functions, names the function and sets the parameters, which in this case is the game number.  The XFL has a very simple syntax for their urls, with the game number at the end. The glue function allows us to pass the parameter into the url for each game.
``` r
retrieve_games <- function(game_num){

  remDr$navigate(glue("https://stats.xfl.com/{game_num}"))
``` 
The next line of code is our first interaction with a webElement.  When inspecting a webpage, right click on the element and select inspect to find the html code behind the element.  In this case, the we find that the playlist tab is of a h3 object.  We can identify it by looking at the attributes of the element, in this case the id is 'pgPlayList'.  Using xpath code in the findElement method, we can retrieve the html for this object, and in this case we want to click it, so we use the clickElement method.

The Sys.sleep(4) command is included to force R to pause for 4 seconds, which allows the webpage to load properly and ensure that the playlist tab element is found.

``` r
  Sys.sleep(4)
  remDr$findElement("xpath", "//h3[@id = 'pgPlayList']")$clickElement()
```
We are combining the play-by-play tables from multiple games, so the title of the home and away score columns won't include the team (even though that's how the table is displayed on the webpage).  Instead, we are going to create our own column for home and away team in each game that we can mutate to get a column for each team's score.  We retrieve the home and away team codes by finding the element for the column headers through their data-bind attribute and using the getElementText() method.

``` r
  away <- unlist(remDr$findElement("xpath", "//div[@data-bind = 'text: awayClubCode']")$getElementText())
  home <- unlist(remDr$findElement("xpath", "//div[@data-bind = 'text: homeClubCode']")$getElementText())
``` 
The structure of the html has everything containted in a div object of class "table totalPlaylist".  From there, the child elements are a div of class "head" for the header row and then each play is contained in a div of class "body".  Each row object has a group of descendents of div objects which has a class named after the column.  Each of these elements contains the text attribute that we want to return.

My strategy here is to return a list for each class/column under the body objects. Once we have that, it's easy to construct a data frame row by row from the data in the table.

To compress the code, I've created a helper function that will retrieve a list of webElements for a given column by identifying it by the class name. Notice that we have an if statement to deal with the play description column, which doesn't directly contain the text attribute.  We have to skip down to it's child element, the div identified by a data-bind called "text: PlayDescription, updatedClass: 'highlight'".  For brevity sake, we will utilize the xpath function contains and just search for "PlayDescription".

``` r
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
```
With the helper function, we can then read in a vector of the column class names through a lapply into the helper function to return a list of webElement lists for each column.  The purpose of this is to store the lists of webElements outside the row-by-row iteration so it doesn't have to pull the webElement element list with each row.

``` r
  columns <- lapply(c("rQtr", "rStart", "rPoss", "rPossDown", "rPlayDesc", "rPlays hideMobile scored",
                      "rYards hideMobile scored", "rTime hideMobile scored", "rVisitor hideMobile scored",
                      "rHome hideMobile scored"), 
                    function(x){
                      retrieve_column(x)
                    })
```
For reason not exactly clear to me, some of the columns return a list of webElements that are longer than the actual amount of plays. So here we just take the minimum length of the webElement lists to make sure we don't get a "subscript out of bounds"" error.

``` r
  plays <- min(sapply(1:length(columns), function(x){
    length(columns[[x]])
  }))
```
Next we use that minimum length to construct a sequence vector **(1:plays)** that we read through the map_dfr function to construct each row of our final data frame.  The map_dfr function returns a data frame that is created by row binding ("df" for data frame and "r" for by row after the underscore). It takes each value in the supplied vector and reads it through a helper function, which we create below.

A few things on the helper function in case anyone is unfamiliar any of the functions/methods used.  The tibble() function is used to create a data frame, with the first column of Game being asigned the game number passed in through the retrieve_games function that this is nested in. I also created custom play_id's, which I have defined as the row number muliplied by 5 (a relic of my experience at Sports Info Solutions haha).  I retrieve the value in each column by using the getElementText() method.  This is nested in an unlist() function to avoid the value being returned as a list.  The home and away team columns are also assigned based on the values we pulled earlier.

``` r
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
```
Now that we have the infastructure built to scrape the play-by-play from each game, we create a cleaner function that will manipulate the pulled data to match the structure of the existing csv in the github.

Let's take a look at the first few chunks of this function:

First we are setting the parameter to be a data frame.  This data frame will almost always be the scraped play-by-play, but if we want to update the cleaning function with new columns to build, we also have the capability to do that with our select function.  The scraped data frame has 14 columns, so we make sure to exclude any columns indexed 15 or greater (-c(15:ncol(df))).
``` r
clean_data <- function(df){
  pbp1 <- df %>%
    select(-c(15:ncol(df))) %>%
```
The mutate statement is where we will start creating the new columns we want.  

First we get a week number, which is accomplished by taking the ceiling of the game_num divided by 4.  We use this Week number in creating the GameID column, which I converted to be in the same format as the GSIS GameIDs that the NFL uses.  

The %% operator gives the remainder by division, so in this case we are using it to find the game number within each week.  Because the XFL schedule has two games on Saturday and two games on Sunday, the remainders of 1 and 2 of game_num divided by 4 represent Saturday games, while 3 and 4 represent Sunday games.  We can then 

``` r
    mutate(Week = ceiling(Game/4),
           # Create GameIDs in NFL's GSIS format
           GameID = if_else(Game%%4 %in% c(1, 2),
                            str_remove_all(glue("{ymd(20200208) + (Week-1)*7}"), "-"),
                            str_remove_all(glue("{ymd(20200209) + (Week-1)*7}"), "-")),
           GameID = if_else(Game%%2 == 1,
                            as.character(glue("{GameID}00")),
                            as.character(glue("{GameID}01"))),
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
                                str_detect(Description, "(pass)|(scramble)|(sack)|(spike)") ~ "pass",
                                str_detect(Description, "punt") ~ "punt",
                                str_detect(Description, "kickoff") ~ "kickoff",
                                str_detect(str_to_lower(Description), "field goal") ~ "field goal",
                                str_detect(Description, "[1-3]pt attempt") ~ "extra point",
                                str_detect(Description, "Aborted") ~ "aborted snap",
                                TRUE ~ "no play"))
  
  return(pbp1)
}
```

In the actual xflscrapR script, we have the code to open the remote driver in the function that scrapes the data instead because the scraping is conditional on whether the file has been updated in the github repository.
