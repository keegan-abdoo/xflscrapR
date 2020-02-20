# Scraping and Cleaning XFL Play-By-Play Data with RSelenium

This is my second tutorial on how to scrape football data with RSelenium (find the first one on scraping weekly Next Gen Stats data [here](https://github.com/keegan-abdoo/Public-Next-Gen-Stats-Weekly-Data/blob/master/ngspublicscraping.md)). Unlike the first tutorial, the data is not stored in a HTML table, so this is a good example getting more hands-on with your scraper. I also include explanation behind the data cleaning function, which Caio Brighenti [@CaioBrighenti2](https://twitter.com/CaioBrighenti2) has helped put together.

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

DISCLAIMER: This was brought to my attention by Ben Baldwin (@benbbaldwin)[https://twitter.com/benbbaldwin].  The rsDriver function will throw a error if your downloaded Java is not the same version as your R. If you are working with a 64-bit R build, you need a 64-bit of Java (which you can download here)[https://java.com/en/download/manual.jsp].

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

## Cleaning Function

Now that we have the infastructure built to scrape the play-by-play from each game, we create a cleaner function that will manipulate the pulled data to match the structure of the existing csv in the github.

Let's take a look at the first few chunks of this function:

First we are setting the parameter to be a data frame.  This data frame will almost always be the scraped play-by-play, but if we want to update the cleaning function with new columns to build, we also have the capability to do that with our select function.  The scraped data frame has 14 columns, so we make sure to exclude any columns indexed 15 or greater (-c(15:ncol(df))).
``` r
clean_data <- function(df){
  pbp1 <- df %>%
    select(-c(15:ncol(df))) %>%
```
The mutate statement is where we will start creating the new columns we want.  

First we get a week number, which is accomplished by taking the ceiling of the game_num divided by 4.  We use this Week number in creating the GameID column, which I converted to be in the same format as the GSIS GameIDs that the NFL uses ({Year}{Month}{Day}{Game#ofDay}).

The %% operator gives the remainder by division, so in this case we are using it to find the game number within each week.  Because the XFL schedule has two games on Saturday and two games on Sunday, the remainders of 1 and 2 of game_num divided by 4 represent Saturday games, while 3 and 4 represent Sunday games.  We can then add the correct multiple of 7 days based on our Week variable to the original dates from Week 1 (February 8/9).  Because the ymd function from the lubridate package outputs the date with "-"s separating year, month and day, we use stringr's str_remove_all function to remove the "-".  Finally, we take one more remainder to find if it's the first or second game in a day, and attach that to the end of the GameID string.

``` r
    mutate(Week = ceiling(Game/4),
           GameID = if_else(Game%%4 %in% c(1, 2),
                            str_remove_all(glue("{ymd(20200208) + (Week-1)*7}"), "-"),
                            str_remove_all(glue("{ymd(20200209) + (Week-1)*7}"), "-")),
           GameID = if_else(Game%%2 == 1,
                            as.character(glue("{GameID}00")),
                            as.character(glue("{GameID}01"))),
```
Next we can extract the down, distance, and field position information from the situation column.  We are accomplishing this with some regex through the stringr package, so I'll explain what's going on here. I also highly recommend pulling up the [stringr cheatsheet](http://edrub.in/CheatSheets/cheatSheetStringr.pdf) whenever you are trying to manipulate text strings.

The str_extract function allows you to return a substring of any string.  Since the Down value is the first number in any column, we identify that through the "^" anchor regular expression.  This simply refers to the first character in a string.  Since there are only 4 downs, we can specify [1-4] as the characters we want to extract.

The distance to go value is found in the middle of the situation string, and it always comes after the ampersand (&) symbol. To extract this, we can use what is called a look-around: "(?<=)", or precedy by. In its use here, we put the ampersand and a space inside the look around, followed by a "[0-9]+" outside the parentheses.  This regular expression translates to return any string of consecutive digits that is preceded by an ampersand and a space.

The distance from the yardline value is extracted with a case_when function.  This function allows us to specify values for a new variable conditionally. The situation string contains field position based on side of the field, so we need to cross reference that value with the Offensive Team variable.  

Our first condition in the case when will deal with when the ball is on the 50 yard line, because by default it isn't on either the offense or defenses side of the field. We accomplish this with a str_detect() function, which returns TRUE if it finds the expression in the given string. Notice we are using the "$" anchor, which is directs to the last character in a string. The next condition extracts the side of the field, again using a look around: "(?=)", or followed by. This time we use " [0-9]{1,2}" with the anchor at the end inside the look around, which follows [A-Z]+.  This translates to extract any string of consecutive letters that is followed by a space, and one or two digits at the end of the string. If the returned team is the same as the team in possesion, we subtract 100 by the yardline, otherwise we return the yardline, giving us distance from the end zone.

``` r
           Down = str_extract(Situation, "^[1-4]"),
           Distance = str_extract(Situation, "(?<=& )[0-9]+"),
           Yardline_100 = case_when(str_detect(Situation, "50$") ~ 50,
                                    str_extract(Situation, "[A-Z]+(?= [0-9]{1,2}$)") == Off ~ 
                                      100 - as.numeric(str_extract(Situation, "[0-9]{1,2}$")),
                                    TRUE ~ as.numeric(str_extract(Situation, "[0-9]{1,2}$"))),
           GoalToGo = if_else(Distance == Yardline_100, 1, 0),
```
The next part of cleaning the data occurs with assigning play_types. Again we use a case_when statement, utilizing str_detect() to search the play description for key words that distinguish the play type.

``` r
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
There will be more columns added in to the cleaning function as Caio and I develop this out more, but I figured this was a helpful resource for people new to string parsing.

## Final function

The xfl_scrapR function brings together both our scraping and cleaning function to output the most up-to-date XFL play-by-play data in a file. Let's look at what's going on here chunk by chunk:

First, we read in the existing play-by-play file in the github repository:

``` r
xfl_scrapR <- function(){
  pbp <- read_csv(url("https://raw.githubusercontent.com/keegan-abdoo/xflscrapR/master/play_by_play_data/regular_season/reg_pbp_2020.csv"))
```
Next we utilize the lubridate package to automate this function based on the date the user is pulling the data. The wday function returns the day of the week as an integer (starting at Sunday == 1) and the Sys.time() function returns the current date and time. We can find the most recent game played by some conditional date checking here.

We also find the most recent game in the existing data set by simply taking the max of the Game column.

``` r
  most_recent_game <- if_else(wday(as_date(Sys.time())) == 7, 2, 4) + 
    4*floor((as.numeric(as_date(Sys.time())) - as.numeric(ymd(20200208)))/7)
  
  last_game <- max(pbp$Game)

```
If the most recent game played is not updated in the github repository, we open the remote driver. With the remote driver opened, we can use the retrieve game function within a map_dfr function to scrape any games that have occured and but haven't been updated yet.  Once we have the data frame of newly-scraped games, we use my new favorite operator %<>% (shoutout to [@ZachFeldman3](https://twitter.com/ZachFeldman3) for showing me this) to rewrite the data frame with our data cleaning function. We then bind the new data frame to the existing one and have a fully updated play-by-play dataset. We exit the browser and the function returns the new data frame.

``` r
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
```

Finally, you just import the most-updated dataset with the following command.  Because we nested all the data-scraping and wrangling in 3 functions, we have reduced this to one line of code a user needs to run!
``` r
pbp <- xfl_scrapR()
```
