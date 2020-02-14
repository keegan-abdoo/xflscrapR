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

The most useful methods in my experience, are those that allow you to interact with elements on the page such as filters and search boxes.  Elements are basically just different HTML objects, and we can search for them specifically using xpaths within a findElement() method (here's a great [tutorial for using xpath in a Selenium](https://www.guru99.com/xpath-selenium.html)). What is returned is an object of class webElement (or list of webElements if there are multiple elements with the same xpath, in which case you use findElement**s**()).

webElement objects, like remoteDriver objects, can be appended by methods. The webElement methods can be used to scrape data (i.e. getElementAttribute()) and also to click and select on filters. For instance, this .

To close a Selenium browser, simply use the close method on the remoteDriver, and then stop the server with a method on the driver.

```r
remDr$close()
driver[["server"]]$stop()
```
