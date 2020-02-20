# xflscrapR - A scraper for XFL play-by-play data

xflscrapR is a R project (and eventual package) by to scrape data from stats.xfl.com and process it into a usable format. Unlike [nflscrapR](https://github.com/maksimhorowitz/nflscrapR), which is able to directly access play-by-play data in JSON format, the XFL's data is only available in web form. To get around this, we use [RSelenium](https://cran.r-project.org/web/packages/RSelenium/index.html), a package to drive a browser client through R. If you're interested in the specifics of the scraping process, we've put together a detailed tutorial on scraping XFL data with Selenium [here](https://github.com/keegan-abdoo/xflscrapR/blob/master/ScrapingAndCleaningTutorial.md).

If you're just interested in downloading and using the data, then you're in the right place. The rest of this readme explains how to get xflscrapR working on your machine, and how to use it to create basic visualizations.

## Getting Started

While the eventual goal is for xflscrapR to be an easily-installed package like nflscrapR, this project is still a work-in-progress and we're not quite there yet. For now, you'll have to follow the instructions below to use xflscrapR.

### Prerequisites

xflscrapR depends on a handful of other R packages, which you'll have to install. As it stands, running the following code will install all the required packages. Note that if you intend to run the web scraping code with Selenium, you'll also need a Java installation and Firefox installed, as detailed in the [scraping tutorial](https://github.com/keegan-abdoo/xflscrapR/blob/master/ScrapingAndCleaningTutorial.md).

```
install.packages("glue")
install.packages("RSelenium")
install.packages("XML")
install.packages("tidyverse")
install.packages("hms")
install.packages("lubridate")
install.packages("magrittr")
install.packages("stringi")
install.packages("gsheet")
```
If you plan on using the provided code to calculate EPA columns, you'll also have to install nflscrapR by following the instructions on their [GitHub repository](https://github.com/maksimhorowitz/nflscrapR).

### Downloading & Running xflscrapR

Once you have all the above dependencies installed, you're reading to start using xflscrapR. 

First, either manually download and extract the repository, or clone it with git. We reccomend the latter, as we're frequently pushing updates and bug fixes.

```
git clone https://github.com/keegan-abdoo/xflscrapR.git
```

Next, you can obtain the xflscrapR data by either opening xflscrapR.R and running it, or by sourcing it in your own R file as follows. Note that the below command assumes xflscrapR.R is in your working directory. If it is not, then you will have to substitute "xflscrapR.R" with the appropriate path to it from your working directory. You'll also have to adjust the ```source("helpers.R")``` line near the beginning of the script. You can find your working directory by running ```getwd``` in R.

```
source("xflscrapR.R")
```

Upon sourcing xflscrapR.R, the script will run, calling the ```xfl_scrapR()``` line and placing the results in the variable ```pbp```. When this runs, xflscrapR determines whether the data hosted at this GitHub repository is up to date. If it is, the code simply cleans and processess it, and returns the prepared dataframe. Otherwise, the Selenium scraper launches and pulls the missing data from the [XFL stats website](stats.xfl.com).

From there, you can check the data loaded properly with the following command.

```
head(pbp)
```
The data columns and format should match the row below. 

| Game| play_id| qtr|time     |posteam |Situation      |desc                                                                                          | DrivePlays| DriveYards|DriveTime | AwayScoreAfterDrive| HomeScoreAfterDrive|home_team |away_team |defteam | Week|    game_id| quarter_seconds_remaining| half_seconds_remaining| game_seconds_remaining|down | ydstogo| yardline_100| goal_to_go|play_type | shotgun| no_huddle| qb_spike| qb_kneel| sack| qb_scramble| pass_attempt| interception| complete_pass| throwaway|pass_length |pass_location |run_direction |run_location |run_gap | touchdown| fumble| yards_gained| first_down| turnover|turnover_type | fumble_lost| fourth_down_decision| extra_point_type| extra_point_conversion| penalty| solo_tackle| assist_tackle| tackle_for_loss|passer_player_name |receiver_player_name |rusher_player_name |interception_player_name |solo_tackle_player_name |assist_tackle_1_player_name |assist_tackle_2_player_name | air_yards| yards_after_catch|
|----:|-------:|---:|:--------|:-------|:--------------|:---------------------------------------------------------------------------------------------|----------:|----------:|:---------|-------------------:|-------------------:|:---------|:---------|:-------|----:|----------:|-------------------------:|----------------------:|----------------------:|:----|-------:|------------:|----------:|:---------|-------:|---------:|--------:|--------:|----:|-----------:|------------:|------------:|-------------:|---------:|:-----------|:-------------|:-------------|:------------|:-------|---------:|------:|------------:|----------:|--------:|:-------------|-----------:|--------------------:|----------------:|----------------------:|-------:|-----------:|-------------:|---------------:|:------------------|:--------------------|:------------------|:------------------------|:-----------------------|:---------------------------|:---------------------------|---------:|-----------------:|
|    1|     125|   1|00:05:04 |SEA     |1st & 10 DC 42 |(Shotgun) B.Silvers pass deep left to D.Byrd out of bounds at DC 14 for 28 yards (D.Lawrence) |         NA|         NA|NA        |                  NA|                  NA|DC        |SEA       |DC      |    1| 2020020800|                       304|                   1204|                   3004|1    |      10|           42|          0|dropback  |       1|         0|        0|       NA|    0|           0|            1|            0|             1|         0|deep        |left          |NA            |NA           |NA      |         0|      0|           28|          1|        0|NA            |           0|                   NA|               NA|                     NA|       0|           1|             0|               0|B.Silvers          |D.Byrd               |NA                 |NA                       |D.Lawrence              |NA                          |NA                          |        26|                 2|

## Usage Examples

COMING SOON - This section will include short guides on ways you can use the data to produce interesting visualizations, as well as a guide on getting EPA data from nflscrapR.

## Bug Reporting & Contributing

This project is still very much a work in progress and has several bugs. If you spot one, please report it in the issues section of this repository. Identifying and isolating bugs is half the legwork of debugging, so we'd really appreciate any bug reports.

If you'd like to contribute to this project, we reccomend creating an issue and detailing what you're hoping to fix or add, along with your code. If it's a worthwhile inclusion and your code matches our architecture well, we'll merge it into the repository.

## Authors

* **Keegan Abdoo** - *Scraper and Data Processing* - [Keegan Abdoo](https://twitter.com/KeeganAbdoo)
* **Caio Brighenti** - *Data Processing* - [Caio Brighenti](https://twitter.com/CaioBrighenti2)

## License

This project has no specific license. It's meant to be used and shared, with the goal of facilitating XFL data analysis. However, if you do use our data, we ask that you credit the project so more people can find it. Just a quick mention of "data from @xflscrapR" or "Data: @xflscrapR" in a figure caption will do.

## Acknowledgments

* [nflscrapR](https://github.com/maksimhorowitz/nflscrapR) for serving as inspiration and a great reference.
* [Anthony Reinhard](https://twitter.com/reinhurdler) for code sharing and cross-checking work.
* [Moose Jester](https://twitter.com/CFB_Moose) for sharing his manually charted air yards dataset.
* [Ben Baldwin](https://twitter.com/benbbaldwin) for finding and reporting countless bugs.
