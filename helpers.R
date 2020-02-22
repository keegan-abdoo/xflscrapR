# convert minutes string to seconds
mins_to_seconds <- function(mins_str){
  mins <- as.numeric(unlist(strsplit(mins_str,":"))[1])
  seconds <- as.numeric(unlist(strsplit(mins_str,":"))[2])
  return(mins * 60 + seconds)
}

# Helper Function for converting field position to distance from endzone
yl_100 <- Vectorize(function(fp_string, off){
    case_when(str_detect(fp_string, "50$") ~ 50,
              str_extract(fp_string, "[A-Z]+(?= [0-9]{1,2}$)") == off ~ 
                  100 - as.numeric(str_extract(fp_string, "[0-9]{1,2}$")),
              TRUE ~ as.numeric(str_extract(fp_string, "[0-9]{1,2}$")))
})

# get colors logos from xflscrapR github and team info from Anthony Reinhard's github
get_team_info <- function() {
  d1 <- read.csv(url("https://raw.githubusercontent.com/ajreinhard/XFL-public/master/teams.csv")) %>%
    select(Abbr, City, TeamName, Full, HeadCoach, Division, Helm)
  d2 <- read.csv(url("https://raw.githubusercontent.com/keegan-abdoo/xflscrapR/master/xfl_logos_and_colors.csv")) %>%
    select(ClubCode, logo, primary, secondary, tertiary, quarternary)
  
  xfl_colors <- d2 %>% left_join(d1, by = c("ClubCode" = "Abbr")) %>%
    rename(team_code = ClubCode, url = logo)
  
  return(xfl_colors)
}
