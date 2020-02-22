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

# get colors and logos courtesy of Anthony Reinhard's github
get_xfl_colors <- function() {
  xfl_colors <- read.csv(url("https://raw.githubusercontent.com/ajreinhard/XFL-public/master/teams.csv")) %>%
    rename(team_code = Abbr, primary = Color1, secondary = Color2) %>%
    mutate(
      #messes with ggplot if this step isn't taken
      primary = as.character(primary), 
      secondary = as.character(secondary), 
      team_code = as.character((team_code))
    ) %>%
    arrange(team_code)
  
  xfl_colors$url <- NA
  xfl_colors$url[1] <- "https://raw.githubusercontent.com/ajreinhard/XFL-public/master/logos/dal.png"
  xfl_colors$url[2] <- "https://raw.githubusercontent.com/ajreinhard/XFL-public/master/logos/dc.png"
  xfl_colors$url[3] <- "https://raw.githubusercontent.com/ajreinhard/XFL-public/master/logos/hou.png"
  xfl_colors$url[4] <- "https://raw.githubusercontent.com/ajreinhard/XFL-public/master/logos/la.png"
  xfl_colors$url[5] <- "https://raw.githubusercontent.com/ajreinhard/XFL-public/master/logos/ny.png"
  xfl_colors$url[6] <- "https://raw.githubusercontent.com/ajreinhard/XFL-public/master/logos/sea.png"
  xfl_colors$url[7] <- "https://raw.githubusercontent.com/ajreinhard/XFL-public/master/logos/stl.png"
  xfl_colors$url[8] <- "https://raw.githubusercontent.com/ajreinhard/XFL-public/master/logos/tb.png"
  
  return(xfl_colors)
}
