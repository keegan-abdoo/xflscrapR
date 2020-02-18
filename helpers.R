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
