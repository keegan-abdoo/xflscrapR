# convert minutes string to seconds
mins_to_seconds <- function(mins_str){
  mins <- as.numeric(unlist(strsplit(mins_str,":"))[1])
  seconds <- as.numeric(unlist(strsplit(mins_str,":"))[2])
  return(mins * 60 + seconds)
}