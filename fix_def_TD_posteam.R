fix_def_TD_posteam <- function(df = pbp) {
  
  # get a table of game_id and posteam
  df_team_games <-
    df %>% select(posteam, game_id) %>% group_by(game_id) %>% distinct(posteam) %>% ungroup() %>% rename(new_posteam = posteam)
  
  # fix = 1 if the posteam is incorrect on the point after try
  df <- df %>%
    mutate(
      fix = case_when(str_detect(str_to_lower(lag(desc)), "intercepted|fumbles|blocked") &
                             str_detect(str_to_lower(desc), "pt attempt") &
                             posteam == lag(posteam) ~ 1,
                           TRUE ~ 0))
  
  # get the correct posteam
  df_fix <- df %>% 
    filter(fix == 1) %>% 
    left_join(df_team_games) %>% 
    filter(posteam != new_posteam) %>% 
    mutate(posteam = new_posteam) %>% 
    select(-new_posteam)
  
  # bind_rows with data that does not need to be fixed
  df <- df %>% 
    filter(fix == 0) %>% 
    bind_rows(df_fix) %>% 
    select(-fix) %>% 
    arrange(Game, play_id)

  return(df)
}
