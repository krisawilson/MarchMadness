# read in 2013-2023
suppressMessages(library(tidyverse))
cbb <- read_csv("cbb.csv", na = c("", "NA", "N/A"))
cbb <- na.omit(cbb)
# read in 2003-2012 from hoopR
library(hoopR)

old_data2 <- load_mbb_team_box(seasons = 2003:2013)
playoffs <- old_data |> 
  filter(season_type == 3) |> 
  select(game_id:game_date, team_display_name, team_short_display_name,
         team_home_away:turnovers, opponent_team_display_name,
         opponent_team_short_display_name, opponent_team_score)
rm(old_data)
