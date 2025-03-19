# load packagess and data!
library(tidyverse)

# need the -1 bc created data with write.csv
full_data <- read_csv("data/men/full-data.csv") |> select(-1)

# let's get cooking ----
# first, grab relevant features
trim_dat <- full_data |> 
  # remove wins, losses, and conference
  select(-c(W_team1, W_team2, L_team1, 
            L_team2, Conf_team1, Conf_team2)) |> 
  # add binary outcome for win
  mutate(team1_win = if_else(winner == team1, 1, 0),
         # compute differences for each game. can't do things like
         # margin of victory or seed difference since won't have
         # that when I go to do actual predictions on unplayed
         # tournaments. that's data leakage!
         PPG_diff = PPG_team1 - PPG_team2,
         PPG_Allowed_diff = PPG_Allowed_team1 - PPG_Allowed_team2,
         Pace_diff = Pace_team1 - Pace_team2,
         MOV_diff = MOV_team1 - MOV_team2,
         SOS_diff = SOS_team1 - SOS_team2,
         OSRS_diff = OSRS_team1 - OSRS_team2,
         DSRS_diff = DSRS_team1 - DSRS_team2,
         Adj_ORtg_diff = Adj_ORtg_team1 - Adj_ORtg_team2,
         Adj_DRtg_diff = Adj_DRtg_team1 - Adj_DRtg_team2) |> 
  # keep differenced columns now
  select(c(1:9, team1_win, contains("diff")))

rm(full_data)

# done here!
write_csv(trim_dat, "data/men/clean-data.csv")
