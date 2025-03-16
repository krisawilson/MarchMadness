# load packagess and data!
library(tidyverse)

# need the -1 bc created data with write.csv
full_data <- read_csv("data/full-data.csv") |> select(-1)

# let's get cooking ----
# first, grab relevant features
trim_dat <- full_data |> 
  # remove wins, losses, and conference
  select(-c(W_team1, W_team2, L_team1, 
            L_team2, Conf_team1, Conf_team2)) |> 
  # add binary outcome for win
  mutate(team1_win = if_else(winner == team1, 1, 0),
         # turn upset into yes/no
         upset_yes = if_else(upset == "yes", 1, 0),
         # compute differences for each game
         MOV_game = abs(score1 - score2),
         seed_diff = abs(seed1 - seed2),
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
  select(c(1:8, year, team1_win, upset_yes,
           MOV_game, contains("diff")))

# standardize covariates
std_dat <- trim_dat |> 
  mutate(across(c(MOV_game, contains("diff")),
                ~ (. - mean(., na.rm = TRUE)) / sd(., na.rm = TRUE),
                .names = "std_{.col}")) |> 
  select(c(1:11, upset_yes, contains("std")))
rm(full_data)

# done here!
write_csv(trim_dat, "data/clean-data.csv")
write_csv(std_dat, "data/standardized-data.csv")
