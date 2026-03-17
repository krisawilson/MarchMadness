##################################################
# THIS HAS TO BE DONE BEFORE THE FIRST FOUR GAMES
# OTHERWISE THE NCAA BRACKET WILL CHANGE
##################################################

library(tidyverse)
source("preprocessing/women/functions/data-cleaning-functions.R")
#source("simulation/women/functions/simulation-functions.R")

# get ratings/stats data:
rat26 <- "https://www.sports-reference.com/cbb/seasons/women/2026-ratings.html"
ratings26 <- scrape_adv_stats_wbb(url = rat26, year = 2026)
# get pace:
u26 <- "https://www.sports-reference.com/cbb/seasons/women/2026-advanced-school-stats.html"
p26 <- scrape_pace_wbb(url = u26, year = 2026)
# join!
team_stats <- inner_join(ratings26, p26, by = c("School", "year"))
# small cleanup
team_stats <- team_stats |> 
  # move pace around
  relocate(Pace, .before = MOV) |> 
  select(-c(Rk, "AP Rank", Conf, W, L)) |> 
  # consistent team names
  mutate(School = team_names_wbb(School))

# grab bracket from NCAA
url1 <- "https://www.ncaa.com/brackets/basketball-women/d1/2026"
bracket26 <- scrape_bracket_ncaaw(url = url1)

# add First Four. At the bottom of the page so they didn't get 
# scraped in appropriately ----
School <- c("Missouri State", "Stephen F. Austin", "Nebraska", "Richmond",
            "Southern", "Samford", "Virginia", "Arizona State")
region <- c("FORT WORTH 3", "FORT WORTH 3", "SACRAMENTO 2", "SACRAMENTO 2",
            "SACRAMENTO 4", "SACRAMENTO 4", "SACRAMENTO 4", "SACRAMENTO 4")
seeds <- c(16, 16, 11, 11, 16, 16, 10, 10)
ff <- data.frame(team = School, region = region, seed = seeds)
bracket26 <- rbind(bracket26, ff)
bracket26 <- bracket26 |> filter_out(team == "First Four")
# fix team names again--thanks NCAA ----
bracket26 <- bracket26 |> mutate(team = team_names_wbb(team))
rm(url1, u26, rat26, ratings26, p26, region, School, seeds, ff)

# 1. Standardize names and filter stats to ONLY the 68 tournament teams
# (We rename "School" to "team" so it matches the bracket and simulation functions)
tourney_stats <- team_stats |> 
  rename(team = School) |> 
  inner_join(bracket26, by = "team")

# 2. Isolate just the teams and their stats (temporarily drop metadata for a clean join)
teams_for_join <- tourney_stats |> 
  select(-region, -seed)

# 3. The Multiverse Cross-Join
# Creates all 68x68 combinations and applies the _team1 and _team2 suffixes
matchups <- teams_for_join |> 
  cross_join(teams_for_join, suffix = c("_team1", "_team2")) |> 
  filter(team_team1 != team_team2) |> 
  rename(team1 = team_team1, team2 = team_team2)

# 4. Calculate the stat differences
matchups_diff <- matchups |> 
  mutate(
    PPG_diff  = PPG_team1 - PPG_team2,
    PPG_Allowed_diff = PPG_Allowed_team1 - PPG_Allowed_team2,
    Pace_diff = Pace_team1 - Pace_team2,
    MOV_diff  = MOV_team1 - MOV_team2,
    SOS_diff  = SOS_team1 - SOS_team2,
    OSRS_diff = OSRS_team1 - OSRS_team2,
    DSRS_diff = DSRS_team1 - DSRS_team2,
    Adj_ORtg_diff = Adj_ORtg_team1 - Adj_ORtg_team2,
    Adj_DRtg_diff = Adj_DRtg_team1 - Adj_DRtg_team2
  )

# 5. Bring the Seed and Region metadata back in for the simulation functions
final_matchup_data <- matchups_diff |> 
  left_join(bracket26 |> select(team, seed1 = seed, region), by = c("team1" = "team")) |> 
  left_join(bracket26 |> select(team, seed2 = seed), by = c("team2" = "team")) |> 
  select(team1, seed1, region, team2, seed2, everything())

# 6. Export the finalized Women's Encyclopedia!
write_csv(final_matchup_data, "simulation/women/input-data.csv")
