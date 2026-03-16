##################################################
# THIS HAS TO BE DONE BEFORE THE FIRST FOUR GAMES
# OTHERWISE THE NCAA BRACKET WILL CHANGE
##################################################

library(tidyverse)
source("preprocessing/men/functions/data-cleaning-functions.R")
#source("simulation/men/functions/simulation-functions.R")

# get ratings/stats data:
rat26 <- "https://www.sports-reference.com/cbb/seasons/men/2026-ratings.html"
ratings26 <- scrape_adv_stats_mbb(url = rat26, year = 2026)
# get pace:
u26 <- "https://www.sports-reference.com/cbb/seasons/men/2026-advanced-school-stats.html"
p26 <- scrape_pace_mbb(url = u26, year = 2026)
# join!
team_stats <- inner_join(ratings26, p26, by = c("School", "year"))
# small cleanup
team_stats <- team_stats |> 
  # move pace around
  relocate(Pace, .before = MOV) |> 
  select(-c(Rk, "AP Rank", Conf, W, L)) |> 
  # consistent team names
  mutate(School = team_names_mbb(School))

rm(p26, ratings26, rat26, u26)
# hard-code 2026 men's bracket
b26 <- read_csv("data/men/2026_ncaa_bracket_expanded.csv")

# fix team names again--this is on Sports Reference!
b26 <- b26 |> mutate(team1 = team_names_mbb(team1),
                     team2 = team_names_mbb(team2),
                     year = 2026)

# final join! team 1 first
full_dat <- b26 |>
  left_join(team_stats, 
            # schools and years should match
            by = c("team1" = "School", "year" = "year")) |>
  rename_with(~ paste0(., "_team1"), 
              # want stats for team 1 to be identified
              .cols = setdiff(names(team_stats), 
                              c("School", "year")))
# now team 2
full_dat <- full_dat |>
  left_join(team_stats, 
            # again, schools and years should match
            by = c("team2" = "School", "year" = "year")) |>
  rename_with(
    ~ paste0(., "_team2"), 
    # again, want stats for team 2 to be identified
    .cols = setdiff(names(team_stats), 
                    c("School", "year", 
                      grep("_team1$", names(full_dat), 
                           value = TRUE)))) |> 
  select(-year)

rm(b26, team_stats)

# manually input UMBC stats
# Define the exact columns you need to fill
umbc_cols <- c("PPG_team2", "PPG_Allowed_team2", "Pace_team2", "MOV_team2", 
               "SOS_team2", "OSRS_team2", "DSRS_team2", "Adj_ORtg_team2", 
               "Adj_DRtg_team2")

# Inject the numbers directly into the UMBC row
full_dat[full_dat$team2 == "UMBC", umbc_cols] <- list(
  76.2,  # PPG_team2
  65.2,  # PPG_Allowed_team2
  65.9,  # Pace_team2
  4.9,   # MOV_team2
  0,  # SOS_team2
  0,   # OSRS_team2
  0,  # DSRS_team2
  114.7, # Adj_ORtg_team2
  100.9  # Adj_DRtg_team2
)

compute_all_matchup_differences <- function(df) {
  
  # Extract team statistics from both team1 and team2 columns
  team1_df <- df |>
    select(team = team1, seed = seed1, 
           PPG = PPG_team1, PPG_Allowed = PPG_Allowed_team1, Pace = Pace_team1, 
           MOV = MOV_team1, SOS = SOS_team1,OSRS = OSRS_team1, DSRS = DSRS_team1, 
           Adj_ORtg = Adj_ORtg_team1, Adj_DRtg = Adj_DRtg_team1)
  
  team2_df <- df |>
    select(team = team2, seed = seed2, PPG = PPG_team2, 
           PPG_Allowed = PPG_Allowed_team2, Pace = Pace_team2, MOV = MOV_team2,
           SOS = SOS_team2, OSRS = OSRS_team2, DSRS = DSRS_team2, 
           Adj_ORtg = Adj_ORtg_team2, Adj_DRtg = Adj_DRtg_team2)
  
  # Combine and remove duplicates
  teams <- bind_rows(team1_df, team2_df) |>
    distinct(team, .keep_all = TRUE)
  
  # Create all possible pairings (matchups) where team_A != team_B
  # We use a cross join (via inner_join with by = character()) to 
  # generate every combination
  matchups <- teams |>
    # rename all columns except "team"
    rename_with(~ paste0(., "_team1"), -team) |> 
    cross_join(teams |> rename_with(~ paste0(., "_team2"), -team)) |> 
    filter(team.x != team.y)
  
  # Compute differences for each stat (team A minus team B)
  matchups <- matchups |>
    mutate(
      PPG_diff = PPG_team1 - PPG_team2,
      PPG_Allowed_diff = PPG_Allowed_team1 - PPG_Allowed_team2,
      Pace_diff = Pace_team1 - Pace_team2,
      MOV_diff = MOV_team1 - MOV_team2,
      SOS_diff = SOS_team1 - SOS_team2,
      OSRS_diff = OSRS_team1 - OSRS_team2,
      DSRS_diff = DSRS_team1 - DSRS_team2,
      Adj_ORtg_diff = Adj_ORtg_team1 - Adj_ORtg_team2,
      Adj_DRtg_diff = Adj_DRtg_team1 - Adj_DRtg_team2
      ) |> 
    select(team1 = team.x, seed1 = seed_team1, 
           team2 = team.y, seed2 = seed_team2, contains("_diff"))
  
  return(matchups)
}

dat_for_sim <- compute_all_matchup_differences(df = full_dat)

# add region
team_lookup <- full_dat |>
  select(team = team1, seed = seed1, region) |>
  bind_rows(full_dat |> select(team = team2, seed = seed2, region)) |>
  distinct(team, region)

# Add region1 and region2 to dat_for_sim by joining with the lookup table
dat_for_sim_2 <- dat_for_sim |>
  left_join(team_lookup, by = c("team1" = "team"),
            relationship = "many-to-many") |>
  rename(region1 = region) |>
  left_join(team_lookup, by = c("team2" = "team"),
            relationship = "many-to-many") |>
  rename(region2 = region)


# write data!
readr::write_csv(dat_for_sim_2, "simulation/men/input-data.csv")

