##################################################
# THIS HAS TO BE DONE BEFORE THE FIRST FOUR GAMES
# OTHERWISE THE NCAA BRACKET WILL CHANGE
##################################################

source("preprocessing/functions/data-cleaning-functions.R")
source("simulation/functions/simulation-functions.R")

# get ratings/stats data:
rat25 <- "https://www.sports-reference.com/cbb/seasons/women/2025-ratings.html"
ratings25 <- scrape_adv_stats_wbb(url = rat25, year = 2025)
# get pace:
u25 <- "https://www.sports-reference.com/cbb/seasons/women/2024-advanced-school-stats.html"
p25 <- scrape_pace_wbb(url = u25, year = 2025)
# join!
team_stats <- inner_join(ratings25, p25, by = c("School", "year"))
# small cleanup
team_stats <- team_stats |> 
  # move pace around
  relocate(Pace, .before = MOV) |> 
  select(-c(Rk, "AP Rank", Conf, W, L)) |> 
  # consistent team names
  mutate(School = team_names_wbb(School))

# grab bracket from NCAA
url1 <- "https://www.ncaa.com/brackets/basketball-women/d1/2025"
bracket25 <- scrape_bracket_ncaaw(url = url1)

# fix team names again--thanks NCAA
bracket25 <- bracket25 |> mutate(team = team_names_wbb(team))

# join all data!
full_dat <- left_join(bracket25, team_stats,
                      by = join_by(team == School))

rm(url1, u25, rat25, ratings25, p25, bracket25, team_stats)

# give the first four teams the minimum of the other stats
full <- full_dat |> 
  mutate(across(3:length(full_dat), 
                ~ifelse(is.na(.), min(., na.rm = TRUE), .))) |> 
  select(-year) |> 
  mutate(region = case_when(row_number() <= 16 ~ "SPOKANE1",
                            row_number() <= 32 ~ "BIRMINGHAM1",
                            row_number() <= 48 ~ "SPOKANE4",
                            row_number() <= 64 ~ "BIRMINGHAM3")) |> 
  distinct(team, region, .keep_all = TRUE)

rm(full_dat)

# get all pairwise comparisons
dat_for_sim <- pairwise_differences(df = full)

# add region column
dat_for_sim_2 <- dat_for_sim |>
  left_join(full |> 
              select(region, team) |> 
              rename(region1 = region, team1 = team), 
            by = "team1",
            relationship = "many-to-many") |>
  left_join(full |> 
              select(region, team) |> 
              rename(region2 = region, team2 = team), 
            by = "team2",
            relationship = "many-to-many") |> 
  relocate(c("region1", "region2"), .before = PPG_diff)

# write data!
readr::write_csv(dat_for_sim_2, "simulation/input-data-2.csv")

