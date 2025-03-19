##################################################
# THIS HAS TO BE DONE BEFORE THE FIRST FOUR GAMES
# OTHERWISE THE NCAA BRACKET WILL CHANGE
##################################################

source("preprocessing/women/functions/data-cleaning-functions.R")
source("simulation/women/functions/simulation-functions.R")

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
  mutate(across(3:ncol(full_dat), ~{
    # Compute mean and standard deviation, ignoring NAs.
    m <- mean(., na.rm = TRUE)
    s <- sd(., na.rm = TRUE)
    x <- .
    missing <- is.na(x) # Identify NA positions.
    # Replace NAs with random draws from a normal distribution 
    # with mean m and sd s.
    x[missing] <- rnorm(sum(missing), mean = m, sd = s)
    x
  })) |> 
  select(-year) |> 
  mutate(region = case_when(row_number() <= 16 ~ "SPOKANE1",
                            row_number() <= 32 ~ "BIRMINGHAM1",
                            row_number() <= 48 ~ "SPOKANE2",
                            row_number() <= 64 ~ "BIRMINGHAM2"))
library(dplyr)

full <- full |>
  group_by(region, team) |>
  mutate(seed = if_else(region == "BIRMINGHAM1" & 
                          team == "First Four" & 
                          row_number() == 1, 11, seed)) |>
  ungroup() |> group_by(region, team) |>
  mutate(seed = if_else(region == "BIRMINGHAM2" & 
                          team == "First Four" & 
                          row_number() == 2, 11, seed)) |>
  ungroup()

rm(full_dat)

# get all pairwise comparisons
dat_for_sim <- pairwise_differences(df = full)

# write data!
readr::write_csv(dat_for_sim, "simulation/women/input-data.csv")

