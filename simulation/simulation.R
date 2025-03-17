source("preprocessing/functions/data-cleaning-functions.R")

# get ratings/stats data:
rat25 <- "https://www.sports-reference.com/cbb/seasons/women/2025-ratings.html"
ratings25 <- scrape_adv_stats_wbb(url = rat25, year = 2025)
# get pace:
u25 <- "https://www.sports-reference.com/cbb/seasons/women/2024-advanced-school-stats.html"
p25 <- get_pace_wbb(url = u25, year = 2025)
# join!
team_stats <- inner_join(ratings25, p25, by = c("School", "year"))
# small cleanup
team_stats <- team_stats |> 
  # move pace around
  relocate(Pace, .before = MOV) |> 
  select(-c(Rk, "AP Rank", Conf, W, L)) |> 
  # consistent team names
  mutate(School = team_names_wbb(School))

# next up: write a function that takes in takes in a data frame and for any two rows, computes the difference between them ... that's the logic we need