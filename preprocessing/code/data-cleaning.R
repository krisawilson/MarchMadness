# grab functions
source("data/data-cleaning-functions.R")

# load packages
require(rvest, quietly = TRUE)
require(dplyr, warn.conflicts = FALSE)
require(purrr, quietly = TRUE)
require(stringr, quietly = TRUE)

# grab bracket data ----
# 2024
t24 <- "https://www.sports-reference.com/cbb/postseason/women/2024-ncaa.html"
b24 <- scrape_bracket(url = t24, year = 2024)
# 2023
t23 <- "https://www.sports-reference.com/cbb/postseason/women/2023-ncaa.html"
b23 <- scrape_bracket(url = t23, year = 2023)
# manually fix the Stanford vs Sacred Heart error
b23[46,"seed2"] <- 16; b23[46,"score1"] <- 92; 
b23[46, "score2"] <- 49; b23[46,"winner"] <- "Stanford";

# 2022
t22 <- "https://www.sports-reference.com/cbb/postseason/women/2022-ncaa.html"
b22 <- scrape_bracket(url = t22, year = 2022)
rm(t22,t23,t24)

# get full data: ----
rat24 <- "https://www.sports-reference.com/cbb/seasons/women/2024-ratings.html"
rat23 <- "https://www.sports-reference.com/cbb/seasons/women/2023-ratings.html"
rat22 <- "https://www.sports-reference.com/cbb/seasons/women/2022-ratings.html"

ratings24 <- clean_data(url = rat24, year = 2024)
ratings23 <- clean_data(url = rat24, year = 2023)
ratings22 <- clean_data(url = rat24, year = 2022)
rm(rat22,rat23,rat24)


# get pace ----
u24 <- "https://www.sports-reference.com/cbb/seasons/women/2024-advanced-school-stats.html"
u23 <- "https://www.sports-reference.com/cbb/seasons/women/2023-advanced-school-stats.html"
u22 <- "https://www.sports-reference.com/cbb/seasons/women/2022-advanced-school-stats.html"

p24 <- get_pace(url = u24, year = 2024)
p23 <- get_pace(url = u23, year = 2023)
p22 <- get_pace(url = u22, year = 2022)
rm(u24,u23,u22)

# combine data ----
brackets <- bind_rows(b24, b23, b22); rm(b24, b23, b22)
paces <- bind_rows(p24, p23, p22); rm(p22, p23, p24)
ratings <- bind_rows(ratings24, ratings23, ratings22)
rm(ratings22, ratings23, ratings24)

# join data!
team_stats <- inner_join(ratings, paces, 
                         by = c("School", "year"))
# small cleanup
team_stats <- team_stats |> 
  # move pace around
  relocate(Pace, .before = MOV) |> select(-Rk) |> 
  # consistent team names
  mutate(School = team_names(School))
rm(paces, ratings)

# final join! team 1 first
full_dat <- brackets |>
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
                           value = TRUE))))
# write CSVs!
#write.csv(brackets, "data/brackets.csv")
#write.csv(team_stats, "data/team-data.csv")
#write.csv(full_dat, "data/brackets-and-stats.csv")
