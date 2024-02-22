# read in 2013-2023. this is 2012-2013 to 2022-2023 season ----
suppressMessages(library(tidyverse))
cbb <- read_csv("cbb.csv", na = c("", "NA", "N/A"))
cbb <- na.omit(cbb)

# results data. from 2002-2003 to 2011-2022 ----
big_dance <- read_csv("Big_Dance_CSV.csv")
# get years we want
dance <- big_dance |> filter(Year >= 2008 & Year <= 2012) |> 
  select(-"Region Number") |> # rename
  rename("seed_1" = "Seed...5", "score_1" = "Score...6",
         "team_1" = "Team...7", "team_2" = "Team...8",
         "score_2" = "Score...9", "seed_2" = "Seed...10",
         "region_name" = `Region Name`) |> 
  # change region_name to round in the tournament
  mutate("region_name" = case_when(Round == 6 ~ "Championship",
                             Round == 5 ~ "F4", 
                             Round == 4 ~ "E8",
                             Round == 3 ~ "R16", 
                             Round == 2 ~ "R32",
                             Round == 1 ~ "R64", TRUE ~ region_name),
         "winner" = if_else(score_1 > score_2, team_1, team_2)) |> 
  # change region name to be more descriptive
  rename("round_title" = "region_name")

# gather amounts of wins ----
results <- dance |> group_by(Year, winner) |> 
  summarise(wins = n()) |> ungroup()

# pivot, then join, then aggregate
dance_long <- dance %>%
  pivot_longer(
    cols = starts_with(c("seed", "score", "team")),
    names_to = c(".value", "team_number"),
    names_sep = "_",
    values_drop_na = TRUE) %>%
  arrange(Year, Round, team_number) |> 
  select(Year, seed, team)

full_data <- left_join(dance_long, results, by = c("Year", "team" = "winner"))

# input zeroes and aggregate
full <- full_data |> 
  mutate(wins = if_else(is.na(wins), 0, wins),
         postseason = case_when(
           wins == 0 ~ "R64", wins == 1 ~ "R32", wins == 2 ~ "R16",
           wins == 3 ~ "E8", wins == 4 ~ "F4", wins == 5 ~ "2ND",
           wins == 6 ~ "Champions", TRUE ~ NA)) |> 
  select(-wins) |> distinct()
rm(big_dance, dance, dance_long, full_data, results)

# scrap kenom stats ----