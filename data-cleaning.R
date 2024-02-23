# packages
suppressMessages(library(tidyverse))
library(rvest)
library(polite)
# read in and clean 2013-2023 ----
# this is 2012-2013 to 2022-2023 seasons
cbb <- read_csv("data/cbb.csv", na = c("", "NA", "N/A"))
clean_cbb <- na.omit() # idk what's next yet |> 

#rm(cbb)

# results data. from 2007-2008 to 2011-2012 ----
big_dance <- read_csv("data/Big_Dance_CSV.csv")
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
dance_long <- dance |>
  pivot_longer(
    cols = starts_with(c("seed", "score", "team")),
    names_to = c(".value", "team_number"),
    names_sep = "_",
    values_drop_na = TRUE) |>
  arrange(Year, Round, team_number) |> 
  select(Year, seed, team)

full_data <- left_join(dance_long, results, 
                       by = c("Year", "team" = "winner"))

# input zeroes and aggregate
full <- full_data |> 
  mutate(wins = if_else(is.na(wins), 0, wins),
         postseason = case_when(
           wins == 0 ~ "R64", wins == 1 ~ "R32",
           wins == 2 ~ "S16", wins == 3 ~ "E8", 
           wins == 4 ~ "F4", wins == 5 ~ "2ND",
           wins == 6 ~ "Champions", TRUE ~ NA)) |> 
  select(-wins) |> distinct()
rm(big_dance, dance, dance_long, full_data, results)

# scrape stats ----
scrape_n_clean <- function(url, year) {
  # politely scrape
  nice_url <- bow(url = url) |> scrape()
  # actually get table
  tab_yr <- nice_url |> 
    html_element(css = "#content > table") |> html_table()
  tnames <- tab_yr[1,] # extract first row as names
  tab_yr <- tab_yr[-1,] # remove first row
  colnames(tab_yr) <- tnames  # attach names
  
  # begin cleaning process
  clean_yr <- tab_yr |> 
    filter(Rk != "Rk") |> # remove extra rows
    # create seed column
    mutate(seed = str_extract(Team, "\\d+")) |> 
    # split team into team and postseason finish
    separate_wider_delim(cols = "Team", delim = ",", 
                         names = c("Team", "postseason"),
                         too_few = "align_start") |> 
    # remove whitespace, remove number, 
    # remove comma, then remove whitespace again
    mutate(Team = str_squish(Team),
           Team = str_replace_all(Team, "\\d", ","),
           Team = str_extract(Team, "^[^,]+"),
           Team = str_squish(Team),
           # remove hidden whitespace from postseason
           postseason = str_squish(postseason)) |> 
    # remove teams that didn't make tourney
    na.omit(postseason) |> 
    select(-c(Rk, G, Rec)) |> # keep wanted vars
    janitor::clean_names() |> # clean names
    # change to numeric, add year
    mutate(across(4:23, as.numeric), year = year) |> 
    # standardize response var to match cbb
    mutate(postseason = case_when(
      postseason == "CHAMPS" ~ "Champions",
      postseason == "Finals" ~ "2ND",
      postseason == "Final Four" ~ "F4",
      postseason == "Elite Eight" ~ "E8",
      postseason == "Sweet Sixteen" ~ "S16",
      TRUE ~ postseason)
      )
  return(clean_yr)
}
# list urls and years----
url08 <- "https://barttorvik.com/trank.php?year=2008&sort=&top=0&conlimit=All&venue=All&type=All#"
url09 <- "https://barttorvik.com/trank.php?year=2009&sort=&top=0&conlimit=All&venue=All&type=All#"
url10 <- "https://barttorvik.com/trank.php?year=2010&sort=&top=0&conlimit=All&venue=All&type=All#"
url11 <- "https://barttorvik.com/trank.php?year=2011&sort=&top=0&conlimit=All&venue=All&type=All#"
url12 <- "https://barttorvik.com/trank.php?year=2012&sort=&top=0&conlimit=All&venue=All&type=All#"
year1 <- 2008
year2 <- 2009
year3 <- 2010
year4 <- 2011
year5 <- 2012
# apply that bad boy ----
urls <- c(url08, url09, url10, url11, url12)
years <- c(year1, year2, year3, year4, year5)

results <- mapply(scrape_n_clean, urls, years, SIMPLIFY = F)

metrics <- rbind(results[[1]], results[[2]], results[[3]],
             results[[4]], results[[5]])

rm(results, url08, url09, url10, url11, url12, urls,
   year1, year2, year3, year4, year5, years)

# next step: adjust wonky numerical values in the data cleaning step ----
## basically: if > 100, divide it by 100.

# following step: join metrics w full (team)