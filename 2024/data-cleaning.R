#setwd"(C:/MarchMadness/2024/")
# packages----
suppressMessages(library(tidyverse))
library(rvest)
library(polite)
# read in and clean data ----
url <- "https://barttorvik.com/trank.php?year=2024&sort=&top=0&conlimit=All&venue=All&type=All#"
# politely scrape
nice_url <- bow(url = url) |> scrape()
# actually get table
tab_yr <- nice_url |> html_element(css = "#content > table") |> html_table()
tnames <- tab_yr[1,] # extract first row as names
tab_yr <- tab_yr[-1,] # remove first row
colnames(tab_yr) <- tnames  # attach names
rm(tnames, nice_url)

clean_yr <- tab_yr |> 
  filter(Rk != "Rk") |> # remove header rows from scraping
  # create seed column
  mutate(seed = str_extract(Team, "\\d+")) |> 
  # split team into team and postseason finish
  separate_wider_delim(cols = "Team", delim = ",", 
                       names = c("Team", "postseason"),
                       too_few = "align_start") |> 
  # remove whitespace, remove seed, 
  # remove comma, then remove whitespace again
  mutate(Team = str_squish(Team),
         Team = str_replace_all(Team, "\\d", ","),
         Team = str_extract(Team, "^[^,]+"),
         Team = str_squish(Team),
         # remove hidden whitespace from postseason
         postseason = str_squish(postseason)) |> 
  # remove teams that didn't make tourney
  na.omit(postseason) |> 
  select(-c(Rk, G, Rec, postseason)) |> # keep wanted vars
  janitor::clean_names() |> # clean names
  # remove offensive 3 pt rate and defensive 3 pt rate defense
  select(-x3pr, -x3prd) |> 
  # change names to match cbb
  rename("tor_d" = "tord", "ftr_d" = "ftrd",
         "efg_o" = "efg_percent", "efg_d" = "efgd_percent",
         "x2p_o" = "x2p_percent", "x2p_d" = "x2p_percent_d",
         "x3p_o" = "x3p_percent", "x3p_d" = "x3p_percent_d") |> 
  mutate(across(3:20, as.numeric))
rm(tab_yr)
write.csv(clean_yr, "C:/MarchMadness/2024/clean-2024.csv")
