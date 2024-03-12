library(tidyverse)
library(rvest)
library(polite)

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
    select(-c(Rk, G, Rec)) |> # keep wanted vars
    janitor::clean_names() |> # clean names
    # change to numeric, add year
    mutate(across(4:23, as.numeric), year = year,
           # standardize response var to match cbb dataset
           postseason = case_when(
             postseason == "CHAMPS" ~ "Champions",
             postseason == "Finals" ~ "2ND",
             postseason == "Final Four" ~ "F4",
             postseason == "Elite Eight" ~ "E8",
             postseason == "Sweet Sixteen" ~ "S16",
             TRUE ~ postseason),
           # fix scraping errors
           across(7:22, ~case_when(
             abs(.) > 10000 ~ . / 1000,
             abs(.) > 1000 ~ . / 100,
             abs(.) > 100 ~ . / 10,
             TRUE ~ .
           )),
           # fix another scraping error
           wab = if_else(abs(wab) > 13.5, wab / 10, wab)) |> 
    ### remove offensive 3 pt rate and defensive 3 pt rate defense
    ### since not in 2013-2023 data, easier to remove it than omit
    ### it in ALL models fit later
    select(-x3pr, -x3prd) |> 
    # change names to match cbb
    rename("tor_d" = "tord", "ftr_d" = "ftrd",
           "efg_o" = "efg_percent", "efg_d" = "efgd_percent",
           "x2p_o" = "x2p_percent", "x2p_d" = "x2p_percent_d",
           "x3p_o" = "x3p_percent", "x3p_d" = "x3p_percent_d")
  return(clean_yr)
}

saveRDS(scrape_n_clean, "data-cleaning/data-cleaning-function.rds")