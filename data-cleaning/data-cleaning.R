# packages
suppressMessages(library(tidyverse))
library(rvest)
library(polite)

# read in 2013-2023 ----
# this is 2012-2013 to 2022-2023 seasons
cbb <- read_csv("data/cbb.csv", na = c("", "NA", "N/A"))
# clean it up ----
clean_cbb <- cbb |> 
  na.omit() |> janitor::clean_names() |> 
  rename("adj_oe" = "adjoe", "adj_de" = "adjde",
         "tor_d" = "tord", "ftr_d" = "ftrd") |> 
  select(-c(g, w))

# scrape and clean 2008-2012 ----
# this is 2007-2008 to 2011-2012 seasons
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
# list urls ----
url08 <- "https://barttorvik.com/trank.php?year=2008&sort=&top=0&conlimit=All&venue=All&type=All#"
url09 <- "https://barttorvik.com/trank.php?year=2009&sort=&top=0&conlimit=All&venue=All&type=All#"
url10 <- "https://barttorvik.com/trank.php?year=2010&sort=&top=0&conlimit=All&venue=All&type=All#"
url11 <- "https://barttorvik.com/trank.php?year=2011&sort=&top=0&conlimit=All&venue=All&type=All#"
url12 <- "https://barttorvik.com/trank.php?year=2012&sort=&top=0&conlimit=All&venue=All&type=All#"
# apply that bad boy ----
urls <- c(url08, url09, url10, url11, url12) # vector of urls
years <- c(2008, 2009, 2010, 2011, 2012) # vector of years
# apply function. result is a list of data frames
results <- mapply(scrape_n_clean, urls, years, SIMPLIFY = F)
# put it together
metrics <- rbind(results[[1]], results[[2]], results[[3]],
             results[[4]], results[[5]])
# cleanup
rm(results, url08, url09, url10, url11, url12, urls, years)

# wrap up ----
# join time!
full <- rbind(clean_cbb, metrics)
rm(clean_cbb, metrics)

# write to csv
write_csv(full, "data/clean-data.csv")