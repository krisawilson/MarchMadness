# packages----
suppressMessages(library(tidyverse))
library(ordinal)
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

# predictions ----

# load in model
probit <- readRDS(file = "C:/MarchMadness/modeling/simulation/probit_model.rds")

# prep data: this includes standardization!
men24 <- clean_yr |> select(-team, -conf, -seed) |> scale() |> data.frame()
# bind data
wholedf <- cbind(select(clean_yr, team, conf, seed), men24)
# prediction!
preds <- predict(probit, wholedf, type = "prob")
preds <- as.data.frame(preds) # turn into data frame 
# change column names
colnames(preds) <- sub("^fit\\.", "", colnames(preds))
# bind again
wdf <- cbind(clean_yr, preds)
wdf <- wdf |> select(-c(3:19)) # remove original data
rm(preds, probit, wholedf, clean_yr, men24, url) # clean env

# begin tourney structure w region assignments ----
east <- c("Connecticut", "Iowa St.", "Illinois", "Auburn", "San Diego St.", 
          "BYU", "Washington St.", "Florida Atlantic", "Northwestern", 
          "Drake", "Duquesne", "UAB", "Yale", "Morehead St.", 
          "South Dakota St.", "Stetson")
west <- c("North Carolina", "Arizona", "Baylor", "Alabama", "Saint Mary's", 
          "Clemson", "Dayton", "Mississippi St.", "Michigan St.", "Nevada", 
          "New Mexico", "Grand Canyon", "College of Charleston", "Colgate", 
          "Long Beach St.", "Howard", "Wagner")
south <- c("Houston", "Marquette", "Kentucky", "Duke", "Wisconsin", 
           "Texas Tech", "Florida", "Nebraska", "Texas A&M", "Colorado", 
           "Boise St.", "North Carolina St.", "James Madison", "Vermont", 
           "Oakland", "Western Kentucky", "Longwood")
midwest <- c("Purdue", "Tennessee", "Creighton", "Kansas", "Gonzaga", 
             "South Carolina", "Texas", "Utah St.", "TCU", "Colorado St.", 
             "Virginia", "Oregon", "McNeese St.", "Samford", "Akron", 
             "Saint Peter's", "Grambling St.", "Montana St.")
# assign regions
full_2024 <- wdf |>
  mutate(region = case_when(
    team %in% east ~ "east",
    team %in% midwest ~ "midwest",
    team %in% south ~ "south",
    team %in% west ~ "west",
    TRUE ~ NA))
rm(wdf)
# subset by region!
east <- full_2024 |> filter(region == "east")
west <- full_2024 |> filter(region == "west")
south <- full_2024 |> filter(region == "south")
midwest <- full_2024 |> filter(region == "midwest")

# first four ----
## just by eyeballing, Howard will be eliminated from the west
west <- west |> filter(team != "Howard")
## Boise St will be eliminated from the south
south <- south |> filter(team != "Boise St.")
## Grambling St and Virginia will be eliminated from the midwest
midwest <- midwest |> filter(!(team %in% c("Grambling St.", "Virginia")))

# sim time ----
sim_four <- function(region = c(east, west, south, midwest), 
                    seeds = c(1:16),
                    round = c("R68", "R64", "R32", "S16", "E8"), 
                    iterations = 500) {
  # filter teams based on input seeds
  teams <- region |> filter(seed %in% seeds) |> arrange(seed)
  # Initialize counters for wins and losses for each team
  results <- tibble(team = teams$team, seed = teams$seed, wins = 0, losses = 0)
  # Perform simulations
  for (i in 1:iterations) {
    # Simulate matchups
    matchups <- lapply(1:2, function(i) {
      team1 <- teams[i, ] # first team
      team2 <- teams[5 - i, ] # last team
      # Generate a random number using a beta distribution
      # as this is a decent approximation
      #rand <- rbeta(1,5,1)
      
      # generate a random number using a uniform distribution
      # as this induces more variability
      rand <- runif(1)
      # distance between probability and random draw
      diff1 <- abs(team1[[round]] - rand)
      diff2 <- abs(team2[[round]] - rand)
      # winner and loser. closer to rand --> loser
      if (diff1 > diff2) {
        winner <- team1
        loser <- team2
      } else {
        winner <- team2
        loser <- team1
      }
      return(list(winner = winner$team, loser = loser$team))
    })
    # Update wins and losses. [[ is for the double indexing from the
    # return as a list
    winners <- unlist(lapply(matchups, "[[", "winner"))
    losers <- unlist(lapply(matchups, "[[", "loser"))
    results <- results |>
      mutate(wins = if_else(team %in% winners, wins + 1, wins),
             losses = if_else(team %in% losers, losses + 1, losses))
  }
  # Calculate proportions
  result <- results |> mutate(win_pct = wins / (wins + losses))
  # add on their probabilities
  final_result <- result |> 
    left_join(region, by = join_by(team, seed)) |> 
    select(team, seed, region, wins, losses, win_pct, 
           R64, R32, S16, E8, F4, `2ND`, Champions)
  return(final_result)
}

sim_two <- function(quad, round = c("R64", "R32", "S16", "E8", "F4"),
                    iterations = 500) {
  # Subset the region data to get the top two teams by win percentage
  top <- quad |> arrange(desc(win_pct)) |> head(2)
  # Initialize counters for wins and losses for each team
  results <- tibble(team = top$team, seed = top$seed, wins = 0, losses = 0)
  # Simulate the matchup between the top two teams
  for (i in 1:iterations) {
    team1 <- top[1, ]
    team2 <- top[2, ]
    # Generate a random number using a uniform distribution,
    # as R32 is bimodal
    rand <- runif(1)
    # Distance between probability and random draw
    diff1 <- abs(team1[[round]] - rand)
    diff2 <- abs(team2[[round]] - rand)
    # Winner and loser. Closer to rand --> loser
    if (diff1 > diff2) {
      winner <- team1
      loser <- team2
    } else {
      winner <- team2
      loser <- team1
    }
    # Update wins and losses
    results$wins <- results$wins + if_else(results$team == winner$team, 1, 0)
    results$losses <- results$losses + if_else(results$team == loser$team, 1, 0)
  }
  # Calculate proportion of wins
  final_result <- results |> 
    mutate(win_pct = wins / (wins + losses)) |> 
    left_join(quad, results, by = join_by(team, seed)) |> 
    select(team, seed, region, wins = wins.x, losses = losses.y, 
           win_pct = win_pct.x, R64:F4, `2ND`, Champions)
  return(final_result)
}

# east ----

# round of 64
east1 <- sim_four(region = east, seeds = c(1, 16, 8, 9), round = "R64")
east2 <- sim_four(east, seeds = c(4, 5, 12, 13), round = "R64")
east3 <- sim_four(east, seeds = c(3, 14, 6, 11), round = "R64")
east4 <- sim_four(east, seeds = c(2, 15, 7, 10), round = "R64")

# round of 32
east21 <- sim_two(quad = east1, round = "R32")
east22 <- sim_two(east2, "R32")
east23 <- sim_two(east3, "R32")
east24 <- sim_two(east4, "R32")
