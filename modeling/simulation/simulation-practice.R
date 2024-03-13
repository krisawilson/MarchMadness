# packages----
library(tidyverse)
library(ordinal)
# load model
probit <- readRDS(file = "C:/MarchMadness/modeling/simulation/probit_model.rds")
# load data
df <- read_csv("data/clean-data.csv")
df <- df |> # add ordinal representation
  mutate(finish = case_when(
    postseason == "R68" ~ 0, postseason == "R64" ~ 1,
    postseason == "R32" ~ 2, postseason == "S16" ~ 3,
    postseason == "E8" ~ 4, postseason == "F4" ~ 5,
    postseason == "2ND" ~ 6, postseason == "Champions" ~ 7,
    TRUE ~ NA),
    postseason = as.factor(postseason), # turn to factor
    # order to create ordered factor for polr
    postseason = reorder(postseason, finish, order = TRUE))

# predict 2016, for example----
data2016 <- df |> filter(year == 2016) 
std_2016 <- data2016 |> select(3:19) |> scale() |> data.frame()
std_2016 <- cbind(std_2016, data2016$postseason)
std_2016 <- std_2016 |> rename("postseason" = "data2016$postseason")

# predictions
# remove last column to get probability for each class.
## type = "prob" for probability. stated explicitly even though this
## is the default
preds <- predict(probit, std_2016[, -ncol(std_2016)], type = "cum.prob")
preds <- as.data.frame(preds)
colnames(preds) <- sub("^fit\\.", "", colnames(preds))
data_2016 <- cbind(data2016, preds) |> 
  select(team, conf, seed, postseason, 24:31)

# region assignments ----
east <- c("Florida Gulf Coast", "Fairleigh Dickinson", 
          "Michigan", "Tulsa", "North Carolina", "Xavier",
          "Stephen F. Austin", "West Virginia",
          "Kentucky", "Stony Brook", "Indiana", "Chattanooga",
          "Notre Dame", "Wisconsin", "Pittsburgh", "Providence",
          "USC", "Weber St.")
west <- c("Holy Cross", "Southern", "Oregon", "Oklahoma",
          "Cal St. Bakersfield", "Texas A&M", "Green Bay",
          "Duke", "UNC Wilmington", "Yale", "Baylor",
          "Northern Iowa", "Texas", "VCU", "Oregon St.",
          "Saint Joseph's", "Cincinnati")
south <- c("Wichita St.", "Vanderbilt", "Kansas", "Austin Peay",
           "Villanova", "UNC Asheville", "Miami FL", "Buffalo",
           "Hawaii", "California", "Maryland", 
           "South Dakota St.", "Arizona", "Iowa", "Temple",
           "Connecticut", "Colorado")
midwest <- c("Virginia", "Hampton", "Middle Tennessee",
             "Michigan St.", "Utah", "Fresno St.", "Iowa St.",
             "Iona", "Arkansas Little Rock", "Purdue", "Gonzaga",
             "Seton Hall", "Syracuse", "Dayton", "Butler",
             "Texas Tech")

# assign to regions----
full_2016 <- data_2016 |> 
  mutate(region = case_when(
    team %in% east ~ "east",
    team %in% midwest ~ "midwest",
    team %in% south ~ "south",
    team %in% west ~ "west",
    TRUE ~ NA))

east <- full_2016 |> filter(region == "east")
west <- full_2016 |> filter(region == "west")
south <- full_2016 |> filter(region == "south")
midwest <- full_2016 |> filter(region == "midwest")

# begin sim ----
sim_quads <- function(region, seeds = c(1:16), 
                      round = c("R68", "R64", "R32", "S16", "E8", "F4", "2ND"), 
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
      # Generate a random number using a uniform distribution
# this is the key--adjust this candidate distribution
      rand <- mean(rbeta(30,5,4))
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
  final_result <- results |> mutate(win_pct = wins / (wins + losses))
  return(final_result)
}

# e.g., south regional first round, first quad:
sim_quads(region = south, seeds = c(1,8,9,16), round = "R64", iterations = 100)
# south regional first round, second quad:
sim_quads(south, seeds = c(4,5,12,13), "R64", 100)
# southern regional second round, first quad
