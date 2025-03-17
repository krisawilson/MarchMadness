# load stuff
library(tidyverse)
library(ranger)
dat <- read_csv("simulation/input-data-2.csv")
load("modeling/models.RData")

simulate_game <- function(teamA, teamB, data, model = "ensemble") {
  ##### 
  # Given two team names, look up their matchup in the data 
  # (in either order), adjust the difference features if needed, 
  # get the predicted win probability from the ensemble, and then 
  # simulate a game
  #####
  
  # grab row with matchup
  matchup_row <- data |>
    filter((team1 == teamA & team2 == teamB) |
             (team1 == teamB & team2 == teamA))
  
  print(paste("Simulating game:", teamA, "vs", teamB))
  print(matchup_row)
  
  if(nrow(matchup_row) == 0) {
    stop("No matchup found for: ", teamA, " vs ", teamB)
  }
  
  matchup_row <- matchup_row[1, ] # use first matching row
  
  # list of covariates
  diff_cols <- c("PPG_diff", "PPG_Allowed_diff", "Pace_diff", 
                 "MOV_diff", "SOS_diff", "OSRS_diff", "DSRS_diff", 
                 "Adj_ORtg_diff", "Adj_DRtg_diff")
  
  # if teamA is listed as team1, use the features as-is.
  # if teamA is actually team2, invert the sign of all differences.
  if(matchup_row$team1 == teamA) {
    matchup_features <- matchup_row[ , diff_cols, drop = FALSE]
  } else {
    matchup_features <- -matchup_row[ , diff_cols, drop = FALSE]
  }
  
  # data frame verification
  matchup_features <- as.data.frame(matchup_features)
  
  # model!
  if (model == "ensemble") {
    prob_rf <- predict(object = rf_mod, 
                       data = matchup_features, 
                       type = "response")$predictions[,"yes"]
    prob_logit <- predict(object = logit_model, 
                          newdata = matchup_features, 
                          type = "response")
    # average the probabilities (may weight in future)
    prob_teamA <- (prob_rf + prob_logit) / 2 
  } else if (model == "rf") {
    prob_teamA <- predict(object = rf_mod, 
                          data = matchup_features, 
                          type = "response")$predictions[,"yes"] 
  } else if (model == "logit_model") {
    prob_teamA <- predict(object = logit_model, 
                          newdata = matchup_features, 
                          type = "response")
  } 
  else { 
    stop("Model ", model, "not supported") 
    }
  
  # simulate a game outcome
  if(runif(1) < prob_teamA) {
    return(teamA)
  } else {
    return(teamB)
  }
}

simulate_region <- function(region1, region2, data, model) {
  #####
  # This function simulates a mini-tournament within a region. It takes 
  # in two region identifiers (which must be equal) along with the matchup 
  # data and the model (ensemble). It first checks that region1 equals 
  # region2. Then it filters the data to that region and reconstructs the 
  # initial bracket based on seed.
  #####
  
  # Check that the two region inputs are identical.
  if(region1 != region2) {
    stop("Both region arguments should be equal.")
  }
  
  # filter the data for matchups within this region.
  reg_data <- data |> filter(region1 == region1, region2 == region1)
  
  # reconstruct the team list for this region from the matchup data.
  # we'll extract all teams that appear as team1 and team2 (in region1).
  teams1 <- reg_data |> select(team = team1, seed = seed1)
  teams2 <- reg_data |> select(team = team2, seed = seed2)
  teams_reg <- bind_rows(teams1, teams2) |> distinct()
  
  # order teams by seed. 
  teams_reg <- teams_reg |> arrange(seed)
  
  # Define the bracket order for 16 teams. To enforce the standard seeding 
  # order, reorder the teams by matching their seed to:
  desired_order <- c(1,16,8,9,5,12,4,13,6,11,3,14,7,10,2,15)
  teams_reg <- teams_reg |> 
    mutate(ordering = match(seed, desired_order)) |>
    arrange(ordering)
  
  # create the initial bracket by pairing the teams sequentially.
  current_bracket <- teams_reg$team
  
  # simulate rounds until one team remains.
  while(length(current_bracket) > 1) {
    winners <- c()
    for(i in seq(1, length(current_bracket), by = 2)) {
      teamA <- current_bracket[i]
      teamB <- current_bracket[i+1]
      winner <- simulate_game(teamA, teamB, data, model)
      winners <- c(winners, winner)
    }
    current_bracket <- winners
  }
  return(current_bracket[1])
}

simulate_final_four <- function(regional_champs, data, model) {
  #####
  # Given a vector of 4 regional champions, simulate the semifinal and   
  # championship
  #####
  if(length(regional_champs) != 4)
    stop("There must be exactly 4 region champions.")
  
  # semifinals: pair 1 vs 2 and 3 vs 4; this might need to be changed
  semifinal1 <- simulate_game(regional_champs[1], regional_champs[2], 
                              data, model)
  semifinal2 <- simulate_game(regional_champs[3], regional_champs[4], 
                              data, model)
  
  # Final: the winners of the semifinals.
  champion <- simulate_game(semifinal1, semifinal2, data, 
                            model)
  return(champion)
}

simulate_tournament <- function(data, model, n_sim = 10000) {
  #####
  # This function simulates the entire tournament by running the regional 
  # simulations and then the final four.
  #####
  
  # make sure there's four regions
  regions <- unique(dat$region1)
  if(length(regions) != 4)
    stop("Expected exactly 4 regions for the tournament simulation.")
  
  champions <- character(n_sim) # initialize champ
  
  for(sim in 1:n_sim) {
    regional_champs <- sapply(regions, function(reg) {
      simulate_region(reg, reg, data, model)
    })
    # Simulate the Final Four.
    champions[sim] <- simulate_final_four(regional_champs, 
                                          data, model)
  }
  return(champions)
}

tourney_results <- simulate_tournament(data = dat, 
                                       model = "rf", n_sim = 10000)
champ_freq <- table(tourney_results)
champ_prob <- prop.table(champ_freq)
print(champ_prob)
