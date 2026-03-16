## REGION NAMES IN simulate_tournament() WILL DEPEND ON YEAR

library(tidyverse)
library(xgboost)
library(caret)
library(mgcv)
load("modeling/men/models.RData")
build_bracket <- function(region_data) {
  # build the bracket from matchup-level data (one row per team)
  if (!"tidyverse" %in% .packages()) {
    library(tidyverse)
  }
  teams1 <- region_data |> 
    select(team = team1, seed = seed1) |> distinct()
  teams2 <- region_data |> 
    select(team = team2, seed = seed2) |> distinct()
  
  bracket <- bind_rows(teams1, teams2) |> distinct()
  return(bracket)
}

get_matchup <- function(team_a, team_b, region_data) {
  # retrieve matchup features for two teams.
  # (flips differences if needed)
  if (!"tidyverse" %in% .packages()) {
    library(tidyverse)
  }
  result <- region_data |> filter(team1 == team_a, team2 == team_b)
  
  if(nrow(result) == 0) {
    result <- region_data |> 
      filter(team1 == team_b, team2 == team_a)
    if(nrow(result) == 0) {
      #print(team_a, team_b)
      stop("No matchup found for these teams.")
    }
    result <- result |> 
      mutate(across(c(contains("_diff")), ~ - .))
  }
  # since once region will have two "First Four" teams,
  # take the first result
  if(nrow(result) > 1){
    result <- result |> slice(1)
  }
  return(result)
}

simulate_game <- function(team_a, team_b, model_choice, 
                          region_data, models_list) {
  
  # 1. Get the matchup features
  matchup <- get_matchup(team_a, team_b, region_data)
  
  x_features <- select(matchup, -c(team1, seed1, team2, 
                                   seed2, region))
  
  # 2. Helper function to extract win probability based on model type
  get_prob <- function(mod_name) {
    if (mod_name == "xgb_model") {
      # Native XGBoost requires DMatrix
      dtest <- xgb.DMatrix(data = as.matrix(x_features))
      return(predict(models_list$xgb_model, dtest))
    } else {
      # Caret models use type = "prob"
      return(predict(models_list[[mod_name]], 
                     newdata = x_features, type = "prob")[,"yes"])
    }
  }
  
  # 3. Calculate Win Probability for Team A
  if (model_choice == "ensemble_avg") {
    # simple average of  best models 
    p_gam <- get_prob("gam_model")
    p_xgb <- get_prob("xgb_model")
    p_en  <- get_prob("en_model")
    prob_team_a <- mean(c(p_gam, p_xgb, p_en))
    
  } else if (model_choice == "ensemble_weighted") {
    # Weighted Average (giving more weight to models with higher AUC)
    # Example weights: GAM (40%), XGBoost (40%), Elastic Net (20%)
    p_gam <- get_prob("gam_model")
    p_xgb <- get_prob("xgb_model")
    p_en  <- get_prob("en_model")
    prob_team_a <- (p_gam * 0.40) + (p_xgb * 0.40) + (p_en * 0.20)
    
  } else {
    # Single Model Selection
    prob_team_a <- get_prob(model_choice)
  }
  
  # 4. The Monte Carlo Coin Flip
  # Generate a random number between 0 and 1. If it's less than Team A's prob, Team A wins.
  u <- runif(1)
  if (u < prob_team_a) {
    return(team_a)
  } else {
    return(team_b)
  }
}

# simulate first four games, only need if bracket is 
# created before First Four are played
simulate_first_four <- function(ff_matchups, model_choice, 
                                region_data, models_list) {
  # Initialize an empty dataframe to hold the 4 advancing teams
  advancing_teams <- tibble(team = character(), 
                            seed = numeric(), 
                            region = character())
  
  for (i in 1:nrow(ff_matchups)) {
    t_a <- ff_matchups$team_a[i]
    t_b <- ff_matchups$team_b[i]
    
    # Run the game simulation
    winner <- simulate_game(t_a, t_b, model_choice, 
                            region_data, models_list)
    
    # Store the winner with the seed and region they just won
    advancing_teams <- advancing_teams |> bind_rows(
      tibble(team = winner, seed = ff_matchups$seed[i], 
             region = ff_matchups$region[i])
    )
  }
  return(advancing_teams)
}

# after First Four have been played
insert_ff_winners <- function(base_60, ff_winners) {
  # Combine the 60 locked teams with the 4 play-in winners
  final_64 <- bind_rows(base_60, ff_winners)
  return(final_64)
}

simulate_block <- function(bracket, model, region_data, verbose = FALSE) {
  packages <- c("tidyverse", "ranger") # grab packages
  for (pkg in packages) {
    if (!pkg %in% .packages()) {
      library(pkg, character.only = TRUE)
    }
  }
  seeds <- bracket$seed # extract seeds, assign teams, then model
  if (all(seeds %in% c(1,16,8,9))) {
    game1_winner <- simulate_game(
      bracket$team[bracket$seed == 1],
      bracket$team[bracket$seed == 16],
      model, region_data
    )
    game2_winner <- simulate_game(
      bracket$team[bracket$seed == 8],
      bracket$team[bracket$seed == 9],
      model, region_data
    )
  } else if (all(seeds %in% c(5,12,4,13))) {
    game1_winner <- simulate_game(
      bracket$team[bracket$seed == 5],
      bracket$team[bracket$seed == 12],
      model, region_data
    )
    game2_winner <- simulate_game(
      bracket$team[bracket$seed == 4],
      bracket$team[bracket$seed == 13],
      model, region_data
    )
  } else if (all(seeds %in% c(6,11,3,14))) {
    game1_winner <- simulate_game(
      bracket$team[bracket$seed == 6],
      bracket$team[bracket$seed == 11],
      model, region_data
    )
    game2_winner <- simulate_game(
      bracket$team[bracket$seed == 3],
      bracket$team[bracket$seed == 14],
      model, region_data
    )
  } else if (all(seeds %in% c(7,10,2,15))) {
    game1_winner <- simulate_game(
      bracket$team[bracket$seed == 7],
      bracket$team[bracket$seed == 10],
      model, region_data
    )
    game2_winner <- simulate_game(
      bracket$team[bracket$seed == 2],
      bracket$team[bracket$seed == 15],
      model, region_data
    )
  } else {
    #print(bracket)
    stop("Seeds do not match a known block pattern.")
  }
  
  # block winners
  block_winner <- simulate_game(game1_winner, game2_winner, 
                                model, region_data)
  # (optional) output winner
  if(verbose){
    cat("Block winner:", block_winner, "\n")
  }
  return(block_winner)
}

simulate_region <- function(region_data, model, verbose = FALSE) {
  packages <- c("tidyverse", "ranger") # grab packages
  for (pkg in packages) {
    if (!pkg %in% .packages()) {
      library(pkg, character.only = TRUE)
    }
  }
  # build the bracket from the matchup data
  bracket <- build_bracket(region_data)
  
  # create blocks based on seed filtering
  block1 <- bracket |> filter(seed %in% c(1,16,8,9))
  block2 <- bracket |> filter(seed %in% c(5,12,4,13))
  block3 <- bracket |> filter(seed %in% c(6,11,3,14))
  block4 <- bracket |> filter(seed %in% c(7,10,2,15))
  
  # get winners of blocks
  winner_block1 <- simulate_block(block1, model, region_data)
  winner_block2 <- simulate_block(block2, model, region_data)
  winner_block3 <- simulate_block(block3, model, region_data)
  winner_block4 <- simulate_block(block4, model, region_data)
  
  # have winners face off
  semi1_winner <- simulate_game(winner_block1, winner_block2, 
                                model, region_data)
  semi2_winner <- simulate_game(winner_block3, winner_block4, 
                                model, region_data)
  
  # region winner aka elite 8
  region_winner <- simulate_game(semi1_winner, semi2_winner, 
                                 model, region_data)
  
  # (optional) output winner
  if(verbose){
    cat("Region winner:", region_winner, "\n")
  }
  return(region_winner)
}

simulate_final_four <- function(rc1, rc2, rc3, rc4, model, data) {
  packages <- c("tidyverse", "ranger") # grab packages
  for (pkg in packages) {
    if (!pkg %in% .packages()) {
      library(pkg, character.only = TRUE)
    }
  }
  # semi 1: spokane winners
  semi1_winner <- simulate_game(rc1, rc2, model, data)
  
  # semi 2: birmingham winners
  semi2_winner <- simulate_game(rc3, rc4, model, data)
  
  # champ!
  champion <- simulate_game(semi1_winner, semi2_winner, model, data)
  
  return(champion)
}

simulate_tournament <- function(data, model) {
  packages <- c("dplyr", "rvest") # grab packages
  for (pkg in packages) {
    if (!pkg %in% .packages()) {
      library(pkg, character.only = TRUE)
    }
  }
  sp1 <- data_reg |> filter(region == "south")
  sp1_winner <- simulate_region(region_data = sp1, model = model)
  
  sp2 <- data_reg |> filter(region == "east")
  sp2_winner <- simulate_region(region_data = sp2, model = model)
  
  b1 <- data_reg |> filter(region == "west")
  b1_winner <- simulate_region(region_data = b1, model = model)
  
  b2 <- data_reg |> filter(region == "midwest")
  b2_winner <- simulate_region(region_data = b2, model = model)
  
  champion <- simulate_final_four(sp1_winner, sp2_winner, 
                                  b1_winner, b2_winner,
                                  model, data)
  return(
    list(champion = champion, 
         sp1 = sp1_winner, sp2 = sp2_winner,
         b1 = b1_winner, b2 = b2_winner)
  )
}