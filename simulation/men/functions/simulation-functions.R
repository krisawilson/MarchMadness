## REGION NAMES IN simulate_tournament() WILL DEPEND ON YEAR

library(ranger)
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

simulate_game <- function(team1, team2, model, region_data) {
  # check packages
  packages <- c("tidyverse", "ranger") # grab packages
  for (pkg in packages) {
    if (!pkg %in% .packages()) {
      library(pkg, character.only = TRUE)
    }
  }
  # grab matchup
  matchup <- get_matchup(team1, team2, region_data)
  
  # predict!
  if (model == "ensemble") {
    prob_rf <- predict(object = rf_mod, 
                       data = matchup, 
                       type = "response")$predictions[,"yes"]
    prob_logit <- predict(object = logit_model, 
                          newdata = matchup, 
                          type = "response")
    prob_team1 <- (prob_rf + prob_logit) / 2 
  } else if (model == "rf") {
    prob_team1 <- predict(object = rf_mod, 
                          data = matchup, 
                          type = "response")$predictions[,"yes"]
  } else if (model == "logit_model") {
    prob_team1 <- predict(object = logit_model, 
                          newdata = matchup, 
                          type = "response")
  } else { 
    stop("Model ", model, " not supported") 
  }
  
  # simulate matchup
  if(runif(1) < prob_team1) {
    return(team1)
  } else {
    return(team2)
  }
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
  sp1 <- data_reg |> filter(region == "SPOKANE1")
  sp1_winner <- simulate_region(region_data = sp1, model = model)
  
  sp2 <- data_reg |> filter(region == "SPOKANE2")
  sp2_winner <- simulate_region(region_data = sp2, model = model)
  
  b1 <- data_reg |> filter(region == "BIRMINGHAM1")
  b1_winner <- simulate_region(region_data = b1, model = model)
  
  b2 <- data_reg |> filter(region == "BIRMINGHAM2")
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