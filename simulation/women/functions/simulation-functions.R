library(tidyverse)
library(xgboost)
library(caret)
library(mgcv)

build_bracket <- function(matchup_data) {
  
  teams1 <- matchup_data |> 
    select(team = team1, seed = seed1) |> 
    distinct()
  
  teams2 <- matchup_data |> 
    select(team = team2, seed = seed2) |> 
    distinct()
  
  bracket <- bind_rows(teams1, teams2) |> distinct()
  return(bracket)
}

get_matchup <- function(team_a, team_b, matchup_data) {
  
  # Look up team_a vs team_b in the static encyclopedia
  result <- matchup_data |> filter(team1 == team_a, team2 == team_b)
  
  if(nrow(result) == 0) {
    result <- matchup_data |> filter(team1 == team_b, team2 == team_a)
    if(nrow(result) == 0) {
      stop(paste("No matchup found for", team_a, "and", team_b)) 
    }
    result <- result |> mutate(across(c(contains("_diff")), ~ - .))
  }
  if(nrow(result) > 1) {
    result <- result |> slice(1)
  }
  return(result)
}

simulate_game <- function(team_a, team_b, model_choice, 
                          matchup_data, models_list) {
  
  matchup <- get_matchup(team_a, team_b, matchup_data)
  
  x_features <- select(matchup, 
                       -any_of(c("team1", "seed1", "team2", 
                                 "seed2", "region1", "region2")))
  
  get_prob <- function(mod_name) {
    if (mod_name == "xgb_model") {
      dtest <- xgb.DMatrix(data = as.matrix(x_features))
      return(predict(models_list$xgb_model, dtest))
    } else {
      return(predict(models_list[[mod_name]], 
                     newdata = x_features, type = "prob")[,"yes"])
    }
  }
  
  if (model_choice == "ensemble_avg") {
    # simple average of best models
    p_gam <- get_prob("gam_model")
    p_rf  <- get_prob("rf_model")
    prob_team_a <- mean(c(p_gam, p_rf))
    
    # weighted average of best models
  } else if (model_choice == "ensemble_weighted") {
    p_gam <- get_prob("gam_model")
    p_xgb <- get_prob("xgb_model")
    p_rf  <- get_prob("rf_model")
    prob_team_a <- (p_gam * 0.40) + (p_xgb * 0.20) + (p_rf * 0.40)
    # single model
  } else {
    prob_team_a <- get_prob(model_choice)
  }
  
  # coin flip step
  u <- runif(1)
  if (u < prob_team_a) {
    return(team_a)
  } else {
    return(team_b)
  }
}

simulate_first_four <- function(ff_matchups, model_choice, 
                                matchup_data, models_list) {
  advancing_teams <- tibble(team = character(), 
                            seed = numeric(), region = character())
  for (i in 1:nrow(ff_matchups)) {
    winner <- simulate_game(ff_matchups$team_a[i], ff_matchups$team_b[i], 
                            model_choice, matchup_data, models_list)
    
    advancing_teams <- advancing_teams |> 
      bind_rows(tibble(team = winner, seed = ff_matchups$seed[i], 
                       region = ff_matchups$region[i]))
  }
  return(advancing_teams)
}

insert_ff_winners <- function(base_60, ff_winners) {
  return(bind_rows(base_60, ff_winners))
}

simulate_block <- function(bracket, model_choice, 
                           matchup_data, models_list, 
                           return_all = FALSE) {
  seeds <- bracket$seed 
  if (all(seeds %in% c(1,16,8,9))) {
    game1_w <- simulate_game(bracket$team[bracket$seed == 1],
                             bracket$team[bracket$seed == 16], 
                             model_choice, matchup_data, models_list)
    
    game2_w <- simulate_game(bracket$team[bracket$seed == 8], 
                             bracket$team[bracket$seed == 9], 
                             model_choice, matchup_data, models_list)
    
  } else if (all(seeds %in% c(5,12,4,13))) {
    game1_w <- simulate_game(bracket$team[bracket$seed == 5], 
                             bracket$team[bracket$seed == 12], 
                             model_choice, matchup_data, models_list)
    
    game2_w <- simulate_game(bracket$team[bracket$seed == 4], 
                             bracket$team[bracket$seed == 13], 
                             model_choice, matchup_data, models_list)
    
  } else if (all(seeds %in% c(6,11,3,14))) {
    game1_w <- simulate_game(bracket$team[bracket$seed == 6], 
                             bracket$team[bracket$seed == 11], 
                             model_choice, matchup_data, models_list)
    
    game2_w <- simulate_game(bracket$team[bracket$seed == 3], 
                             bracket$team[bracket$seed == 14], 
                             model_choice, matchup_data, models_list)
    
  } else if (all(seeds %in% c(7,10,2,15))) {
    game1_w <- simulate_game(bracket$team[bracket$seed == 7], 
                             bracket$team[bracket$seed == 10], 
                             model_choice, matchup_data, models_list)
    
    game2_w <- simulate_game(bracket$team[bracket$seed == 2], 
                             bracket$team[bracket$seed == 15], 
                             model_choice, matchup_data, models_list)
  } else stop("Seeds do not match a known block pattern.")
  
  r32_w <- simulate_game(game1_w, game2_w, model_choice, 
                         matchup_data, models_list)
  # return winners if true
  if (return_all) {
    return(list(r64 = c(game1_w, game2_w), r32 = r32_w))
  } else {
    return(r32_w)
  }
}

simulate_region <- function(region_bracket, model_choice, 
                            matchup_data, models_list,
                            return_all = FALSE) {
  block1 <- region_bracket |> filter(seed %in% c(1,16,8,9))
  block2 <- region_bracket |> filter(seed %in% c(5,12,4,13))
  block3 <- region_bracket |> filter(seed %in% c(6,11,3,14))
  block4 <- region_bracket |> filter(seed %in% c(7,10,2,15))
  
  b1_res <- simulate_block(block1, model_choice, matchup_data, 
                           models_list, return_all)
  b2_res <- simulate_block(block2, model_choice, matchup_data, 
                           models_list, return_all)
  b3_res <- simulate_block(block3, model_choice, matchup_data, 
                           models_list, return_all)
  b4_res <- simulate_block(block4, model_choice, matchup_data, 
                           models_list, return_all)
  
  if (return_all) {
    semi1_w <- simulate_game(b1_res$r32, b2_res$r32, 
                             model_choice, matchup_data, models_list)
    semi2_w <- simulate_game(b3_res$r32, b4_res$r32, 
                             model_choice, matchup_data, models_list)
    champ   <- simulate_game(semi1_w, semi2_w, model_choice, 
                             matchup_data, models_list)
    
    return(list(
      r64 = c(b1_res$r64, b2_res$r64, b3_res$r64, b4_res$r64),
      r32 = c(b1_res$r32, b2_res$r32, b3_res$r32, b4_res$r32),
      s16 = c(semi1_w, semi2_w),
      e8  = champ
    ))
  } else {
    semi1_w <- simulate_game(b1_res, b2_res, model_choice, 
                             matchup_data, models_list)
    semi2_w <- simulate_game(b3_res, b4_res, model_choice, 
                             matchup_data, models_list)
    return(simulate_game(semi1_w, semi2_w, model_choice, 
                         matchup_data, models_list))
  }
}

simulate_final_four <- function(rc1, rc2, rc3, rc4, 
                                model_choice, matchup_data, 
                                models_list, return_all = FALSE) {
  semi1_w <- simulate_game(rc1, rc2, model_choice, 
                           matchup_data, models_list)
  semi2_w <- simulate_game(rc3, rc4, model_choice, 
                           matchup_data, models_list)
  champion <- simulate_game(semi1_w, semi2_w, model_choice, 
                            matchup_data, models_list)
  
  if (return_all) {
    return(list(f4 = c(semi1_w, semi2_w), champion = champion))
  } else {
    return(champion)
  }
}

simulate_tournament <- function(bracket_64, matchup_data, 
                                model_choice, models_list, 
                                return_all = FALSE) {
  sp1 <- bracket_64 |> filter(region == "FORT WORTH 1")
  sp2 <- bracket_64 |> filter(region == "SACRAMENTO 2")
  b1  <- bracket_64 |> filter(region == "SACRAMENTO 4")
  b2  <- bracket_64 |> filter(region == "FORT WORTH 3")
  
  sp1_w <- simulate_region(sp1, model_choice, matchup_data, 
                           models_list, return_all)
  sp2_w <- simulate_region(sp2, model_choice, matchup_data, 
                           models_list, return_all)
  b1_w  <- simulate_region(b1, model_choice, matchup_data, 
                           models_list, return_all)
  b2_w  <- simulate_region(b2, model_choice, matchup_data, 
                           models_list, return_all)
  
  if (return_all) {
    # Combine the winners across all 4 regions into master lists
    r64_all <- c(sp1_w$r64, sp2_w$r64, b1_w$r64, b2_w$r64)
    r32_all <- c(sp1_w$r32, sp2_w$r32, b1_w$r32, b2_w$r32)
    s16_all <- c(sp1_w$s16, sp2_w$s16, b1_w$s16, b2_w$s16)
    e8_all  <- c(sp1_w$e8, sp2_w$e8, b1_w$e8, b2_w$e8)
    
    ff_res <- simulate_final_four(sp1_w$e8, sp2_w$e8, b1_w$e8, 
                                  b2_w$e8, model_choice, matchup_data, 
                                  models_list, return_all)
    return(list(
      r64 = r64_all,
      r32 = r32_all,
      s16 = s16_all,
      e8  = e8_all,
      f4  = ff_res$f4,
      champion = ff_res$champion
    ))
  } else {
    champion <- simulate_final_four(sp1_w, sp2_w, b1_w, b2_w, 
                                    model_choice, matchup_data, 
                                    models_list, return_all = FALSE)
    return(list(champion = champion, south_winner = sp1_w, 
                east_winner = sp2_w, west_winner = b1_w, 
                midwest_winner = b2_w))
  }
}
