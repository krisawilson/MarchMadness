# simulation.R
library(tidyverse)
library(xgboost)
library(caret)
library(mgcv)

source("simulation/men/functions/simulation-functions.R")

# 1. Load the compiled matchups and the models
dat <- read_csv("simulation/men/input-data.csv")
load("modeling/men/models.RData") 

# Put models in a list to pass to functions cleanly
models <- list(
  gam_model   = gam_model,
  xgb_model   = xgb_model,
  en_model    = en_model,
  logit_model = logit_model,
  rf_model    = rf_model,
  bayes_model = bayes_model
)

# define first four. this is for 2026
first_four_matchups <- tibble(
  team_a = c("NC State", "UMBC", "Prairie View A&M", "Southern Methodist"),
  team_b = c("Texas", "Howard", "Lehigh", "Miami (OH)"),
  seed   = c(11, 11, 16, 16),
  # need to check the regions
  region = c("West", "Midwest", "South", "Midwest")
)

# extract all teams
all_teams <- dat |> 
  select(team = team1, seed = seed1, region = region1) |> 
  distinct()

# get field that doesn't have to play-in
base_60_teams <- all_teams |> 
  # could use filter(!team %in% ...)
  filter_out(team %in% c(first_four_matchups$team_a, 
                         first_four_matchups$team_b))

N <- 20000
simulation_results <- vector("list", N) # initialization
chosen_model <- "ensemble_weighted" # can easily change

start <- proc.time()
for(i in 1:N) {
  
  # Step A: Simulate First Four to get the 4 winners
  ff_winners <- simulate_first_four(first_four_matchups, 
                                    chosen_model, dat, models)
  # step B
  current_64_bracket <- insert_ff_winners(
    base_60_teams, ff_winners
  )
  # Step C: Run the main 64-team simulation
  result <- simulate_tournament(bracket_64 = current_64_bracket, 
                                matchup_data = dat,
                                model_choice = chosen_model, 
                                models_list = models,
                                return_all = TRUE)
  
  simulation_results[[i]] <- result
  
  if(i %% 100 == 0) cat("Iteration", i, "completed...\n")
}
end <- proc.time()
print(end - start)

save(simulation_results, file = "analysis/men/results.RData")