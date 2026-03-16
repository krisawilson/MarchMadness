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

# 2. define first four. update with 2026
# The 8 teams playing in the 4 First Four games
first_four_matchups <- tibble(
  team_a = c("NC State", "UMBC", "Prairie View", "SMU"),
  team_b = c("Texas", "Howard", "Lehigh", "Miami (OH)"),
  seed   = c(11, 11, 16, 16),
  # need to check the regions
  region = c("west", "south", "midwest", "midwest")
)

# Extract your base 60 teams (teams NOT in the First Four)
# Assuming you have a dataframe of all 68 tournament teams called `all_68_teams`
base_60_teams <- all_68_teams |> 
  filter(!team %in% c(first_four_matchups$team_a, first_four_matchups$team_b)) |>
  select(team, seed, region)

N <- 15000
simulation_results <- vector("list", N) # initialization
chosen_model <- "ensemble_weighted" # can easily change

start <- proc.time()
for(i in 1:N) {
  
  # Step A: Simulate First Four to get the 4 winners
  ff_winners <- simulate_first_four(first_four, 
                                    chosen_model, dat, models)
  # step B
  current_64_bracket <- insert_ff_winners(
    base_60_teams, ff_winners
  )
  # Step C: Run the main 64-team simulation
  result <- simulate_tournament(data = current_64_bracket, 
                                model = chosen_model, 
                                models_list = models)
  
  simulation_results[[i]] <- result
  
  if(i %% 10 == 0) cat("Iteration", i, "completed...\n")
}
end <- proc.time()
print(end - start)

save(simulation_results, file = "simulation/men/results.RData")