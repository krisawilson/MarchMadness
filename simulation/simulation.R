# load stuff
library(tidyverse); suppressPackageStartupMessages(library(ranger))
source("simulation/functions/simulation-functions.R")
dat <- read_csv("simulation/input-data.csv")

## this preprocessing has to be done before running
## simulate_tournament()
data_reg <- data |> 
  filter(region1 == region2) |> 
  mutate(region = region1) |> 
  relocate(region, .before = PPG_diff) |> 
  select(-c(region1, region2))
# Preallocate a character vector (if returning just the champion) 
# a list (if returning more info)
N <- 5000; simulation_results <- vector("list", N);

# Loop through the simulations
start <- proc.time()
for(i in 1:N) {
  result <- simulate_tournament(data = dat, model = "rf")
  simulation_results[[i]] <- result
  cat("Iteration", i, "done. Champion:", result$champion, "\n")
}
end <- proc.time()
start - end
# Calculate probabilities of winning the championship
# champion_probs <- prop.table(table(champions))
# print(champion_probs)
# 
# # Extract outcomes
# champions <- sapply(simulation_results, function(x) x$champion)
# sp1_winners <- sapply(simulation_results, function(x) x$sp1)
# sp2_winners <- sapply(simulation_results, function(x) x$sp2)
# b1_winners <- sapply(simulation_results, function(x) x$b1)
# b2_winners <- sapply(simulation_results, function(x) x$b2)
# 
# # Calculate champion winning probabilities.
# champion_probs <- prop.table(table(champions))
# print(champion_probs)
# 
# # For example, to get probabilities for the SPOKANE1 region winner:
# sp1_probs <- prop.table(table(sp1_winners))
# print(sp1_probs)
