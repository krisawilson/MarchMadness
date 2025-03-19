# load stuff
library(tidyverse); suppressPackageStartupMessages(library(ranger));
source("simulation/functions/simulation-functions.R")
dat <- read_csv("simulation/input-data.csv")

## this preprocessing has to be done before running
## simulate_tournament()
data_reg <- dat |> 
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

# save results!
save(simulation_results, file = "simulation/results.RData")

# # Extract champion outcomes from each simulation
# champions <- sapply(simulation_results, function(x) x$champion)
# champion_props <- as.data.frame(prop.table(table(champions))
#                                 ) |> arrange(desc(Freq))
# print(champion_props)
# 
# # Similarly, for the region winners (replace with the actual region names from your simulation)
# sp1_winners <- sapply(simulation_results, function(x) x$sp1)
# sp1_props <- as.data.frame(
#   prop.table(table(sp1_winners))) |> arrange(desc(Freq))
# print(sp1_props)
# 
# sp2_winners <- sapply(simulation_results, function(x) x$sp2)
# sp2_props <- as.data.frame(
#   prop.table(table(sp2_winners))) |> arrange(desc(Freq))
# print(sp2_props)
# 
# b1_winners <- sapply(simulation_results, function(x) x$b1)
# b1_props <- as.data.frame(
#   prop.table(table(b1_winners))) |> arrange(desc(Freq))
# print(b1_props)
# 
# b2_winners <- sapply(simulation_results, function(x) x$b2)
# b2_props <- as.data.frame(
#   prop.table(table(b2_winners))) |> arrange(desc(Freq))
# print(b2_props)
