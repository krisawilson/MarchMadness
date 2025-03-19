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

# prep sim
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

# go to sandbox to assess predictions