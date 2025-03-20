# load stuff
library(tidyverse); suppressPackageStartupMessages(library(ranger));
source("simulation/men/functions/simulation-functions.R")
dat <- read_csv("simulation/men/input-data.csv")

## this preprocessing has to be done before running
## simulate_tournament()
data_reg <- dat |> 
  filter(region1 == region2) |> 
  mutate(region = region1) |> 
  relocate(region, .before = PPG_diff) |> 
  select(-c(region1, region2))

# prep sim
N <- 15000; simulation_results <- vector("list", N);

# Loop through the simulations
start <- proc.time()
for(i in 1:N) {
  result <- simulate_tournament(data = dat, model = "logit_model")
  simulation_results[[i]] <- result
  cat("Iteration", i, "done. Champion:", result$champion, "\n")
}
end <- proc.time()
abs(start - end)

# save results!
save(simulation_results, file = "simulation/men/results.RData")

library(tidyverse); suppressPackageStartupMessages(library(ranger));
source("simulation/functions/simulation-functions.R")
# don't need these functions
rm(simulate_final_four, simulate_tournament, simulate_region)
dat <- read_csv("simulation/input-data.csv")

data_reg <- dat |>
  filter(region1 == region2) |>
  mutate(region = region1) |>
  relocate(region, .before = PPG_diff) |>
  select(-c(region1, region2))
rm(dat)

# grab each region
south <- data_reg |> filter(region == "south")
east <- data_reg |> filter(region == "east")
west <- data_reg |> filter(region == "west")
midwest <- data_reg |> filter(region == "midwest")

# A helper function to simulate one region and return block winners and region winner
simulate_region_with_blocks <- function(region_data, model) {
  # Build the bracket from the matchup data for this region
  bracket <- build_bracket(region_data)
  
  # Create blocks based on seed filtering
  block1 <- bracket |> filter(seed %in% c(1,16,8,9))
  block2 <- bracket |> filter(seed %in% c(5,12,4,13))
  block3 <- bracket |> filter(seed %in% c(6,11,3,14))
  block4 <- bracket |> filter(seed %in% c(7,10,2,15))
  
  # Get winners of each block
  block_winner1 <- simulate_block(block1, model, region_data)
  block_winner2 <- simulate_block(block2, model, region_data)
  block_winner3 <- simulate_block(block3, model, region_data)
  block_winner4 <- simulate_block(block4, model, region_data)
  
  # Play semifinals and final to get region winner
  semi1_winner <- simulate_game(block_winner1, block_winner2,
                                model, region_data)
  semi2_winner <- simulate_game(block_winner3, block_winner4,
                                model, region_data)
  region_winner  <- simulate_game(semi1_winner, semi2_winner,
                                  model, region_data)
  
  return(list(block1 = block_winner1,
              block2 = block_winner2,
              block3 = block_winner3,
              block4 = block_winner4,
              semi1  = semi1_winner,
              semi2  = semi2_winner,
              region = region_winner))
}

N <- 15000; simulation_results <- vector("list", N);

start <- proc.time()
for (i in 1:N) {
  # For each simulation, simulate each region separately
  result <- list(
    sp1  = simulate_region_with_blocks(south,  model = "logit_model"),
    sp2  = simulate_region_with_blocks(east,  model = "logit_model"),
    b1 = simulate_region_with_blocks(west, model = "logit_model"),
    b2 = simulate_region_with_blocks(midwest, model = "logit_model")
  )
  
  simulation_results[[i]] <- result
  cat("Iteration", i, "done.\n")
}
(end <- proc.time() - start)

save(simulation_results, file ="simulation/men/results.RData")
# final rounds results ----
#load("simulation/results.RData")
# Extract champion outcomes from each simulation
champions <- sapply(simulation_results, function(x) x$champion)
as.data.frame(prop.table(table(champions))) |> arrange(desc(Freq))
#
# # Similarly for the region winners
sp1_winners <- sapply(simulation_results, function(x) x$sp1)
as.data.frame(prop.table(table(sp1_winners))) |> arrange(desc(Freq))

sp2_winners <- sapply(simulation_results, function(x) x$sp2)
as.data.frame(prop.table(table(sp2_winners))) |> arrange(desc(Freq))

b1_winners <- sapply(simulation_results, function(x) x$b1)
as.data.frame(prop.table(table(b1_winners))) |> arrange(desc(Freq))

b2_winners <- sapply(simulation_results, function(x) x$b2)
as.data.frame(prop.table(table(b2_winners))) |> arrange(desc(Freq))

# preliminary rounds simulations ----
# # south ----
# 
# # south, block 1 winner
# sp1_block1 <- sapply(simulation_results,
#                      function(x) x$sp1)
# a1 <- as.data.frame(prop.table(table(sp1_block1))) |> arrange(desc(Freq))
# 
# # south, block 2 winner
# sp1_block2 <- sapply(simulation_results,
#                      function(x) x$sp2)
# a2 <- as.data.frame(prop.table(table(sp1_block2))) |> arrange(desc(Freq))
# 
# # south, block 3 winner
# sp1_block3 <- sapply(simulation_results,
#                      function(x) x$sb1)
# a3 <- as.data.frame(prop.table(table(sp1_block3))) |> arrange(desc(Freq))
# 
# # south, block 4 winner
# sp1_block4 <- sapply(simulation_results,
#                      function(x) x$b2)
# a4 <- as.data.frame(prop.table(table(sp1_block4))) |> arrange(desc(Freq))
# 
# # east ----
# 
# # east, block 1 winner
# sp2_block1 <- sapply(simulation_results,
#                      function(x) x$sp1)
# a8 <- as.data.frame(prop.table(table(sp2_block1))) |> arrange(desc(Freq))
# 
# # east, block 2 winner
# sp2_block2 <- sapply(simulation_results,
#                      function(x) x$sp2)
# a9 <- as.data.frame(prop.table(table(sp2_block2))) |> arrange(desc(Freq))
# 
# # east, block 3 winner
# sp2_block3 <- sapply(simulation_results,
#                      function(x) x$b1)
# a10 <- as.data.frame(prop.table(table(sp2_block3))) |> arrange(desc(Freq))
# 
# # east, block 4 winner
# sp2_block4 <- sapply(simulation_results,
#                      function(x) x$b2)
# a11 <- as.data.frame(prop.table(table(sp2_block4))) |> arrange(desc(Freq))
# 
# # west ----
# # west, block 1 winner
# b1_block1 <- sapply(simulation_results,
#                     function(x) x$b11)
# a15 <- as.data.frame(prop.table(table(b1_block1))) |> arrange(desc(Freq))
# 
# # west, block 2 winner
# b1_block2 <- sapply(simulation_results,
#                     function(x) x$b12)
# a16 <- as.data.frame(prop.table(table(b1_block2))) |> arrange(desc(Freq))
# 
# # west, block 3 winner
# b1_block3 <- sapply(simulation_results,
#                     function(x) x$b13)
# a17 <- as.data.frame(prop.table(table(b1_block3))) |> arrange(desc(Freq))
# 
# # west, block 4 winner
# b1_block4 <- sapply(simulation_results,
#                     function(x) x$b14)
# a18 <- as.data.frame(prop.table(table(b1_block4))) |> arrange(desc(Freq))
# 
# 
# # midwest ----
# # midwest, block 1 winner
# b2_block1 <- sapply(simulation_results,
#                     function(x) x$b21)
# a22 <- as.data.frame(prop.table(table(b2_block1))) |> arrange(desc(Freq))
# 
# # midwest, block 2 winner
# b2_block2 <- sapply(simulation_results,
#                     function(x) x$b22)
# a23 <- as.data.frame(prop.table(table(b2_block2))) |> arrange(desc(Freq))
# 
# # midwest, block 3 winner
# b2_block3 <- sapply(simulation_results,
#                     function(x) x$b23)
# a24 <- as.data.frame(prop.table(table(b2_block3))) |> arrange(desc(Freq))
# 
# # midwest, block 4 winner
# b2_block4 <- sapply(simulation_results,
#                     function(x) x$b24)
# a25 <- as.data.frame(prop.table(table(b2_block4))) |> arrange(desc(Freq))
# 
# save(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,
#      a20,a21,a22,a23,a24,a25,a26,a27,a28, 
#      file = "simulation/men/prelim-rounds.RData")
# 
