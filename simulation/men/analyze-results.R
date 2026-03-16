library(tidyverse); suppressPackageStartupMessages(library(ranger));
source("simulation/men/functions/simulation-functions.R")
# don't need these functions
rm(simulate_final_four, simulate_tournament, simulate_region)
dat <- read_csv("simulation/men/input-data.csv")

data_reg <- dat |> 
  filter(region1 == region2) |> 
  mutate(region = region1) |> 
  relocate(region, .before = PPG_diff) |> 
  select(-c(region1, region2))
rm(dat)

# grab each region
east <- data_reg |> filter(region == "east")
south <- data_reg |> filter(region == "south")
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

N <- 1000; simulation_results <- vector("list", N); 

start <- proc.time()
for (i in 1:N) {
  # For each simulation, simulate each region separately
  result <- list(
    e  = simulate_region_with_blocks(east,  model = "logit_model"),
    s  = simulate_region_with_blocks(south,  model = "logit_model"),
    w = simulate_region_with_blocks(west, model = "logit_model"),
    mw = simulate_region_with_blocks(midwest, model = "logit_model")
  )
  
  simulation_results[[i]] <- result
  cat("Iteration", i, "done.\n")
}
(end <- proc.time() - start)

# east ----

# east, block 1 winner
e_block1 <- sapply(simulation_results, 
                     function(x) x$e$block1)
as.data.frame(prop.table(table(e_block1))) |> arrange(desc(Freq))

# east, block 2 winner
e_block2 <- sapply(simulation_results, 
                     function(x) x$e$block2)
as.data.frame(prop.table(table(e_block2))) |> arrange(desc(Freq))

# east, block 3 winner
e_block3 <- sapply(simulation_results, 
                     function(x) x$e$block3)
as.data.frame(prop.table(table(e_block3))) |> arrange(desc(Freq))

# east, block 4 winner
e_block4 <- sapply(simulation_results, 
                     function(x) x$e$block4)
as.data.frame(prop.table(table(e_block4))) |> arrange(desc(Freq))

# east, semi winners
e_semi1 <- sapply(simulation_results, 
                    function(x) x$e$semi1)
as.data.frame(prop.table(table(e_semi1))) |> arrange(desc(Freq))

e_semi2 <- sapply(simulation_results, 
                    function(x) x$e$semi2)
as.data.frame(prop.table(table(e_semi2))) |> arrange(desc(Freq))

# east region winner
e_region <- sapply(simulation_results, 
                     function(x) x$e$region)
as.data.frame(prop.table(table(e_region))) |> arrange(desc(Freq))

# south ----

# south, block 1 winner
s_block1 <- sapply(simulation_results, 
                     function(x) x$s$block1)
as.data.frame(prop.table(table(s_block1))) |> arrange(desc(Freq))

# south, block 2 winner
s_block2 <- sapply(simulation_results, 
                     function(x) x$s$block2)
as.data.frame(prop.table(table(s_block2))) |> arrange(desc(Freq))

# south, block 3 winner
s_block3 <- sapply(simulation_results, 
                     function(x) x$s$block3)
as.data.frame(prop.table(table(s_block3))) |> arrange(desc(Freq))

# south, block 4 winner
s_block4 <- sapply(simulation_results, 
                     function(x) x$s$block4)
as.data.frame(prop.table(table(s_block4))) |> arrange(desc(Freq))

# south, semi winners
s_semi1 <- sapply(simulation_results, 
                    function(x) x$s$semi1)
as.data.frame(prop.table(table(s_semi1))) |> arrange(desc(Freq))

s_semi2 <- sapply(simulation_results, 
                    function(x) x$s$semi2)
as.data.frame(prop.table(table(s_semi2))) |> arrange(desc(Freq))

# south region winner
s_region <- sapply(simulation_results, 
                     function(x) x$s$region)
as.data.frame(prop.table(table(s_region))) |> arrange(desc(Freq))

# west ----
# west, block 1 winner
w_block1 <- sapply(simulation_results, 
                    function(x) x$w$block1)
as.data.frame(prop.table(table(w_block1))) |> arrange(desc(Freq))

# west, block 2 winner
w_block2 <- sapply(simulation_results, 
                    function(x) x$w$block2)
as.data.frame(prop.table(table(w_block2))) |> arrange(desc(Freq))

# west, block 3 winner
w_block3 <- sapply(simulation_results, 
                    function(x) x$w$block3)
as.data.frame(prop.table(table(w_block3))) |> arrange(desc(Freq))

# west, block 4 winner
w_block4 <- sapply(simulation_results, 
                    function(x) x$w$block4)
as.data.frame(prop.table(table(w_block4))) |> arrange(desc(Freq))

# west, semi winners
w_semi1 <- sapply(simulation_results, 
                   function(x) x$w$semi1)
as.data.frame(prop.table(table(w_semi1))) |> arrange(desc(Freq))

w_semi2 <- sapply(simulation_results, 
                   function(x) x$w$semi2)
as.data.frame(prop.table(table(w_semi2))) |> arrange(desc(Freq))

# west region winner
w_region <- sapply(simulation_results, 
                    function(x) x$w$region)
as.data.frame(prop.table(table(w_region))) |> arrange(desc(Freq))

# midwest ----
# midwest, block 1 winner
mw_block1 <- sapply(simulation_results, 
                    function(x) x$mw$block1)
as.data.frame(prop.table(table(mw_block1))) |> arrange(desc(Freq))

# midwest, block 2 winner
mw_block2 <- sapply(simulation_results, 
                    function(x) x$mw$block2)
as.data.frame(prop.table(table(mw_block2))) |> arrange(desc(Freq))

# midwest, block 3 winner
mw_block3 <- sapply(simulation_results, 
                    function(x) x$mw$block3)
as.data.frame(prop.table(table(mw_block3))) |> arrange(desc(Freq))

# midwest, block 4 winner
mw_block4 <- sapply(simulation_results, 
                    function(x) x$mw$block4)
as.data.frame(prop.table(table(mw_block4))) |> arrange(desc(Freq))

# midwest, semi winners
mw_semi1 <- sapply(simulation_results, 
                   function(x) x$mw$semi1)
as.data.frame(prop.table(table(mw_semi1))) |> arrange(desc(Freq))

mw_semi2 <- sapply(simulation_results, 
                   function(x) x$mw$semi2)
as.data.frame(prop.table(table(mw_semi2))) |> arrange(desc(Freq))

# midwest region winner
mw_region <- sapply(simulation_results, 
                    function(x) x$mw$region)
as.data.frame(prop.table(table(mw_region))) |> arrange(desc(Freq))


# # final rounds results ----
# load("simulation/results.RData")
# # Extract champion outcomes from each simulation
champions <- sapply(simulation_results, function(x) x$champion)
as.data.frame(prop.table(table(champions))) |> arrange(desc(Freq))
#
# # Similarly for the region winners
e_winners <- sapply(simulation_results, function(x) x$sp1)
as.data.frame(prop.table(table(e_winners))) |> arrange(desc(Freq))

s_winners <- sapply(simulation_results, function(x) x$sp2)
as.data.frame(prop.table(table(s_winners))) |> arrange(desc(Freq))

w_winners <- sapply(simulation_results, function(x) x$b1)
as.data.frame(prop.table(table(w_winners))) |> arrange(desc(Freq))

mw_winners <- sapply(simulation_results, function(x) x$b2)
as.data.frame(prop.table(table(mw_winners))) |> arrange(desc(Freq))
