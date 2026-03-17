library(tidyverse)

# 1. Load the results from your simulation loop
load("simulation/men/results.RData")

# 2. Set the number of simulations (should match your loop)
N <- length(simulation_results)

# 3. Create a fast extraction function
# This digs into the massive list and counts how many times each team appeared in each round
get_round_probs <- function(sim_data, round_name) {
  # Extract that specific round from all 15,000 lists
  all_teams_in_round <- purrr::map(sim_data, round_name) |> unlist()
  
  # Count them up and divide by N to get the percentage!
  tibble(team = all_teams_in_round) |>
    count(team, name = paste0(round_name, "_count")) |>
    mutate(!!paste0(round_name, "_prob") := !!sym(paste0(round_name, "_count")) / N) |>
    select(team, ends_with("_prob"))
}

# 4. Calculate probabilities for every round
r32_probs <- get_round_probs(simulation_results, "r64") # Winning R64 means making R32
s16_probs <- get_round_probs(simulation_results, "r32") # Winning R32 means making Sweet 16
e8_probs  <- get_round_probs(simulation_results, "s16") # Winning Sweet 16 means making Elite 8
f4_probs  <- get_round_probs(simulation_results, "e8")
champ_probs <- get_round_probs(simulation_results, "champion")

# 5. Join them all together into one Master Analytics Table
# use full_join because teams may never make later rounds
bracket_analytics <- r32_probs |>
  full_join(s16_probs, by = "team") |>
  full_join(e8_probs, by = "team") |>
  full_join(f4_probs, by = "team") |>
  full_join(champ_probs, by = "team") |>
  # Replace NA values (0% chance) with 0
  mutate(across(where(is.numeric), ~replace_na(., 0))) |>
  # Sort by highest chance to win the Championship
  arrange(desc(champion_prob))

# 6. View the results and save them for your bracket pool!
View(bracket_analytics)
write_csv(bracket_analytics, "analysis/men/2026_bracket_optimal_picks.csv")