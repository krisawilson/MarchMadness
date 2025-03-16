# DATA CLEANING FUNCTIONS ----

# Load required libraries
#require(rvest, quietly = TRUE)
#require(dplyr, quietly = TRUE)
#require(purrr, quietly = TRUE)
#require(stringr, quietly = TRUE)

extract_team_info <- function(team_node) {
  #####
  ## Parameters ##
  
  # Input: team_node: from HTML
  
  # Output: list of seeds, teams, score
  #####
  # 3xtract seed (first <span>)
  seed <- team_node |> html_node("span") |> html_text(trim = TRUE)
  
  # 3xtract team name and score:
  # the first <a> is the team name, the second <a> is the score
  a_tags <- team_node |> html_nodes("a")
  
  team_name <- {
    if (length(a_tags) >= 1) {
      a_tags[1] |> html_text(trim = TRUE)
    } else {
      NA
    }
  }
  
  score <- {
    if (length(a_tags) >= 2) {
      a_tags[2] |> html_text(trim = TRUE)
    } else {
      NA
    }
  }
  
  # arrange output as list
  list(
    seed  = as.numeric(seed),
    team  = team_name,
    score = as.numeric(score)
  )
}

# Define the main function that scrapes a bracket page and returns a data frame.
scrape_bracket <- function(url, year) {
  #####
  ## Parameters ##
  
  # Input: 
  # url: url for the March Madness bracket
  # year: year of the tournament
  
  # Output: final_df: data frame of teams, scores, and round of tournament
  #####
  if (year < 2010 | year > 2025) {
    stop("Year must be between 2010 and 2025.")
  }
  # Read the HTML page from the URL
  page <- read_html(url)
  
  # in the HTML, each game is a child of a <div class="round">.
  game_nodes <- page |> html_nodes("div.round > div")
  
  games_list <- list() # initialize list to store results
  
  # loop over each node to get the games
  for (i in seq_along(game_nodes)) {
    game <- game_nodes[i]
    
    team_nodes <- game |> html_nodes(xpath = "./div")
    
    if (length(team_nodes) < 2) next # error handling
    
    # extract info for each team using extract_team_info
    team1_info <- extract_team_info(team_nodes[1])
    team2_info <- extract_team_info(team_nodes[2])
    
    # in the HTML, the winning team is indicated by having a class     
    # "winner" on its team block. thanks sports reference!
    team1_class <- team_nodes[1] |> html_attr("class")
    team2_class <- team_nodes[2] |> html_attr("class")
    
    winner <- if (!is.na(team1_class) && grepl("winner", team1_class)) {
      team1_info$team
    } else if (!is.na(team2_class) && grepl("winner", team2_class)) {
      team2_info$team
    } else {
      NA  # should have it throw an error instead
    }
    
    # Create a data frame row for this game
    game_df <- data.frame(
      seed1  = team1_info$seed,
      team1  = team1_info$team,
      seed2  = team2_info$seed,
      team2  = team2_info$team,
      score1 = team1_info$score,
      score2 = team2_info$score,
      winner = winner,
      stringsAsFactors = FALSE
    )
    games_list[[length(games_list) + 1]] <- game_df
  }
  
  # combine into one data frame
  result_df <- bind_rows(games_list)
  
  final_df <- result_df |>
    mutate(
      # add column for regionals vs final four
      region = case_when(
        row_number() <= 15 ~ "region1", # regional rounds
        row_number() > 15 & row_number() <= 30 ~ "region2", 
        row_number() > 30 & row_number() <= 45 ~ "region3", 
        row_number() > 45 & row_number() <= 60 ~ "region4", 
        # final four
        row_number() > 60 & row_number() <= 62 ~ "final four", 
        row_number() == 63 ~ "championship"), 
      upset = case_when(
        winner == team1 & seed1 > seed2 ~ "yes",
        winner == team2 & seed2 > seed1 ~ "yes",
        TRUE ~ "no"),
      year = year
    )
  return(final_df)
}

# convert webpage into data frame!
clean_data <- function(url, year) {
  #####
  ## Parameters ##
  
  # Input: 
  # url: url for NCAA team stats
  # year: year of the tournament
  
  # Output: clean_table: data frame of cleaned data
  #####
  if (year < 2010 | year > 2025) {
    stop("Year must be between 2010 and 2025.")
  }
  # read in webpage
  webpage <- read_html(url)
  
  # turn it into data frame
  table_node <- webpage |>
    html_node("div#div_ratings table#ratings") |>
    html_table(fill = TRUE)
  
  colnames(table_node) <- 1:length(table_node) 
  #assign temporary column names
  
  # remove empty cols
  table_fix <- table_node |>  select(-c("4", "10", "12"))
  
  # extract the first row as new column names
  new_names <- table_fix[1, ] |> unlist() |> as.character()
  
  # remove the first row and assign new column names.
  # clean remaining data as well
  clean_table <- table_fix[-1, ] |> 
    setNames(new_names) |>
    
    # remove rows w/o acutal data
    filter(Rk != "Rk", OSRS != "SRS") |>  
    rename("Adj_ORtg" = "ORtg", # adjusted stats
           "Adj_DRtg" = "DRtg",
           "PPG" = "Pts", 
           "PPG_Allowed" = "Opp") |>
    # remove total stats
    select(-c("SRS", "NRtg")) |> 
    
    # convert all columns except 2 & 3 to numeric
    mutate(across(-c(2, 3), as.numeric),
           year = year)  
  
  return(clean_table)
}

get_pace <- function(url, year) {
  if (year < 2010 | year > 2025) {
    stop("Year must be between 2010 and 2025.")
  }
  # Read the webpage and extract the table
  page <- read_html(url)
  table_node <- page |> 
    html_element("div#div_adv_school_stats table#adv_school_stats") |> 
    html_table(fill = TRUE)
  
  colnames(table_node) <- 1:length(table_node)
  
  tab <- table_node |> select(c("1", "2", "22"))
  
  new_names <- tab[1, ] |> unlist() |> as.character()
  
  clean_tab <- tab[-1, ] |> 
    setNames(new_names) |> 
    filter(Rk != "Rk", Pace != "School Advanced") |> 
    mutate(across(c(1, 3), as.numeric),
           year = year,
    # fix the case where NCAA was left in the stats table
           School = str_replace(School,"\\s*NCAA\\s*$", "")) |> 
    select(-"Rk")
  
  return(clean_tab)
}

team_names <- function(col) {
  case_when(
    col == "North Carolina" ~ "UNC",
    col == "Louisiana State" ~ "LSU",
    col == "Connecticut" ~ "UConn",
    col == "Massachusetts" ~ "UMass",
    col == "Mississippi" ~ "Ole Miss",
    col == "Southern California" ~ "USC",
    col == "Brigham Young" ~ "BYU",
    col == "Nevada-Las Vegas" ~ "UNLV",
    col == "UC-Irvine" ~ "UC Irvine",
    col == "IU Indy" ~ "IU Indianapolis",
    TRUE ~ col  
  )
}
