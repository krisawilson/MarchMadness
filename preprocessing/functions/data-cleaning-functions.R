# NCAAW DATA CLEANING FUNCTIONS ----

extract_team_info_wbb <- function(team_node) {
  #####
  # This function takes in one input, an HTML node, representing
  # each team for a March Madness game. The function outputs
  # a list, containing the seeds for each team, the team name,
  # and the score of the game.
  ## Parameters ##
  
  # Input: team_node: from HTML
  
  # Output: list of seeds, teams, score
  #####
  
  # first, check if rvest loaded in. if not, load it in
  if (!"rvest" %in% .packages()) {
    library(rvest)
  }
  
  # extract seed (first <span>)
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

scrape_bracket_wbb <- function(url, year) {
  #####
  # This function takes in two inputs, a string url, and a year.
  # This function creates the tournament bracket for a given 
  # year for the women's NCAA tournament.

  ## inputs ##
  # url (string): a string with the url/link to the webpage
  # year (int): a year 2010 or later
  
  ## outputs ##
  # final_df: a data frame where each row is an NCAA tournament
  # game. This includes seeds, scores, the winner, round of
  # the tournament, and whether the game was an upset
  #####
  
  # load required packages if not loaded in already
  packages <- c("dplyr", "rvest") # grab packages
  for (pkg in packages) {
    if (!pkg %in% .packages()) {
      library(pkg, character.only = TRUE)
    }
  }
  
  # year check
  if (year < 2009) {
    stop("Year must be 2010 or later.")
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
    team1_info <- extract_team_info_wbb(team_nodes[1])
    team2_info <- extract_team_info_wbb(team_nodes[2])
    
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
      seed1  = team1_info$seed, team1  = team1_info$team,
      seed2  = team2_info$seed, team2  = team2_info$team,
      score1 = team1_info$score, score2 = team2_info$score,
      winner = winner, stringsAsFactors = FALSE
    )
    games_list[[length(games_list) + 1]] <- game_df
  }
  
  # combine into one data frame
  result_df <- bind_rows(games_list)
  
  # create final data frame
  final_df <- result_df |>
    mutate(
      # add column for regionals vs final four
      region = case_when(
        row_number() <= 15 ~ "region1", # regional rounds
        row_number() > 15 & row_number() <= 30 ~ "region2", 
        row_number() > 30 & row_number() <= 45 ~ "region3", 
        row_number() > 45 & row_number() <= 60 ~ "region4", 
        row_number() > 60 & row_number() <= 62 ~ "final four", 
        row_number() == 63 ~ "championship"), 
      # add column for year
      year = year
    )
  return(final_df)
}

scrape_adv_stats_wbb <- function(url, year) {
  #####
  # This function takes in two inputs, a string url, and a year.
  # This function converts the SR webpage of advanced statistics
  # into a data frame of advanced statistics for women's 
  # basketball teams for a given NCAA season.

  ## inputs ##
  # url (string): a string with the url/link to the webpage
  # year (int): a year between 2010 or later
  
  ## outputs ##
  # clean_table: a data frame which contains the school/university
  # and advanced statistics.
  #####
  packages <- c("dplyr", "rvest") # grab packages
  for (pkg in packages) {
    if (!pkg %in% .packages()) {
      library(pkg, character.only = TRUE)
    }
  }
  # year check
  if (year < 2009) {
    stop("Year must be 2010 or later.")
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

get_pace_wbb <- function(url, year) {
  #####
  # This function takes in two inputs, a string url, and a year.
  # Very similar to the above function, this function extracts
  # the pace column from sports reference's wbb advanced stats
  
  ## inputs ##
  # url (string): a string with the url/link to the webpage
  # year (int): a year between 2010 and 2025
  
  ## outputs ##
  # clean_tab: a data frame which contains the school/university
  # and their pace of play
  #####
  packages <- c("dplyr", "rvest", "stringr")
  for (pkg in packages) {
    if (!pkg %in% .packages()) {
      library(pkg, character.only = TRUE)
    }
  }
  # year check
  if (year < 2009) {
    stop("Year must be 2010 or later.")
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

team_names_wbb <- function(col) {
  #####
  # This function is meant to be applied on a column in a 
  # dataset, i.e., used inside of `mutate()` or similar.
  # Takes in one input, the column, and edits the values of
  # the column as seen below.
  #####
  if (!"dplyr" %in% .packages()) {
    library(dplyr)
  }
  # manually fix name discrepancies
  case_when(
    col == "North Carolina" ~ "UNC",
    col == "Louisiana State" ~ "LSU",
    col == "Connecticut" ~ "UConn",
    col == "Massachusetts" ~ "UMass",
    col == "Mississippi" ~ "Ole Miss",
    col == "Southern California" ~ "USC",
    col == "Brigham Young" ~ "BYU",
    col == "Nevada-Las Vegas" ~ "UNLV",
    col == "UC Irvine" ~ "UC-Irvine",
    col == "UC Davis" ~ "UC-Davis",
    col == "IU Indy" ~ "IU Indianapolis",
    TRUE ~ col  
  )
}
