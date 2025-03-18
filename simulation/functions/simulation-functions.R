compare_stats <- function(df, row1, row2) {
  #####
  # This function takes in a data frame df, and two rows of this
  # data frame. The function computes the differences between
  # select columns of the data frame, and returns these
  # differences as a named vector. A helper for 
  # pairwise_differences() function
  
  ## INPUTS:
  # df: data frame containing column "PPG"
  # row1: positive integer containing row index
  # row2: positive integer containing row index
  
  ## OUTPUT:
  # diff_vals: a vector containing the column differences
  #####
  
  # finds column index where ppg is
  start_col <- which(names(df) == "PPG")
  if (length(start_col) == 0) {
    stop("Column 'PPG' not found in the data frame.")
  }
  
  # gets all columns from ppg to the end
  stat_cols <- names(df)[start_col:ncol(df)]
  
  # index check
  if (row1 > nrow(df) || row2 > nrow(df)) {
    stop("One or both row indices exceed the number of rows in the data frame.")
  }
  
  # computes differences
  diff_vals <- df[row1, stat_cols] - df[row2, stat_cols]
  return(unlist(diff_vals))
}


pairwise_differences <- function(df) {
  #####
  # This function takes compare_stats() and applies it to all
  # unique row index pairs in the data frame df. Outputs these
  # into a data frame.
  #####
  
  results_list <- list() #initialize results
  
  # generate all unique row index pairs
  pairs <- combn(1:nrow(df), 2)
  
  # loop over each pair and compute the differences
  for (i in 1:ncol(pairs)) {
    r1 <- pairs[1, i]
    r2 <- pairs[2, i]
    diff_vec <- compare_stats(df, r1, r2)
    
    # rename--will help us later
    names(diff_vec) <- paste0(names(diff_vec), "_diff")
    
    # convert to data frame row
    result_row <- data.frame(
      team1 = df$team[r1],
      seed1 = df$seed[r1],
      region1 = df$region[r1],
      team2 = df$team[r2],
      seed2 = df$seed[r2],
      region2 = df$region[r2],
      t(diff_vec), # transpose so that stats become columns
      row.names = NULL,
      stringsAsFactors = FALSE
    )
    
    results_list[[i]] <- result_row
  }
  
  # combine all rows into one data frame
  pairwise_df <- do.call(rbind, results_list)
  return(pairwise_df)
}