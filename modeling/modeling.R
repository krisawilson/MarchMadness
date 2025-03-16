# packages
library(tidyverse)

# read in data
standardized_data <- read_csv("data/standardized-data.csv")

# extract just covariates for my sanity
dat <- standardized_data |> 
  select(team1_win:length(standardized_data))

# first up: logistic regression

# second up: penalized logistic regression

# third up: gams!

# fourth up: tree-based methods. requires original data!