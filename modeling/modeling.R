# packages
library(tidyverse)

# read in data
standardized_data <- read_csv("data/standardized-data.csv")

# extract just covariates for my sanity
dat <- standardized_data # |> 