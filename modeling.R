# packages and preprep----
#setwd(C:\MarchMadness)
suppressPackageStartupMessages(library(tidyverse))
library(caret) # for parition and training setups
library(themis) # for smote
library(glmnet) # for regularized regression
library(xgboost) # xgboost
library(rpart) # for decision tree

# read in data
df <- read_csv("data/clean-data.csv")
df <- df |> # add ordinal representation
  mutate(finish = case_when(
    postseason == "R68" ~ 0, postseason == "R64" ~ 1,
    postseason == "R32" ~ 2, postseason == "S16" ~ 3,
    postseason == "E8" ~ 4, postseason == "F4" ~ 5,
    postseason == "2ND" ~ 6, postseason == "Champions" ~ 7,
    TRUE ~ NA))

# set 1: not addressing response imbalance ----

# set 2: addressing response imbalance ----