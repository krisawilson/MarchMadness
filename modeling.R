# packages and preprep----
#setwd(C:\MarchMadness)
suppressPackageStartupMessages(library(tidyverse))
library(caret) # for parition and training setups
#require(themis) # used in smote
library(glmnet) # for regularized regression
library(xgboost) # xgboost
library(rpart) # for decision tree
#require(MASS) # used for polr. masks dplyr::select . gonna use caret tho

# read in data
df <- read_csv("data/clean-data.csv")
df <- df |> # add ordinal representation
  mutate(finish = case_when(
    postseason == "R68" ~ 0, postseason == "R64" ~ 1,
    postseason == "R32" ~ 2, postseason == "S16" ~ 3,
    postseason == "E8" ~ 4, postseason == "F4" ~ 5,
    postseason == "2ND" ~ 6, postseason == "Champions" ~ 7,
    TRUE ~ NA),
    postseason = as.factor(postseason))

# set 1: preserve class distribution (imbalance) ----
set.seed(1056)
ctrl <- trainControl(method = "cv", number = 5) # set up CV
ind <- createDataPartition(df$postseason, p = 0.75, list = F) # split data
train_df <- df[ind,]
test_df <- df[-ind,]

# model 1: ordered logistic regression

# set 2: addressing response imbalance ----
ctrl2 <- trainControl(method = "cv", number = 5, sampling = "smote")