# packages and preprep----
#setwd(C:\MarchMadness)
suppressPackageStartupMessages(library(tidyverse))
library(caret) # for parition and training setups
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
    TRUE ~ NA),
    postseason = as.factor(postseason), # turn to factor
  # order to create ordered factor for polr
    postseason = reorder(postseason, finish, order = TRUE))

# set a: preserve class distribution (imbalance) ----
set.seed(425440)
ctrl <- trainControl(method = "cv", number = 5) # set up CV
ind <- createDataPartition(df$postseason, p = 0.75, list = F) # split data
train_df <- df[ind,]
test_df <- df[-ind,]

# model 1a: ordered logistic regression ----
ordr1 <- train(
  postseason ~ adj_oe + adj_de + barthag + efg_o + efg_d + tor + tor_d +
    orb + drb + ftr + ftr_d + x2p_o + x2p_d + x3p_o + x3p_d + adj_t + wab, 
  data = train_df, method = "polr",
  trControl = ctrl
  )
summary(ordr1)
# remove insignif vars
ordr2 <- train(postseason ~ adj_oe + adj_de + barthag + x2p_d + wab, 
               data = train_df, method = "polr", trControl = ctrl)
summary(ordr2)
# check variable importance:
varImp(ordr2, scale = FALSE) # from caret
rm(ordr1, df, ind)
# model 2a: regularized logistic regression ----

# set 2: addressing response imbalance ----
ctrl2 <- trainControl(method = "cv", number = 5, sampling = "smote")