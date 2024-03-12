# packages ----
suppressPackageStartupMessages(library(tidyverse))
library(caret) # for parition, training, and cross validation
library(glmnet) # for regularized classification model
library(rpart) # for decision tree
library(ranger) # random forest classification tree
library(ordinal) # for linear classification models

# setup ----
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

# standardize data, as this improves stability of some models
std_df <- df |> 
  group_by(year) |> # want statistics relative to year
  mutate(across(adj_oe:wab, ~as.vector(scale(.)))) |> ungroup()

# split training and test data
set.seed(425440)
ctrl <- trainControl(method = "cv", number = 5) # set up CV
ind <- createDataPartition(df$postseason, p = 0.75, list = F) # split data
train_df <- std_df[ind,]
test_df <- std_df[-ind,]

# check overall variable importance
#filterVarImp(train_df[,3:17], train_df$postseason)

# model 1: ordinal regression w logit and probit links  ----

# while not shown here, smaller model fit better, so going with it
logit <- clm(postseason ~ adj_oe + adj_de + barthag + x2p_d,
               data = train_df, link = "logit")
probit <- clm(postseason ~ adj_oe + adj_de + barthag + x2p_d,
              data = train_df, link = "probit")

# model 2: random forest ----

## don't need to standardize :D ##
tree_df <- df[ind,]
tree_test <- df[-ind,]
rm(ind)
# initialize tuning params
rf_tune_grid <- expand.grid(mtry = seq(from = 2, to = 16, by = 2),
                            splitrule = "gini",
                            min.node.size = 10) # the default for probability
                                                # tried 1, was worse
set.seed(425440)
# train model
rf <- train(postseason ~ .,
            data = tree_df[,3:20], method = "ranger", num.trees = 150,
            trControl = ctrl, tuneGrid = rf_tune_grid)
# fit model
rf_full <- ranger(postseason ~ ., 
                  data = tree_df[,3:20],
                  num.trees = 150, probability = T, mtry = rf$bestTune$mtry,
                  splitrule = "gini", importance = "impurity")
#vip::vip(rf_full, geom = "point") + theme_bw()
rm(rf, rf_tune_grid)

# model 3: gradient boosted tree ----
## again, don't need to use standardized data ##
# initialize tuning params
xgb_tune_grid <- expand.grid(nrounds = seq(from = 20, to = 200, by = 20),
                             eta = c(0.025, 0.05, 0.1, 0.3),
                             max_depth = c(1,2,3,4),
                             gamma = 0,
                             colsample_bytree = 1,
                             min_child_weight = 1,
                             subsample = 1)
set.seed(425440)
# train model
xgb_tune <- train(x = as.matrix(tree_df[,3:19]),
                  y = tree_df$postseason,
                  trControl = ctrl,
                  tuneGrid = xgb_tune_grid,
                  objective = "multi:softprob",
                  method = "xgbTree",
                  verbosity = 0)

# actually fit the boosted tree
xgb_mod <- xgboost::xgboost(
  data = as.matrix(tree_df[,3:19]),
  label = tree_df$finish,
  objective = "multi:softprob",
  nrounds = xgb_tune$bestTune$nrounds,
# doing it the hard way bc of the num_class silliness
  params = list(
    max_depth = xgb_tune$bestTune$max_depth, 
    eta = xgb_tune$bestTune$eta, 
# the rest are constants
    gamma = 0,
    colsample_bytree = 1, min_child_weight = 1,
    subsample = 1, num_class = 8), verbose = 0
  ) # num_class was the troublemaker

rm(xgb_tune, xgb_tune_grid)

# model 4: decision tree ----
trained_tree <- train(postseason ~ .,
                      data = tree_df[,3:20], method = "rpart",
                      trControl = trainControl(method = "cv",
                                               number = 10),
                      tuneLength = 20)
#rpart.plot::rpart.plot(trained_tree$finalModel)
#vip::vip(trained_tree, geom = "point") + theme_bw()
#ggplot(trained_tree) + theme_bw()
# extract model
final_tree <- trained_tree$finalModel
rm(trained_tree)
# testing time! ----
rm(ctrl)
# train_df is standardized training data
# test_df is standardized testing data
# tree_df is nonstandardized training data
# tree_test is nonstandardized testing data

# save models and data to test in another file
save.image("C:/MarchMadness/modeling/testing/test-env.RData")