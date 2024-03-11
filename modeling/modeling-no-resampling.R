# packages ----
#setwd(C:\MarchMadness)
suppressPackageStartupMessages(library(tidyverse))
library(caret) # for parition, training, and cross validation
library(glmnet) # for regularized classification model
library(rpart) # for decision tree
library(ranger) # random forest classification tree

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

# set a: preserve class distribution (imbalance in data) ----
set.seed(425440)
ctrl <- trainControl(method = "cv", number = 5) # set up CV
ind <- createDataPartition(df$postseason, p = 0.75, list = F) # split data
train_df <- df[ind,]
test_df <- df[-ind,]

# check overall variable importance
filterVarImp(train_df[,3:19], train_df$postseason)

# standardize data, as this improves stability of some models
std_train <- train_df |> 
  select(3:19) |> scale() |> data.frame() |> # standardization
  cbind(train_df$postseason) |> # add response
  rename(postseason = "train_df$postseason") # clean its name
rm(ind, df)

# model 1a: full ordered logistic regression ----
## while not the best model by AIC, a slight increase in AIC 
## is worth leaving some terms out the model
ord <- train(postseason ~ adj_oe + adj_de + barthag + x2p_d, 
               data = std_train, method = "polr", trControl = ctrl)
summary(ord)
# ordered regression via polr
ord_mod <- MASS::polr(postseason ~ adj_oe + adj_de + barthag + x2p_d,
                      data = std_train, method = "logistic",
                      Hess = TRUE, na.action = na.omit)
# check variable importance:
varImp(ord, scale = FALSE) # from caret
rm(ord)
# model 2a: regularized logistic regression ----
lass <- train(postseason ~ . , data = std_train, 
              method = "glmnet", family = "multinomial", trControl = ctrl)
# alpha = 0.55, lambda = 0 is the elasticnet we're going to try

# Fit the elastic net model 
lass_mod <- glmnet(x = as.matrix(std_train[, -ncol(std_train)]), 
                   y = train_df$postseason, 
                   family = "multinomial", alpha = 0.55)
rm(lass)

# model 3a: cumulative link model ----

# while not shown here, smaller model fit better, so going with it
cumul_l <- ordinal::clm(postseason ~ adj_oe + adj_de + barthag + x2p_d,
                      data = std_train, link = "logit")
cumul_p <- ordinal::clm(postseason ~ adj_oe + adj_de + barthag + x2p_d,
                        data = std_train, link = "probit")
# note that logit link is slightly better than probit 

# model 4a: random forest ----

## don't need to standardize :D ##
# initialize tuning params
rf_tune_grid <- expand.grid(mtry = seq(from = 2, to = 16, by = 2),
                            splitrule = "gini",
                            min.node.size = 1) # from 10 for probability
set.seed(425440)
# train model
rf <- train(postseason ~ .,
            data = train_df[,3:20], method = "ranger", num.trees = 150,
            trControl = ctrl, tuneGrid = rf_tune_grid)
# fit model
rf_full <- ranger(postseason ~ ., 
                  data = train_df[,3:20],
                  num.trees = 150, probability = T, mtry = rf$bestTune$mtry,
                  splitrule = "gini", importance = "impurity")
vip::vip(rf_full, geom = "point") + theme_bw()
rm(rf, rf_tune_grid)

# model 5a: gradient boosted tree ----
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
xgb_tune <- train(x = as.matrix(train_df[,3:19]),
                  y = train_df$postseason,
                  trControl = ctrl,
                  tuneGrid = xgb_tune_grid,
                  objective = "multi:softprob",
                  method = "xgbTree",
                  verbosity = 0)

# actually fit the boosted tree
xgb_mod <- xgboost::xgboost(
  data = as.matrix(train_df[,3:19]),
  label = train_df$finish,
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

# testing time! ----
rm(ctrl)
# standardize response data
std_test <- test_df |> 
  select(3:19) |> scale() |> data.frame() |> # standardization
  cbind(test_df$postseason) |> # add response
  rename(postseason = "test_df$postseason") # clean its name

# save models and data to test in another file
save.image("C:/MarchMadness/models-env.RData")