# packages ----
library(tidyverse)
library(caret)
library(glmnet)
library(xgboost)
library(ranger)
library(mgcv)
library(pROC)
#libary(vip)

# data prep ----
clean_data <- read_csv("data/women/clean-data.csv")
# get just raw data
dat <- clean_data |> select(team1_win:length(clean_data))
# split into training and test! ----
set.seed(27606)
train_index <- createDataPartition(dat$team1_win, 
                                   p = 0.7, list = FALSE)
train_data <- dat[train_index, ]
test_data  <- dat[-train_index, ]

# set up train control
ctrl <- trainControl(
  method = "repeatedcv", number = 5,
  repeats = 3, classProbs = TRUE,
  summaryFunction = twoClassSummary,
  savePredictions = "final"
  )

# change from 0/1 to no/yes
train_data <- train_data |> 
  mutate(team1_win = if_else(team1_win == 1, "yes", "no"),
         team1_win = as.factor(team1_win))

# first up: logistic regression ----
step_model <- train(
  team1_win ~ ., data = train_data, method = "glmStepAIC",
  trControl = ctrl, metric = "ROC", family = binomial,
  trace = FALSE 
)
logit_model <- step_model$finalModel
rm(step_model)
# second up: penalized logistic regression (elastic net) ----
en_mod <- train(team1_win ~ ., data = train_data,
                method = "glmnet", trControl = ctrl,
                metric = "ROC", tuneLength = 10)
glmnet_mod <- en_mod$finalModel
rm(en_mod)

# third up: random forest! ----
rf_model <- train(team1_win ~ ., data = train_data, 
                  method = "ranger", trControl = ctrl, 
                  metric = "ROC", importance = "impurity")
#ggplot(rf_model) + theme_classic()
rf_mod <- rf_model$finalModel
rm(rf_model)
# fourth up: xgboost ----
xgboost_tune_grid <- expand.grid(
  nrounds = seq(from=20, to=200, by=20),
  eta = c(.025, .05, .1, .3), gamma = 0,
  max_depth = c(1,2,3,4), colsample_bytree = 1,
  min_child_weight = 1,subsample = 1
  )

# caret uses ntree_limit, which is deprecated
xgb_model <- train(
  x = as.matrix(select(train_data, - team1_win)), 
  y = train_data$team1_win, trControl = ctrl, 
  tuneGrid = xgboost_tune_grid, metric = "ROC",
  method = "xgbTree", verbose = 0)
#vip::vip(xgb_model) + theme_bw()

# fit using the best tune
xgb_fit <- xgb_model$finalModel
rm(xgb_model, xgboost_tune_grid)

# fifth up: gams! ----
# change the data back to 0/1
#train_data <- train_data |> 
#  mutate(team1_win = if_else(team1_win == "yes", 1, 0))
#
# gam_model <- gam(team1_win ~ s(MOV_game) + 
#                    s(MOV_diff) + s(OSRS_diff) +
#                    s(DSRS_diff), data = train_data, 
#                  family = "binomial", method = "REML")

# evaluation time! ----
test_data <- test_data |> 
  mutate(team1_win = if_else(team1_win == 1, "yes", "no"),
         team1_win = as.factor(team1_win))
# first, logistic regression
logit_preds <- predict(logit_model, test_data, type = "response")
roc_logit <- roc(test_data$team1_win, logit_preds) # from pROC
auc(roc_logit)
logit_classes <- if_else(logit_preds >= 0.5, "yes", "no")
confusionMatrix(as.factor(logit_classes),
               reference = test_data$team1_win,
               positive = "yes")

# next up, elastic net
en_preds <- predict(glmnet_mod, 
                    as.matrix(select(test_data, -team1_win)),
                    type = "response",
                    s = glmnet_mod$lambda[2])
roc_en <- roc(test_data$team1_win, as.numeric(en_preds))
auc(roc_en)

# third up, random forest
rf_preds <- predict(rf_mod, test_data)
rf_preds_yes <- rf_preds$predictions[,"yes"] # grab the yesses
roc_rf <- roc(test_data$team1_win, rf_preds_yes)
auc(roc_rf)
rf_classes <- if_else(rf_preds_yes >= 0.5, "yes", "no")
confusionMatrix(as.factor(rf_classes),
               reference = test_data$team1_win,
               positive = "yes")

# fourth up, xgboost
xgb_preds <- predict(xgb_fit, 
                     as.matrix(select(test_data, -team1_win)))
roc_xgb <- roc(test_data$team1_win, xgb_preds)
auc(roc_xgb)
#xgb_classes <- if_else(xgb_preds <= 0.5, "yes", "no")
#confusionMatrix(as.factor(xgb_classes), 
#                reference = test_data$team1_win, 
#                positive = "yes")

# last but certainly not least: the gam
#gam_preds <- predict(gam_model, select(test_data, -team1_win), 
#                                       type = "response")
#roc_gam <- roc(test_data$team1_win, as.numeric(gam_preds))
#auc(roc_gam)

# takeaway: probably mix the random forest and logistic regression models

# save models!
save(logit_model, rf_mod, file = "modeling/women/models.RData")
