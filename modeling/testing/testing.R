# packages ----
suppressPackageStartupMessages(library(tidyverse))
library(rpart)
library(ranger)
library(ordinal)
library(xgboost)

# load env
load("C:/MarchMadness/modeling/testing/test-env.RData")
# train_df is standardized training data
# test_df is standardized testing data
# tree_df is nonstandardized training data
# tree_test is nonstandardized testing data
# logit and probit testing ----

logit_probs <- predict(object = logit,
                       newdata = test_df[,3:19], # no response --> all classes
                       type = "prob") # probabilities
logit_probs <- as.data.frame(logit_probs)

# change column names
colnames(logit_probs) <- sub("^fit\\.", "", colnames(logit_probs))

logit_preds <- predict(logit, test_df, type = "class") # classes
logit_preds <- as.data.frame(logit_preds)
# add col to dataframe
logit_probs <- cbind(logit_probs, logit_preds)

probit_probs <- predict(object = probit, 
                        newdata = test_df[,3:19], 
                        type = "prob") # probabilities
probit_probs <- as.data.frame(probit_probs)

# change column names
colnames(probit_probs) <- sub("^fit\\.", "", colnames(probit_probs))

probit_preds <- predict(probit, test_df, type = "class") # classes
probit_preds <- as.data.frame(probit_preds)
# combine dataframes
probit_probs <- cbind(probit_probs, probit_preds)
# rf and xgb predictions ----
rf_pred <- predict(rf_full, data = tree_test) # nonstandardized data
rf_probs <- as.data.frame(rf_pred$predictions)
## create a column w the classes ##
max_col_index <- max.col(rf_probs, "first")
max_col_names <- colnames(rf_probs)[max_col_index]
rf_probs$fit <- max_col_names
rf_probs$fit <- as.factor(rf_probs$fit)
rm(max_col_index, max_col_names, rf_pred)

# xgboost prediction
xgb_pred <- predict(xgb_mod, as.matrix(tree_test[,3:19]), reshape = TRUE)
xgb_pred <- as.data.frame(xgb_pred)
colnames(xgb_pred) <- levels(tree_test$postseason)
## create a column w the classes ##
max_col_index <- max.col(xgb_pred, "first")
max_col_names <- colnames(xgb_pred)[max_col_index]
xgb_pred$fit <- max_col_names
xgb_pred$fit <- as.factor(xgb_pred$fit)
rm(max_col_index, max_col_names, xgb_mod, rf_full)

# decision tree predictions ----
# class probabilities
dectree_probs <- predict(object = final_tree,
                         newdata = tree_test,
                         type = "prob")
dectree_probs <- as.data.frame(dectree_probs)
# class predictions
dectree_preds <- predict(final_tree, tree_test, type = "class")
dectree_preds <- as.data.frame(dectree_preds)
# combine
dectree_probs <- cbind(dectree_probs, dectree_preds)
dectree_probs <- dectree_probs |> rename("fit" = "dectree_preds")
# validation via accuracy ----
validation <- function(data, test = tree_test){
  # extract predictions
  preds <- data$fit # vector of class predictions
  refs <- tree_test$postseason # vector of true values
  refs <- factor(refs, ordered = FALSE) # unorder the levels
  return(caret::confusionMatrix(data = preds, reference = refs))
}

lapply(list(logit_probs, probit_probs, rf_probs, xgb_pred, dectree_probs), validation)

## conclusions ----
# accuracy of logit is 0.532, probit is 0.564, rf is 0.496, xgb is 0.52,
# decision tree is 0.512
# ensemble methods don't predict all classes; that could be an issue.
# probit is far and away the best. not worried about R68/R64. Predicted all
# champions correctly. 59% in R32/R16. Not as good with final four and runner up

# save probit model
#saveRDS(probit, file = "C:/MarchMadness/modeling/simulation/probit_model.rds")