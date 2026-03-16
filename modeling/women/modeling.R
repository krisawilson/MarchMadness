# packages ----
packages <- c("tidyverse", "caret", "glmnet", "xgboost", 
              "ranger", "mgcv", "pROC", "vip", "arm")
for (pkg in packages) {
  if (!pkg %in% installed.packages()){
    install.packages(pkg, quiet = TRUE)
  }
  if (!pkg %in% .packages()) {
    suppressPackageStartupMessages(library(pkg, character.only = TRUE))
  }
}
# data prep ----
clean_data <- read_csv("data/women/clean-data.csv")
# get just raw data

# MASS is loaded in, so need to specify dplyr
dat <- clean_data |> dplyr::select(team1_win:length(clean_data))
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
test_data <- test_data |> 
  mutate(team1_win = if_else(team1_win == 1, "yes", "no"),
         team1_win = as.factor(team1_win))

# first up: logistic regression ----
logit_model <- train(
  team1_win ~ ., data = train_data, method = "glmStepAIC",
  trControl = ctrl, metric = "ROC", family = binomial,trace = FALSE
)
# second up: penalized logistic regression (elastic net) ----
en_model <- train(
  team1_win ~ ., data = train_data, method = "glmnet", 
  trControl = ctrl, metric = "ROC", tuneLength = 10
)

# third up: random forest! ----
rf_model <- train(
  team1_win ~ ., data = train_data, method = "ranger", 
  trControl = ctrl, metric = "ROC", importance = "impurity"
)
#ggplot(rf_model) + theme_classic()

# fourth up: xgboost ----

# use naive implementation to avoid compatibility issues with caret
# setup
dtrain <- xgb.DMatrix(
  data = as.matrix(dplyr::select(train_data, -team1_win)),
  label = if_else(train_data$team1_win == "yes", 1, 0)
)
dtest <- xgb.DMatrix(
  data = as.matrix(dplyr::select(test_data, -team1_win)),
  label = if_else(test_data$team1_win == "yes", 1, 0)
)
# set up tuning
xgb_grid <- expand.grid(eta = c(.025, .05, .1, .3),
                        max_depth = c(1, 2, 4, 4))
# store best results
best_auc <- 0; best_params <- list(); best_nrounds <- 0
# start tuning
cat("Tuning XGBoost...\n")
for (i in 1:nrow(xgb_grid)){
  params <- list(
    objective = "binary:logistic",
    eval_metric = "auc",
    eta = xgb_grid$eta[i],
    max_depth = xgb_grid$max_depth[i]
  )
  
  # run 5-fold cv wit early stopping
  set.seed(27606)
  cv_results <- xgb.cv(
    params = params, data = dtrain, nfold = 5,
    nrounds = 500, early_stopping_rounds = 20,
    verbose = 0, stratified = TRUE
  )
  current_best_auc <- max(cv_results$evaluation_log$test_auc_mean)
  current_optimal_nrounds <- which.max(cv_results$evaluation_log$test_auc_mean)
  
  # if best, then save
  if (current_best_auc > best_auc){
    best_auc <- current_best_auc
    best_params <- params
    best_nrounds <- current_optimal_nrounds
  }
}
cat("Best Tuning Results:\n")
cat("AUC:", best_auc, "| ETA:", best_params$eta, 
    "| Max Depth:", best_params$max_depth, 
    "| N-Rounds:", best_nrounds, "\n\n")
# fit final xgb
xgb_model <- xgb.train(
  params = best_params, data = dtrain, nrounds = best_nrounds
)

# fifth up: gams! ----
gam_model <- train(
  team1_win ~ ., data = train_data, method = "gam", trControl = ctrl,
  metric = "ROC", family = "binomial"
)

# sixth: Bayesian logistic regression via caret ----
bayes_model <- train(
  team1_win ~ ., data = train_data, method = "bayesglm",
  trControl = ctrl, metric = "ROC"
)

# evaluation time! ----

# helper for evaluation
evaluate_model <- function(model, test_data, model_name) {
  preds_prob <- predict(model, newdata = test_data, type = "prob")[,"yes"]
  preds_class <- predict(model, newdata = test_data)
  roc_obj <- roc(test_data$team1_win, preds_prob, quiet = TRUE)
  cat("\n---", model_name, "---\n")
  cat("AUC:", auc(roc_obj), "\n")
  print(confusionMatrix(preds_class, reference = test_data$team1_win, 
                        positive = "yes")$overall["Accuracy"])
}

# evaluate all caret models
evaluate_model(logit_model, test_data, "Logistic Regression")
evaluate_model(en_model, test_data, "Elastic Net")
evaluate_model(rf_model, test_data, "Random Forest")
evaluate_model(gam_model, test_data, "GAM")
evaluate_model(bayes_model, test_data, "Bayesian Logistic Regression")

# evaluate xgb
xgb_preds_prob <- predict(xgb_model, dtest)
roc_xgb <- roc(test_data$team1_win, xgb_preds_prob, quiet = TRUE)
xgb_classes <- as.factor(ifelse(xgb_preds_prob >= 0.5, "yes", "no"))

cat("\n--- Native XGBoost ---\n")
cat("AUC:", auc(roc_xgb), "\n")
print(confusionMatrix(xgb_classes, reference = test_data$team1_win, 
                      positive = "yes")$overall["Accuracy"])
# save models!
save(logit_model, en_model, rf_model, xgb_model, gam_model, 
     bayes_model, file = "modeling/women/models.RData")

# gam and rf clear here