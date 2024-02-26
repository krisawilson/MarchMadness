# packages ----
#setwd(C:\MarchMadness)
suppressPackageStartupMessages(library(tidyverse))
library(caret) # for parition, training, and cross validation
library(glmnet) # for regularized classification model
library(xgboost) # xgboost classification tree, masks dplyr::slice
library(rpart) # for decision tree
library(ranger) # random forest classification tree
library(VGAM) # ordinal regression, masks caret::predictors

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

# set a: preserve class distribution (imbalance) ----
set.seed(425440)
ctrl <- trainControl(method = "cv", number = 5) # set up CV
ind <- createDataPartition(df$postseason, p = 0.75, list = F) # split data
train_df <- df[ind,]
test_df <- df[-ind,]

# see which candidates are the best for the model. 
### note regsubsets() is a linear regression context
summary(leaps::regsubsets(postseason ~ ., data = train_df[,3:20]))
# check overall variable importance
filterVarImp(train_df[,3:19], train_df$postseason)

# model 1a: full ordered logistic regression ----
## while not the best model by AIC, a slight increase in AIC 
## is worth leaving some terms out the model
ord <- train(postseason ~ adj_oe + adj_de + barthag + x2p_d, 
               data = train_df, method = "polr", trControl = ctrl)
summary(ord)
ord_mod <- MASS::polr(postseason ~ adj_oe + adj_de + barthag + x2p_d,
                      data = train_df, method = "logistic",
                      Hess = TRUE, na.action = na.omit)
# check variable importance:
varImp(ord, scale = FALSE) # from caret

## keep ord, as it contains the accuracy ##

# model 2a: regularized logistic regression ----
lass <- train(postseason ~ . -team -conf -seed -year -finish, 
              data = train_df, method = "glmnet",
              family = "multinomial", trControl = ctrl)
# alpha = 1 and alpha = 0.55 are almost indistinguishable. both have
## lambda = 0, so alpha = 1 would be back to regular model. try 
## elastic net w alpha = 0.55
# convert to matrix
X <- train_df |> 
  select(-c(team, conf, seed, year, finish, postseason)) |> as.matrix()
# do CV with elastic net
lass <- cv.glmnet(x = X, y = train_df$postseason, type.measure = "class", family = "multinomial")
# Fit the elastic net model 
lass_mod <- glmnet(x = X, y = train_df$postseason, 
                      family = "multinomial", alpha = 0.55)
rm(X)
## keep lass, as it contains the accuracy ##

# model 3a: cumulative link and adjacent categories models ----
## examine equal/unequal coefficients and link functions.
## parallel = TRUE means eq. coefs. logit/probit are separated by 0.001
suppressWarnings(train(postseason ~ . -team -conf -seed -year -finish, 
      data = train_df, method = "vglmCumulative", trControl = ctrl))
## warnings from from logit vs logitlink, but not something I can't fix

# fitting the models with VGAM
adj <- vglm(postseason ~ adj_oe + adj_de + barthag + x2p_d + wab,
            data = train_df, family = "acat", link = "logitlink")
# cumulative probit needs work... moving on to trees

# model 4a: random forest ----

# model 5a: graident boosted tree ----

# set 2: addressing response imbalance ----
ctrl2 <- trainControl(method = "cv", number = 5, sampling = "smote")