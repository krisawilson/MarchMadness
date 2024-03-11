# packages ----
#setwd(C:\MarchMadness)
suppressPackageStartupMessages(library(tidyverse))
library(caret) # for parition, training, and cross validation
library(glmnet) # for regularized classification model
library(rpart) # for decision tree
library(ranger)

load("C:/MarchMadness/models-env.RData")