# packages required to run the sim!

install.packages("pak")
pkgs <- c("tidyverse", "rvest", "caret", "glmnet", 
          "xgboost","ranger", "mgcv", "pROC", "vip")
pak::pkg_install(pkg = pkgs)