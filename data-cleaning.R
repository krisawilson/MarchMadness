# read in 2013-2023
suppressMessages(library(tidyverse))
cbb <- read_csv("cbb.csv", na = c("", "NA", "N/A"))
cbb <- na.omit(cbb)
# read in 2003-2012
library(hoopR)
