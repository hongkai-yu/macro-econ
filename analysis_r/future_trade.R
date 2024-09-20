library(tidyverse)
library(lubridate)
library(glue)
library(glmnet)
library(randomForest)
library(caret)
library(furrr)

source('r/util.R')
symbol = "UR"

df = read_csv(glue('{DATA_FOLDER}/processed/{UR}.csv'))

