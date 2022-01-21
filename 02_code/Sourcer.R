sourceAll <- function(){
  library(data.table)
  library(checkmate)
  library(ggplot2)
  library(readxl)
  library(xgboost)
  
  source("02_code/08_utility/oneHotEncode.R")
  source("02_code/08_utility/cleanColnames.R")
  source("02_code/08_utility/cleanNaNanInf.R")
  
  source("02_code/02_clean/clean.R")
  source("02_code/04_prepare/prepare.R")

}

