sourceAll <- function(){
  library(data.table)
  library(checkmate)
  library(ggplot2)
  library(readxl)
  library(xgboost)
  library(extrafont)
  library(png)
  library(patchwork)
  library(devtools)
  library(elementalist)
  #install_github("AppliedDataSciencePartners/xgboostExplainer")
  #library(xgboostExplainer)
  
  source("02_code/08_utility/oneHotEncode.R")
  source("02_code/08_utility/cleanColnames.R")
  source("02_code/08_utility/cleanNaNanInf.R")
  
  source("02_code/01_import/import.R")
  source("02_code/02_clean/clean.R")
  source("02_code/04_prepare/prepare.R")
  source("02_code/05_model/model.R")
  source("02_code/07_deploy/deployHelper.R")
  source("02_code/08_utility/colorSchemes.R")

}

